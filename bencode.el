;;; bencode.el --- Bencode encoding / decoding -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacs-bencode
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This is a strict and robust bencode encoder and decoder. Encoding
;; is done precisely, carefully taking into account character encoding
;; issues. As such, the encoder always returns unibyte data. When
;; encoding strings and keys, UTF-8 is used by default for both
;; encoding and decoding, but this can be configured.

;; The API entrypoints are:
;; * `bencode-encode'
;; * `bencode-encode-to-buffer'
;; * `bencode-decode'
;; * `bencode-decode-from-buffer'

;;; Code:

(require 'cl-lib)

(define-error 'bencode "Bencode error")
(define-error 'bencode-unsupported-type "Type cannot be encoded" 'bencode)
(define-error 'bencode-invalid-key "Not a valid dictionary key" 'bencode)
(define-error 'bencode-invalid-plist "Plist is invalid" 'bencode)
(define-error 'bencode-invalid-byte "Invalid input byte" 'bencode)
(define-error 'bencode-overflow "Integer too large" 'bencode)
(define-error 'bencode-end-of-file "End of file during parsing"
  '(bencode end-of-file))

(defsubst bencode--int (object)
  "Encode OBJECT as an integer into the current buffer."
  (insert "i" (number-to-string object) "e"))

(defsubst bencode--string (object coding-system)
  "Encode OBJECT as a string into the current buffer."
  (if (multibyte-string-p object)
      (let ((encoded (encode-coding-string object coding-system :nocopy)))
        (insert (number-to-string (length encoded)) ":" encoded))
    (insert (number-to-string (length object)) ":" object)))

(defsubst bencode--hash-table-entries (object coding-system)
  "Return a list of key-sorted entries in OBJECT with encoded keys."
  (let ((entries ()))
    (maphash (lambda (key value)
               (cond
                ((multibyte-string-p key)
                 (let ((encoded (encode-coding-string
                                 key coding-system :nocopy)))
                   (push (cons encoded value) entries)))
                ((stringp key)
                 (push (cons key value) entries))
                ((signal 'bencode-invalid-key key))))
             object)
    (cl-sort entries #'string< :key #'car)))

(defsubst bencode--plist-entries (object coding-system)
  "Return a list of key-sorted entries in OBJECT with encoded keys."
  (let ((plist object)
        (entries ()))
    (while plist
      (let ((key (pop plist)))
        (unless (keywordp key)
          (signal 'bencode-invalid-key key))
        (when (null plist)
          (signal 'bencode-invalid-plist object))
        (let ((name (substring (symbol-name key) 1))
              (value (pop plist)))
          (if (multibyte-string-p name)
              (let ((encoded (encode-coding-string
                              name coding-system :nocopy)))
                (push (cons encoded value) entries))
            (push (cons name value) entries)))))
    (cl-sort entries #'string< :key #'car)))

(cl-defun bencode-encode (object &key (coding-system 'utf-8))
  "Return a unibyte string encoding OBJECT with bencode.

:coding-system -- coding system for encoding strings into byte strings (utf-8)

Supported types:
* Integer
* Multibyte and unibyte strings
* List of supported types
* Vector of supproted types (encodes to list)
* Hash table with string keys (encodes to dictionary)
* Plist with keyword symbol keys (encodes to dictionary)

When multibyte strings are encountered either as values or dictionary
keys, they are encoded with the specified coding system (default:
UTF-8). The same coding system must be used when decoding.

Possible error signals:
* bencode-unsupported-type
* bencode-invalid-key
* bencode-invalid-plist

This function is not recursive. It is safe to input very deeply
nested data structures."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (bencode-encode-to-buffer object :coding-system coding-system)
    (buffer-string)))

(cl-defun bencode-encode-to-buffer (object &key (coding-system 'utf-8))
  "Like `bencode-encode' but to the current buffer at point."
  (let ((stack (list (cons :new object))))
    (while stack
      (let* ((next (pop stack))
             (value (cdr next)))
        (cl-case (car next)
          ;; Start encoding a new, unexamined value
          (:new
           (cond ((integerp value)
                  (bencode--int value))
                 ((stringp value)
                  (bencode--string value coding-system))
                 ((and (consp value)
                       (keywordp (car value)))
                  (insert "d")
                  (let ((entries (bencode--plist-entries value coding-system)))
                    (push (cons :dict entries) stack)))
                 ((listp value)
                  (insert "l")
                  (push (cons :list value) stack))
                 ((vectorp value)
                  (insert "l")
                  (push (cons :vector (cons 0 value)) stack))
                 ((hash-table-p value)
                  (insert "d")
                  (let ((entries (bencode--hash-table-entries
                                  value coding-system)))
                    (push (cons :dict entries) stack)))
                 ((signal 'bencode-unsupported-type object))))
          ;; Continue encoding dictionary
          ;; (:dict . remaining-dict)
          (:dict
           (if (null value)
               (insert "e")
             (let ((entry (car value)))
               (bencode--string (car entry))
               (push (cons :dict (cdr value)) stack)
               (push (cons :new (cdr entry)) stack))))
          ;; Continue encoding list
          ;; (:list . remaining-list)
          (:list
           (if (null value)
               (insert "e")
             (push (cons :list (cdr value)) stack)
             (push (cons :new (car value)) stack)))
          ;; Continue encoding vector (as list)
          ;; (:vector index . vector)
          (:vector
           (let ((i (car value))
                 (v (cdr value)))
             (if (= i (length v))
                 (insert "e")
               (push (cons :vector (cons (+ i 1) v)) stack)
               (push (cons :new (aref v i)) stack)))))))))

(defsubst bencode--decode-int ()
  "Decode an integer from the current buffer at point."
  (forward-char)
  (let ((start (point)))
    ;; Don't allow leading zeros
    (if (eql (char-after) ?0)
        ;; Unless the value *is* zero
        (prog1 0
          (forward-char)
          (unless (eql (char-after) ?e)
            (signal 'bencode-invalid-byte
                    (cons (char-after) (point))))
          (forward-char))
      ;; Skip minus sign
      (when (eql (char-after) ?-)
        (forward-char)
        ;; Negative zero not allowed
        (when (eql (char-after) ?0)
          (signal 'bencode-invalid-byte
                  (cons (char-after) (point)))))
      ;; Check for empty integer
      (when (eql ?e (char-after))
        (signal 'bencode-invalid-byte
                (cons (char-after) (point))))
      ;; Skip over digits
      (unless (re-search-forward "[^0-9]" nil :noerror)
        (signal 'bencode-end-of-file (point)))
      ;; Check for terminator
      (unless (eql ?e (char-before))
        (signal 'bencode-invalid-byte
                (cons (char-before) (point))))
      ;; Try to parse the digits
      (let* ((string (buffer-substring start (point)))
             (result (string-to-number string)))
        (if (floatp result)
            (signal 'bencode-overflow (cons string result))
          result)))))

(defsubst bencode--decode-string (coding-system)
  "Decode a string from the current buffer at point.

Returns cons of (raw . decoded)."
  (let ((start (point)))
    ;; Skip over length digits
    (unless (re-search-forward "[^0-9]" nil :noerror)
     (signal 'bencode-end-of-file (point)))
    ;; Did we find a colon?
    (unless (eql ?: (char-before))
      (signal 'bencode-invalid-byte
              (cons (char-before) (point))))
    (let* ((length-string (buffer-substring start (- (point) 1)))
           (length (string-to-number length-string)))
      (when (floatp length)
        (signal 'bencode-overflow
                (cons length-string length)))
      (when (> (+ (point) length) (point-max))
        (signal 'bencode-end-of-file (+ (point) length)))
      (let ((string (buffer-substring (point) (+ (point) length))))
        (prog1 (cons string
                     (decode-coding-string string coding-system :nocopy))
          (forward-char length))))))

(defsubst bencode--to-plist (list)
  "Convert a series of parsed dictionary entries into a plist."
  (let ((plist ()))
    (while list
      (push (pop list) plist)
      (push (intern (concat ":" (pop list))) plist))
    plist))

(defsubst bencode--to-hash-table (list)
  "Convert a series of parsed dictionary entries into a hash table."
  (let ((table (make-hash-table :test 'equal)))
    (prog1 table
      (while list
        (let ((value (pop list))
              (key (pop list)))
          (setf (gethash key table) value))))))

(cl-defun bencode-decode-from-buffer
    (&key (list-type 'list) (dict-type 'plist) (coding-system 'utf-8))
  "Like `bencode-decode' but from the current buffer starting at point.

The point is left where parsing finished. You may want to reject
inputs with data trailing beyond the point."
  ;; Operations are pushed onto an operation stack. One operation is
  ;; executed once per iteration. Some operations push multiple new
  ;; operations onto the stack. When no more operations are left,
  ;; return the remaining element from the value stack.
  (let ((op-stack '(:read))    ; operations stack
        (value-stack ())       ; stack of parsed values
        (last-key-stack ()))   ; last key seen in top dictionary
    (while op-stack
      (cl-case (pop op-stack)
        ;; Figure out what type of value is to be read next and
        ;; prepare stacks accordingly.
        (:read
         (cl-case (char-after)
           ((nil) (signal 'bencode-end-of-file (point)))
           (?e (signal 'bencode-invalid-byte (point)))
           (?i (push (bencode--decode-int) value-stack))
           (?l (forward-char)
               (push :list op-stack)
               (push nil value-stack))
           (?d (forward-char)
               (push :dict op-stack)
               (push nil value-stack)
               (push nil last-key-stack))
           ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
            (push (cdr (bencode--decode-string coding-system)) value-stack))))
        ;; Push value at top of value stack onto list just below it
        (:append
         (push (pop value-stack) (car value-stack)))
        ;; Read a key and push onto the value stack
        (:key
         (let* ((string (bencode--decode-string coding-system))
                (raw (car string))
                (key (cdr string))
                (last-key (car last-key-stack)))
           (when last-key
             (when (string= last-key raw)
               (signal 'bencode-invalid-key (cons 'duplicate key)))
             (when (string< raw last-key)
               (signal 'bencode-invalid-key (list 'string> last-key raw))))
           (setf (car last-key-stack) raw)
           (push key value-stack)))
        ;; End list, or queue operations to read another value
        (:list
         (if (eql (char-after) ?e)
             (let ((result (nreverse (car value-stack))))
               (forward-char)
               (if (eq list-type 'vector)
                   (setf (car value-stack) (vconcat result))
                 (setf (car value-stack) result)))
           (let ((ops (list :read :append :list)))
             (setf op-stack (nconc ops op-stack)))))
        ;; End dict, or queue operations to read another entry
        (:dict
         (if (eql (char-after) ?e)
             (let ((result (car value-stack)))
               (forward-char)
               (pop last-key-stack)
               (if (eq dict-type 'hash-table)
                   (setf (car value-stack) (bencode--to-hash-table result))
                 (setf (car value-stack) (bencode--to-plist result))))
           (let ((ops (list :key :append :read :append :dict)))
             (setf op-stack (nconc ops op-stack)))))))
    (car value-stack)))

(cl-defun bencode-decode
    (string &key (list-type 'list) (dict-type 'plist) (coding-system 'utf-8))
  "Decode bencode data from STRING.

:coding-system -- coding system for decoding byte strings (utf-8)
:dict-type -- target format for dictionaries (symbol: plist, hash-table)
:list-type -- target format for lists (symbol: list, vector)

Input should generally be unibyte. Strings parsed as values and
keys will be decoded using the coding system indicated by the
given coding system (default: UTF-8). The same coding system
should be used as when encoding. There are never decoding errors
since Emacs can preserve arbitrary byte data across encoding and
decoding. See \"Text Representations\" in the Gnu Emacs Lisp
Reference Manual.

Input is strictly validated and invalid inputs are rejected. This
includes dictionary key constraints. Dictionaries are decoded
into plists. Lists are decoded into lists. If an integer is too
large to store in an Emacs integer, the decoder will signal an
overlow error. Signals an error if STRING contains trailing data.

Possible error signals:
* bencode-end-of-file
* bencode-invalid-key
* bencode-invalid-byte
* bencode-overflow

This function is not recursive. It is safe to parse very deeply
nested inputs."
  (with-temp-buffer
    (insert string)
    (setf (point) (point-min))
    (prog1 (bencode-decode-from-buffer :list-type list-type
                                       :dict-type dict-type
                                       :coding-system coding-system)
      (when (< (point) (point-max))
        (signal 'bencode-invalid-byte (cons "Trailing data" (point)))))))

(provide 'bencode)

;;; bencode.el ends here
