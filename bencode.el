;;; bencode.el --- Bencode encoding / decoding -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(define-error 'bencode "Bencode error")
(define-error 'bencode-unsupported-type "Type cannot be encoded" 'bencode)
(define-error 'bencode-invalid-key "Dictionary key is not a string" 'bencode)
(define-error 'bencode-invalid-plist "Plist is invalid" 'bencode)

(defsubst bencode--int (object)
  (insert "i" (number-to-string object) "e"))

(defsubst bencode--string (object)
  (if (multibyte-string-p object)
      (let* ((coding-system (or coding-system-for-write 'utf-8))
             (encoded (encode-coding-string object coding-system :nocopy)))
        (insert (number-to-string (length encoded)) ":" encoded))
    (insert (number-to-string (length object)) ":" object)))

(defsubst bencode--hash-table-entries (object)
  "Return a list of key-sorted entries in OBJECT with encoded keys."
  (let ((coding (or coding-system-for-write 'utf-8))
        (entries ()))
    (maphash (lambda (key value)
               (cond
                ((multibyte-string-p key)
                 (let ((encoded (encode-coding-string key coding :nocopy)))
                   (push (cons encoded value) entries)))
                ((stringp key)
                 (push (cons key value) entries))
                ((signal 'bencode-invalid-key key))))
             object)
    (cl-sort entries #'string< :key #'car)))

(defsubst bencode--plist-entries (object)
  "Return a list of key-sorted entries in OBJECT with encoded keys."
  (let ((coding (or coding-system-for-write 'utf-8))
        (plist object)
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
              (let ((encoded (encode-coding-string name coding :nocopy)))
                (push (cons encoded value) entries))
            (push (cons name value) entries)))))
    (cl-sort entries #'string< :key #'car)))

(defun bencode (object)
  "Return a unibyte string encoding OBJECT with bencode.

Supported types:
* Integer
* Multibyte and unibyte strings
* List of supported types
* Vector of supproted types (encodes to list)
* Hash table with string keys (encodes to dictionary)
* Plist with keyword symbol keys (encodes to dictionary)

When multibyte strings are encountered either as values or
dictionary keys, they are encoded with the coding system
indicated by `coding-system-for-write', or UTF-8 when nil. The
same coding system must be used when decoding.

Possible signals:
* bencode-unsupported-type
* bencode-invalid-key
* bencode-invalid-plist

This function is not recursive. It is safe to use on deeply
nested data structures."
  (let ((stack (list (cons :new object))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (while stack
        (let* ((next (pop stack))
               (value (cdr next)))
          (cl-case (car next)
            (:new
             (cond ((integerp value)
                    (bencode--int value))
                   ((stringp value)
                    (bencode--string value))
                   ((and (listp value)
                         (keywordp (car value)))
                    (insert "d")
                    (let ((entries (bencode--plist-entries value)))
                      (push (cons :dict entries) stack)))
                   ((listp value)
                    (insert "l")
                    (push (cons :list value) stack))
                   ((vectorp value)
                    (insert "l")
                    (push (cons :vector (cons 0 value)) stack))
                   ((hash-table-p value)
                    (insert "d")
                    (let ((entries (bencode--hash-table-entries value)))
                      (push (cons :dict entries) stack)))
                   ((signal 'bencode-unsupported-type object))))
            (:dict
             (if (null value)
                 (insert "e")
               (let ((entry (car value)))
                 (bencode--string (car entry))
                 (push (cons :dict (cdr value)) stack)
                 (push (cons :new (cdr entry)) stack))))
            (:list
             (if (null value)
                 (insert "e")
               (push (cons :list (cdr value)) stack)
               (push (cons :new (car value)) stack)))
            (:vector
             (let ((i (car value))
                   (v (cdr value)))
               (if (= i (length v))
                   (insert "e")
                 (push (cons :vector (cons (+ i 1) v)) stack)
                 (push (cons :new (aref v i)) stack)))))))
      (buffer-string))))

(provide 'bencode)

;;; bencode.el ends here
