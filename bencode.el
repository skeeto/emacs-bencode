;;; bencode.el --- Bencode encoding / decoding -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(define-error 'bencode "Bencode error")
(define-error 'bencode-unsupported-type "Type cannot be encoded" 'bencode)
(define-error 'bencode-bad-key "Dictionary key is not a string" 'bencode)

(defun bencode (object)
  "Return a unibyte string encoding OBJECT with bencode.

When multibyte strings are encountered either as values or
dictionary keys, they are encoded with the coding system
indicated by `coding-system-for-write', or UTF-8 when nil. The
same coding system must be used when decoding."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (bencode--internal object)
    (buffer-string)))

(defun bencode--internal (object)
  (cond
    ;; i...e
    ((integerp object)
     (insert "i" (number-to-string object) "e"))
    ;; <length>:<encoded-string>
    ((multibyte-string-p object)
     (let* ((coding-system (or coding-system-for-write 'utf-8))
            (encoded (encode-coding-string object coding-system :nocopy)))
       (insert (number-to-string (length encoded)) ":" encoded)))
    ;; <length>:<string>
    ((stringp object)
     (insert (number-to-string (length object)) ":" object))
    ;; l...e
    ((listp object)
     (insert "l")
     (dolist (element object)
       (bencode--internal element))
     (insert "e"))
    ;; l...e
    ((vectorp object)
     (insert "l")
     (dotimes (i (length object))
       (bencode--internal (aref object i)))
     (insert "e"))
    ;; d...e
    ((hash-table-p object)
     (let ((coding-system (or coding-system-for-write 'utf-8))
           (entries ()))
       (maphash (lambda (key value) (push (cons key value) entries)) object)
       (dolist (entry entries)
         (let ((key (car entry)))
           (cond ((multibyte-string-p key)
                  (setf (car entry)
                        (encode-coding-string key coding-system :nocopy)))
                 ((not (stringp key))
                  (signal 'bencode-bad-key key)))))
       (insert "d")
       (dolist (entry (cl-sort entries #'string< :key #'car))
         (bencode--internal (car entry))
         (bencode--internal (cdr entry)))
       (insert "e")))
    ((signal 'bencode-unsupported-type object))))

(provide 'bencode)

;;; bencode.el ends here
