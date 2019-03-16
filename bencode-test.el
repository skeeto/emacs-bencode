;;; bencode-test.el --- tests for bencode.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for the bencode package.

;;; Code:

(require 'bencode)
(require 'ert)

(defun bencode--random-state (seed)
  (record 'bencode--random-state (float seed)))

(defun bencode--random (n state)
  "A PRNG with identical behavior between 32-bit and 64-bit Emacs."
  ;; Park-Miller LCG
  (let* ((x (aref state 1))
         (hi (/   x 127773.0))
         (lo (mod x 127773.0))
         (mul (- (* lo 16807.0) (* hi 2836.0)))
         (nx (if (> mul 0.0) mul (+ mul 2147483647.0))))
    (setf (aref state 1) nx)
    (if (floatp n)
        (* n (/ nx 2147483648.0))
      (floor (mod nx n)))))

(defun bencode--shuffle (values random-state)
  "Return a shuffled copy of VALUES."
  (let ((v (vconcat values)))
    (cl-loop for i from (1- (length v)) downto 1
             for j = (bencode--random (+ i 1) random-state)
             do (cl-rotatef (aref v i) (aref v j))
             finally return (append v nil))))

(defun bencode--gen-integer (s)
  (- (bencode--random 2000000 s) 1000000))

(defun bencode--gen-char (s)
  ;;(+ 32 (bencode--random (- (* 2 256) 32) s))
  (let ((set "π0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (aref set (bencode--random (length set) s))))

(defun bencode--gen-string (s)
  (let* ((min 2)
         (max 32)
         (length (+ min (bencode--random (- max min) s)))
         (string (make-string length 0)))
    (prog1 string
      (dotimes (i length)
        (setf (aref string i) (bencode--gen-char s))))))

(defun bencode--gen-list (s depth)
  (let* ((min 0)
         (max 16)
         (length (+ min (bencode--random (- max min) s)))
         (list (make-vector length nil)))
    (prog1 list
      (dotimes (i length)
        (setf (aref list i) (bencode--gen s depth))))))

(defun bencode--gen-dict (s depth)
  (let ((dict (make-hash-table :test 'equal))
        (min 4)
        (max 100))
    (prog1 dict
      (dotimes (_ (+ min (bencode--random (- max min) s)))
        (let ((key (bencode--gen-string s))
              (value (bencode--gen s depth)))
          (setf (gethash key dict) value))))))

(defun bencode--gen (s depth)
  (let ((draw (* 4.0 (expt (bencode--random 1.0 s) (+ 1.0 (* depth 4.0))))))
    (cl-case (floor draw)
      (0 (bencode--gen-integer s))
      (1 (bencode--gen-string s))
      (2 (bencode--gen-list s (+ 1 depth)))
      (3 (bencode--gen-dict s (+ 1 depth))))))

(ert-deftest bencode-unsupported ()
  (should-error (bencode-encode 0.0)
                :type 'bencode-unsupported-type)
  (should-error (bencode-encode 'foo)
                :type 'bencode-unsupported-type)
  (should-error (bencode-encode (symbol-function 'bencode-encode))
                :type 'bencode-unsupported-type)
  (should-error (bencode-encode (make-bool-vector 16 nil))
                :type 'bencode-unsupported-type))

(ert-deftest bencode-integer ()
  (let ((positive (bencode-encode 42))
        (negative (bencode-encode -3000))
        (zero (bencode-encode 0)))
    (should (not (multibyte-string-p positive)))
    (should (not (multibyte-string-p negative)))
    (should (not (multibyte-string-p zero)))
    (should (equal positive "i42e"))
    (should (equal negative "i-3000e"))
    (should (equal zero "i0e"))))

(ert-deftest bencode-string ()
  (let ((simple (bencode-encode "hello")))
    (should (not (multibyte-string-p simple)))
    (should (equal simple "5:hello")))
  (let ((utf8 (bencode-encode "π = 3.14")))
    (should (not (multibyte-string-p utf8)))
    (should (equal utf8 "9:\xcf\x80 = 3.14")))
  (let* ((utf-8 (bencode-encode "naïvety"))
         (coding-system-for-write 'latin-1)
         (latin-1 (bencode-encode "naïvety")))
    (should (not (multibyte-string-p utf-8)))
    (should (equal utf-8 "8:na\xc3\xafvety"))
    (should (not (multibyte-string-p latin-1)))
    (should (equal latin-1 "7:na\xefvety"))))

(ert-deftest bencode-list ()
  (let ((empty-list (bencode-encode '()))
        (empty-vector (bencode-encode [])))
    (should (not (multibyte-string-p empty-list)))
    (should (not (multibyte-string-p empty-vector)))
    (should (equal empty-list "le"))
    (should (equal empty-vector "le")))
  (let ((numbers (bencode-encode [1 2 3])))
    (should (not (multibyte-string-p numbers)))
    (should (equal numbers "li1ei2ei3ee")))
  (let ((nested (bencode-encode [()])))
    (should (not (multibyte-string-p nested)))
    (should (equal nested "llee"))))

(defun bencode--hash-to-plist (table)
  (let ((plist ()))
    (maphash (lambda (key value)
               (push value plist)
               (push (intern (concat ":" key)) plist))
             table)
    plist))

(ert-deftest bencode-dict ()
  (let ((empty (bencode-encode (make-hash-table :test 'equal))))
    (should (not (multibyte-string-p empty)))
    (should (equal empty "de")))
  ;; Test key validation
  (should-error (bencode-encode #s(hash-table test equal data (0 0)))
                :type 'bencode-invalid-key)
  (should-error (bencode-encode #s(hash-table test equal data ([1 2 3] 0)))
                :type 'bencode-invalid-key)
  ;; Test key encoding
  (let* ((table #s(hash-table test equal data ("α" 0 "β" 1 "ω" 2)))
         (plist (bencode--hash-to-plist table))
         (utf-8 (bencode-encode table))
         (utf-8-plist (bencode-encode plist))
         (coding-system-for-write 'iso-8859-7)
         (greek (bencode-encode table))
         (greek-plist (bencode-encode plist)))
    (should (not (multibyte-string-p utf-8)))
    (should (equal utf-8 "d2:\xce\xb1i0e2:\xce\xb2i1e2:\xcf\x89i2ee"))
    (should (equal utf-8-plist utf-8))
    (should (not (multibyte-string-p greek)))
    (should (equal greek "d1:\xe1i0e1:\xe2i1e1:\xf9i2ee"))
    (should (equal greek-plist greek)))
  ;; Check for invalid plists
  (should-error (bencode-encode '(:foo 0 :bar))
                :type 'bencode-invalid-plist)
  (should-error (bencode-encode '(:foo 0 bar 2))
                :type 'bencode-invalid-key)
  ;; Test that keys are sorted
  (let ((table (make-hash-table :test 'equal))
        (keys (cl-loop with state = (bencode--random-state 0)
                       for i from 0 below (lsh 1 12)
                       collect (format "%04x" i) into values
                       finally return (bencode--shuffle values state))))
    (dolist (key keys)
      (setf (gethash key table) 0))
    (let ((encoded (bencode-encode table))
          (encoded-plist (bencode-encode (bencode--hash-to-plist table))))
      (should (not (multibyte-string-p encoded)))
      (should (equal encoded encoded-plist))
      (should (eql (aref encoded 0) ?d))
      (should (eql (aref encoded (- (length encoded) 1)) ?e))
      (let ((last ()))
        (dotimes (i (/ (length encoded) 9))
          (let* ((beg (+ 1 (* i 9)))
                 (end (+ 7 (* i 9)))
                 (key (substring encoded beg end)))
            (when last
              (should (string> key last)))
            (setf last key)))))))

(ert-deftest bencode-decode-integer ()
  ;; Invalid inputs
  (should-error (bencode-decode-string "i1")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "i1f")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string "i01e")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string "i-0e")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string "ie")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string "i-e")
                :type 'bencode-invalid-byte)
  (when (or (floatp (+ 1 most-positive-fixnum))
            (< (+ 1 most-positive-fixnum) 0))
    ;; Only test overflow when bigint is not supported
    (let ((overflow (format "i%d0e" most-positive-fixnum)))
      (should-error (bencode-decode-string overflow)
                    :type 'bencode-overflow)))
  ;; Valid inputs
  (should (eql (bencode-decode-string "i0e") 0))
  (should (eql (bencode-decode-string "i1e") 1))
  (should (eql (bencode-decode-string "i-1e") -1))
  (should (eql (bencode-decode-string "i1000e") 1000))
  (should (eql (bencode-decode-string "i-123456e") -123456)))

(ert-deftest bencode-decode-string ()
  ;; Invalid inputs
  (should-error (bencode-decode-string "1")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "5:hi")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "1x")
                :type 'bencode-invalid-byte)
  ;; Valid inputs
  (should (equal (bencode-decode-string "5:hello") "hello"))
  (should (equal (bencode-decode-string "0:") ""))
  (should (equal (bencode-decode-string "2:\xcf\x80") "π"))
  (should (equal (bencode-decode-string "8:na\xc3\xafvety") "naïvety"))
  (let ((coding-system-for-read 'latin-1))
    (should (equal (bencode-decode-string "7:na\xefvety") "naïvety"))))

(ert-deftest bencode-decode-list ()
  ;; Invalid inputs
  (should-error (bencode-decode-string "l")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "li0e")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "li10e")
                :type 'bencode-end-of-file)
  ;; Valid inputs
  (should (equal (bencode-decode-string "li0ei1ei2ee")
                 '(0 1 2)))
  (should (equal (bencode-decode-string "le")
                 '()))
  (should (equal (bencode-decode-string "llleee")
                 '((()))))
  (should (equal (bencode-decode-string "li1ell0:eei2ee")
                 '(1 (("")) 2))))


(ert-deftest bencode-decode-dict ()
  ;; Invalid inputs
  (should-error (bencode-decode-string "d")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "d1:x")
                :type 'bencode-end-of-file)
  (should-error (bencode-decode-string "di0ei0ee")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string "d1:x0:1:a0:e")
                :type 'bencode-invalid-key)
  (should-error (bencode-decode-string "d1:x0:1:x0:e")
                :type 'bencode-invalid-key)
  (should-error (bencode-decode-string "d1:a0:2:bbe")
                :type 'bencode-invalid-byte)
  ;; Valid inputs
  (should (equal (bencode-decode-string "de")
                 '()))
  (should (equal (bencode-decode-string "d0:i0e1:ai1ee")
                 '(: 0 :a 1)))
  (should (equal (bencode-decode-string "d1:a0:2:bb0:e")
                 '(:a "" :bb "")))
  (should (equal (bencode-decode-string "d1:ad1:ai0eee")
                 '(:a (:a 0)))))

(ert-deftest bencode-decode ()
  (should-error (bencode-decode-string "i0e ")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string " i0e")
                :type 'bencode-invalid-byte)
  (should-error (bencode-decode-string "")
                :type 'bencode-end-of-file)
  (let* ((bencode-dictionary-type :hash-table)
         (table (bencode-decode-string "d3:aaai0e3:bbbi1ee")))
    (should (eql (gethash "aaa" table) 0))
    (should (eql (gethash "bbb" table) 1)))
  (let ((bencode-list-type :vector))
    (should (equal (bencode-decode-string "li0ei1ei2ee") [0 1 2])))
  (with-temp-buffer
    ;; Problem: This won't blow the stack when parsing, but it could
    ;; when checking the result with `equal'.
    (let ((depth 100)
          (output 0))
      (dotimes (_ depth)
        (insert "l"))
      (insert "i0e")
      (dotimes (_ depth)
        (insert "e"))
      (dotimes (_ depth)
        (setf output (list output)))
      (setf (point) (point-min))
      (should (equal (bencode-decode) output)))))

(defun bencode-benchmark ()
  (let ((data (bencode--gen-dict (bencode--random-state 493) 0)))
    (princ (format "encode: %S\n"
                   (benchmark-run 10
                     (bencode-encode data))))
    (with-temp-buffer
      (insert (bencode-encode data))
      (princ (format "decode: %S\n"
                     (benchmark-run 10
                       (setf (point) (point-min))
                       (bencode-decode)))))))

(provide 'bencode-test)

;;; bencode-test.el ends here
