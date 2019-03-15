;;; bencode-test.el --- tests for bencode.el -*- lexical-binding: t; -*-

;;; Code

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
  (should-error (bencode 0.0)
                :type 'bencode-unsupported-type)
  (should-error (bencode 'foo)
                :type 'bencode-unsupported-type)
  (should-error (bencode (symbol-function 'bencode))
                :type 'bencode-unsupported-type)
  (should-error (bencode (make-bool-vector 16 nil))
                :type 'bencode-unsupported-type))

(ert-deftest bencode-integer ()
  (let ((positive (bencode 42))
        (negative (bencode -3000))
        (zero (bencode 0)))
    (should (not (multibyte-string-p positive)))
    (should (not (multibyte-string-p negative)))
    (should (not (multibyte-string-p zero)))
    (should (equal positive "i42e"))
    (should (equal negative "i-3000e"))
    (should (equal zero "i0e"))))

(ert-deftest bencode-string ()
  (let ((simple (bencode "hello")))
    (should (not (multibyte-string-p simple)))
    (should (equal simple "5:hello")))
  (let ((utf8 (bencode "π = 3.14")))
    (should (not (multibyte-string-p utf8)))
    (should (equal utf8 "9:\xcf\x80 = 3.14")))
  (let* ((utf-8 (bencode "naïvety"))
         (coding-system-for-write 'latin-1)
         (latin-1 (bencode "naïvety")))
    (should (not (multibyte-string-p utf-8)))
    (should (equal utf-8 "8:na\xc3\xafvety"))
    (should (not (multibyte-string-p latin-1)))
    (should (equal latin-1 "7:na\xefvety"))))

(ert-deftest bencode-list ()
  (let ((empty-list (bencode '()))
        (empty-vector (bencode [])))
    (should (not (multibyte-string-p empty-list)))
    (should (not (multibyte-string-p empty-vector)))
    (should (equal empty-list "le"))
    (should (equal empty-vector "le")))
  (let ((numbers (bencode [1 2 3])))
    (should (not (multibyte-string-p numbers)))
    (should (equal numbers "li1ei2ei3ee")))
  (let ((nested (bencode [()])))
    (should (not (multibyte-string-p nested)))
    (should (equal nested "llee"))))

(ert-deftest bencode-dict ()
  (let ((empty (bencode (make-hash-table :test 'equal))))
    (should (not (multibyte-string-p empty)))
    (should (equal empty "de")))
  ;; Test key validation
  (should-error (bencode #s(hash-table test equal data (0 0)))
                :type 'bencode-invalid-key)
  (should-error (bencode #s(hash-table test equal data ([1 2 3] 0)))
                :type 'bencode-invalid-key)
  ;; Test key encoding
  (let* ((table #s(hash-table test equal data ("α" 0 "β" 1 "ω" 2)))
         (utf-8 (bencode table))
         (coding-system-for-write 'iso-8859-7)
         (greek (bencode table)))
    (should (not (multibyte-string-p utf-8)))
    (should (equal utf-8 "d2:\xce\xb1i0e2:\xce\xb2i1e2:\xcf\x89i2ee"))
    (should (not (multibyte-string-p greek)))
    (should (equal greek "d1:\xe1i0e1:\xe2i1e1:\xf9i2ee")))
  ;; Test that keys are sorted
  (let ((table (make-hash-table :test 'equal))
        (keys (cl-loop with state = (bencode--random-state 0)
                       for i from 0 below (lsh 1 12)
                       collect (format "%04x" i) into values
                       finally return (bencode--shuffle values state))))
    (dolist (key keys)
      (setf (gethash key table) 0))
    (let ((encoded (bencode table)))
      (should (not (multibyte-string-p encoded)))
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

(defun bencode-benchmark ()
  (let ((data (bencode--gen-dict (bencode--random-state 493) 0)))
    (print
     (benchmark-run 10
       (bencode data)))))

;;; bencode-test.el ends here
