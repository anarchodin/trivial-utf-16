(in-package :trivial-utf-16)

(defconstant +native-utf-16+
  (if (= char-code-limit #x10000) 't nil)
  "Whether or not the implementation uses UTF-16 natively.")

(deftype high-surrogate ()
  "A Unicode High Surrogate."
  '(integer #xD800 #xDBFF))

(deftype low-surrogate ()
  "A Unicode Low Surrogate."
  '(integer #xDC00 #xDFFF))

(deftype unicode-point ()
  "A Unicode code point."
  '(integer 0 #x10FFFF))

(deftype unicode-string ()
  "A vector of Unicode code points."
  '(vector unicode-point))

(defun codepoint-as-utf-16 (codepoint)
  "Translate a Unicode code point to its UTF-16 representation. Returns a list of one or two codepoints. Passes surrogate code points straight through."
  (check-type codepoint unicode-point "a Unicode code point")
  (if (< codepoint #x10000)
      (list codepoint) ; If it's BMP, just return it.
      (let ((twentybits (- codepoint #x10000)))
        (list (dpb #b110110 (byte 6 10) (ldb (byte 10 10) twentybits))
              (dpb #b110111 (byte 6 10) (ldb (byte 10 0) twentybits))))))

(defun surrogates-to-codepoint (high-surrogate low-surrogate)
  "Translate a pair of surrogate codepoints to a non-BMP codepoint. Returns the codepoint as an integer."
  (check-type high-surrogate high-surrogate "a Unicode high-surrogate")
  (check-type low-surrogate low-surrogate "a Unicode low-surrogate")
  (let ((low-bits (ldb (byte 10 0) low-surrogate))
        (high-bits (ldb (byte 10 0) high-surrogate)))
    (+ #x10000 (dpb high-bits (byte 10 10) low-bits))))

(defun encode-utf-16 (unicode-string)
  "Turn a vector of Unicode code points into a vector of UTF-16 code units. Indifferent to unpaired surrogates."
  (coerce
   (mapcan #'codepoint-as-utf-16 (coerce unicode-string 'list))
   '(vector (unsigned-byte 16))))

(defun decode-utf-16 (utf-16-string)
  "Turn a vector of UTF-16 code units into a vector of Unicode code points. Passes unpaired surrogate codepoints straight through."
  (let ((result '()))
    (dotimes (i (length utf-16-string))
      (let ((codepoint (elt utf-16-string i)))
        ;; The first branch converts high-surrogate followed by low-surrogate.
        ;; The second branch ignores low-surrogate preceded by high-surrogate.
        ;; FIXME: I'm sure this can be done better.
        (cond ((and (typep codepoint 'high-surrogate)
                    (< i (1- (length utf-16-string)))
                    (typep (elt utf-16-string (1+ i)) 'low-surrogate))
               (push (surrogates-to-codepoint codepoint (elt utf-16-string (1+ i))) result))
              ((and (typep codepoint 'low-surrogate)
                    (> i 0)
                    (typep (elt utf-16-string (1- i)) 'high-surrogate)))
              (t (push codepoint result)))))
    (nreverse result)))

(defun from-unicode-string (unicode-string)
  "Take a vector of Unicode code points and turn it into a Lisp string."
  (map 'string #'code-char (if +native-utf-16+
                               (encode-utf-16 unicode-string)
                               unicode-string)))

(defun to-unicode-string (lisp-string)
  "Take a Lisp string and turn it into a vector of Unicode code points."
  (let ((encoded-string (map 'vector #'char-code lisp-string)))
    (coerce (if +native-utf-16+
                (decode-utf-16 encoded-string)
                encoded-string)
            'unicode-string)))
