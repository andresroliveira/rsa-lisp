;;; messenger.lisp

(load "rsa.lisp")
(load "padding.lisp")

(defun os2ip (octets)
  (reduce (lambda (acc byte) (+ (* acc 256) byte))
          octets :initial-value 0))

(defun i2osp (x xlen)
  (if (>= x (expt 256 xlen))
      (error "Integer too large for xLen ~D" xlen)
      (let ((result nil))
        (loop repeat xlen
              do (push (mod x 256) result)
              (setf x (floor x 256)))
        result)))

(defun text-to-octets (str) (map 'list #'char-code str))
(defun octets-to-text (octets) (map 'string #'code-char octets))

(defun chunk-text (text size)
  "Divide o texto em pedaços de tamanho 'size'."
  (loop for i from 0 below (length text) by size
        collect (subseq text i (min (+ i size) (length text)))))

(defun rsa-encrypt-string (message public-key)
  "Fatia a mensagem, aplica padding e cifra cada bloco."
  (let* ((block-size (key-block-size public-key))
         ;; O espaço útil é reduzido pelo overhead do padding (11 bytes)
         (payload-size (- block-size 11))
         (chunks (chunk-text message payload-size)))
    (mapcar (lambda (chunk)
              (let* ((octets (text-to-octets chunk))
                     (padded (pad-block octets block-size))
                     (m (os2ip padded)))
                (encrypt m public-key)))
            chunks)))

(defun rsa-decrypt-string (cipher-list private-key)
  "Decifra os blocos, remove o padding e reconstrói a string original."
  (let* ((block-size (key-block-size private-key)))
    (let ((decrypted-chunks
           (mapcar (lambda (c)
                     (let* ((m (decrypt c private-key))
                            (padded (i2osp m block-size))
                            (data (unpad-block padded)))
                       (octets-to-text data)))
                   cipher-list)))
      (apply #'concatenate 'string decrypted-chunks))))
