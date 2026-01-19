;;; messenger.lisp
(load "rsa.lisp")

(defun os2ip (octets)
  (reduce (lambda (acc byte) (+ (* acc 256) byte))
          octets :initial-value 0))

(defun i2osp (x xlen)
  (let ((result nil))
    (loop repeat xlen
          do (push (mod x 256) result)
             (setf x (floor x 256)))
    result))

(defun text-to-octets (str) (map 'list #'char-code str))
(defun octets-to-text (octets) (map 'string #'code-char octets))

(defun chunk-text (text size)
  "Divide o texto em pedaços de tamanho 'size'."
  (loop for i from 0 below (length text) by size
        collect (subseq text i (min (+ i size) (length text)))))

(defun rsa-encrypt-string (message public-key)
  "Fatia a mensagem conforme a chave e cifra cada bloco."
  (let* ((block-size (key-block-size public-key))
         (chunks (chunk-text message block-size)))
    (mapcar (lambda (chunk)
              (encrypt (os2ip (text-to-octets chunk)) public-key))
            chunks)))

(defun rsa-decrypt-string (cipher-list private-key total-length)
  "Decifra os blocos e junta as strings respeitando o tamanho original."
  (let* ((block-size (key-block-size private-key))
         (num-chunks (length cipher-list)))
    (let ((decrypted-chunks
           (loop for c in cipher-list
                 for i from 1
                 collect (let* ((m (decrypt c private-key))
                                ;; Calcula se o bloco atual é o último (pode ser menor)
                                (len (if (= i num-chunks)
                                         (let ((rem (mod total-length block-size)))
                                           (if (zerop rem) block-size rem))
                                         block-size)))
                           (octets-to-text (i2osp m len))))))
      (apply #'concatenate 'string decrypted-chunks))))
