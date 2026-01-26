;;; messenger.lisp
(load "rsa.lisp")
(load "padding.lisp")

;; --- Utilitários de Conversão ---
(defun os2ip (octets)
  (reduce (lambda (acc byte) (+ (* acc 256) byte)) octets :initial-value 0))

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
  (loop for i from 0 below (length text) by size
        collect (subseq text i (min (+ i size) (length text)))))

;; --- NOVA: Função de Hash (Simulando SHA-1) ---
(defun simple-hash (octets)
  "Reduz qualquer lista de bytes para exatos 20 bytes usando XOR."
  (let ((digest (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for byte in octets
          for i from 0
          do (setf (aref digest (mod i 20))
                   (logxor (aref digest (mod i 20)) byte)))
    (coerce digest 'list)))

;; --- Cifragem (Confidencialidade) ---
(defun rsa-encrypt-string (message public-key)
  (let* ((block-size (key-block-size public-key))
         (payload-size (- block-size 11))
         (chunks (chunk-text message payload-size)))
    (mapcar (lambda (chunk)
              (let* ((octets (text-to-octets chunk))
                     (padded (pad-block octets block-size :encryption))
                     (m (os2ip padded)))
                (encrypt m public-key)))
            chunks)))

(defun rsa-decrypt-string (cipher-list private-key)
  (let* ((block-size (key-block-size private-key)))
    (let ((decrypted-chunks
            (mapcar (lambda (c)
                      (let* ((m (decrypt c private-key))
                             (padded (i2osp m block-size))
                             (data (unpad-block padded)))
                        (octets-to-text data)))
                    cipher-list)))
      (apply #'concatenate 'string decrypted-chunks))))

;; --- Assinatura (Autenticidade - RFC 8017) ---
(defun rsa-sign-string (message private-key)
  "Assina o HASH da mensagem."
  (let* ((block-size (key-block-size private-key))
         (octets (text-to-octets message))
         (digest (simple-hash octets)) ;; Resumo fixo de 20 bytes
         (padded (pad-block digest block-size :signature))
         (m (os2ip padded)))
    (rsasp1 private-key m)))

(defun rsa-verify-string (message signature public-key)
  "Verifica se o Hash da assinatura bate com o Hash da mensagem original."
  (let* ((block-size (key-block-size public-key))
         (s-rep (rsavp1 public-key signature))
         (padded (i2osp s-rep block-size))
         (decrypted-hash (unpad-block padded))
         (actual-hash (simple-hash (text-to-octets message))))
    (equal decrypted-hash actual-hash)))
