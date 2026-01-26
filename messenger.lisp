;;; messenger.lisp


(load "rsa.lisp")
(load "padding.lisp")
(load "hash.lisp")

(defun os2ip (octets)
  "Octet-String-to-Integer: Converte uma lista de bytes em um inteiro único."
  (reduce (lambda (acc byte) (+ (* acc 256) byte)) octets :initial-value 0))

(defun i2osp (x xlen)
  "Integer-to-Octet-String: Converte um inteiro em uma lista de bytes de tamanho xLen."
  (if (>= x (expt 256 xlen))
      (error "Integer too large for xLen ~D" xlen)
      (let ((result nil))
        (loop repeat xlen
              do (push (mod x 256) result)
                 (setf x (floor x 256)))
        result)))

(defun text-to-octets (str)
  "Converte string para lista de códigos ASCII."
  (map 'list #'char-code str))

(defun octets-to-text (octets)
  "Converte lista de códigos ASCII para string."
  (map 'string #'code-char octets))

(defun chunk-text (text size)
  "Divide o texto em blocos baseados no espaço útil (payload) do RSA."
  (loop for i from 0 below (length text) by size
        collect (subseq text i (min (+ i size) (length text)))))

(defun rsa-encrypt-string (message public-key)
  "Fatia a mensagem, aplica Padding Tipo 02 e cifra cada bloco."
  (let* ((k (key-octet-length public-key))
         ;; Espaço útil = Tamanho total (k) - Overhead do Padding (11)
         (payload-size (- k 11))
         (chunks (chunk-text message payload-size)))
    (mapcar (lambda (chunk)
              (let* ((octets (text-to-octets chunk))
                     (padded (pad-block octets k :encryption))
                     (m (os2ip padded)))
                (encrypt m public-key)))
            chunks)))

(defun rsa-decrypt-string (cipher-list private-key)
  "Decifra usando a otimização CRT."
  (let ((k (key-octet-length private-key)))
    (let ((decrypted-chunks
            (mapcar (lambda (c)
                      (let* ((m (decrypt-crt c private-key)) ;; Chamada CRT aqui
                             (padded (i2osp m k))
                             (data (unpad-block padded)))
                        (octets-to-text data)))
                    cipher-list)))
      (apply #'concatenate 'string decrypted-chunks))))

(defun rsa-sign-string (message private-key)
  "Gera uma assinatura digital única para o Hash SHA-256 da mensagem."
  (let ((k (key-octet-length private-key)))
    (let* ((octets (text-to-octets message))
           (digest (sha256 octets)) ;; Gera resumo de 32 bytes
           (padded (pad-block digest k :signature)) ;; Padding Tipo 01
           (m (os2ip padded)))
      (rsasp1 private-key m))))

(defun rsa-verify-string (message signature public-key)
  "Verifica se o Hash contido na assinatura bate com o Hash da mensagem atual."
  (let ((k (key-octet-length public-key)))
    (let* ((s-rep (rsavp1 public-key signature))
           ;; Converte o representante da assinatura para k bytes
           (padded (i2osp s-rep k))
           (decrypted-hash (unpad-block padded))
           (actual-hash (sha256 (text-to-octets message))))
      (equal decrypted-hash actual-hash))))
