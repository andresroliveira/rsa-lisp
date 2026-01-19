;;; messenger.lisp

(load "rsa.lisp")

(defun os2ip (octets)
  "Octet-String-to-Integer: Transforma uma sequência de bytes em um único inteiro gigante."
  (reduce (lambda (acc byte)
            (+ (* acc 256) byte))
          octets
          :initial-value 0))

(defun i2osp (x xlen)
  "Integer-to-Octet-String: Transforma o inteiro de volta em bytes, garantindo o tamanho xlen."
  (let ((result nil))
    (loop repeat xlen
          do (push (mod x 256) result)
             (setf x (floor x 256)))
    result))

(defun text-to-octets (str)
  "Nossa versão manual: Transforma string em lista de códigos."
  (map 'list #'char-code str))

(defun octets-to-text (octets)
  "Nossa versão manual: Transforma lista de códigos em string."
  (map 'string #'code-char octets))

(defun rsa-encrypt-string (message public-key)
  "Pega o texto, gera o Super Número e cifra."
  (let* ((octets (text-to-octets message))
         (m (os2ip octets)))
    (encrypt m public-key)))

(defun rsa-decrypt-string (ciphertext private-key msg-len)
  "Decifra o blocão e reconstrói o texto de tamanho msg-len."
  (let* ((m (decrypt ciphertext private-key))
         (octets (i2osp m msg-len)))
    (octets-to-text octets)))
