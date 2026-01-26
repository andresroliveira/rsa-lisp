;;; rsa.lisp

(load "math.lisp")

(defconstant +octeto-size+ 8
  "Tamanho padrão de um octeto em bits.")

(defun generate-keys (p q)
  (let* ((n (* p q))
         (phi (* (1- p) (1- q)))
         (e 65537)
         (d (mod-inverse e phi)))
    (list :pub (list e n) :priv (list d n))))

(defun encrypt (m pub-key)
  (let ((e (first pub-key))
        (n (second pub-key)))
    (exp-mod m e n)))

(defun decrypt (c priv-key)
  (let ((d (first priv-key))
        (n (second priv-key)))
    (exp-mod c d n)))

(defun get-key-modulus (key)
  "Retorna o n da chave."
  (second key))

(defun key-block-size (key)
  "Calcula o tamanho do bloco k-1 baseado no n da chave."
  (let* ((n (get-key-modulus key))
         ;; k é o número de octetos necessários para representar n
         (k (ceiling (integer-length n) +octeto-size+)))
    (1- k)))

(defun rsasp1 (priv-key m)
  "RSA Signature Primitive 1: s = m^d mod n."
  (let ((d (first priv-key))
        (n (second priv-key)))
    (if (or (< m 0) (>= m n))
        (error "Mensagem fora do alcance do módulo.")
        (exp-mod m d n))))

(defun rsavp1 (pub-key s)
  "RSA Verification Primitive 1: m = s^e mod n."
  (let ((e (first pub-key))
        (n (second pub-key)))
    (if (or (< s 0) (>= s n))
        (error "Assinatura fora do alcance do módulo.")
        (exp-mod s e n))))
