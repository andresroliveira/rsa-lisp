;;; rsa.lisp
(load "math.lisp")

(defconstant +octeto-size+ 8)

(defun generate-keys (p q)
  "Gera chaves com componentes CRT: dP, dQ e qInv."
  (let* ((n (* p q))
         (phi (* (1- p) (1- q)))
         (e 65537)
         (d (mod-inverse e phi))
         ;; Coeficientes CRT
         (dp (mod d (1- p)))
         (dq (mod d (1- q)))
         (qinv (mod-inverse q p)))
    (list :pub (list e n)
          :priv (list d n p q dp dq qinv))))

(defun encrypt (m pub-key)
  (exp-mod m (first pub-key) (second pub-key)))

(defun decrypt-crt (c priv-key)
  "Decifração acelerada via Teorema Chinês do Resto."
  (let ((p (third priv-key))
        (q (fourth priv-key))
        (dp (fifth priv-key))
        (dq (sixth priv-key))
        (qinv (seventh priv-key)))
    (let* ((m1 (exp-mod c dp p))
           (m2 (exp-mod c dq q))
           ;; h = qInv * (m1 - m2) mod p
           (h (mod (* qinv (+ (- m1 m2) p)) p)))
      (+ m2 (* h q)))))

(defun key-octet-length (key)
  (ceiling (integer-length (second key)) +octeto-size+))

(defun rsasp1 (priv-key m)
  "Assinatura (também pode usar CRT para ser mais rápida)."
  (decrypt-crt m priv-key))

(defun rsavp1 (pub-key s)
  (exp-mod s (first pub-key) (second pub-key)))
