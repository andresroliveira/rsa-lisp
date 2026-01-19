;;; rsa.lisp

(load "math.lisp")

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
