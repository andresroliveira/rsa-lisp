;; Exponenciação modular recursiva (Exponenciação binária)
(defun exp-mod (base exp n)
  (cond ((= exp 0) 1)
        ((evenp exp)
         (let ((half (exp-mod base (/ exp 2) n)))
           (mod (* half half) n)))
        (t
         (mod (* base (exp-mod base (1- exp) n)) n))))

;; Algoritmo de Euclides Estendido recursivo
(defun extended-gcd (a b)
  (if (= b 0)
      (values a 1 0)
      (multiple-value-bind (g x1 y1) (extended-gcd b (mod a b))
        (values g y1 (- x1 (* (floor a b) y1))))))

;; Inverso modular
(defun mod-inverse (e phi)
  (multiple-value-bind (g x y) (extended-gcd e phi)
    (declare (ignore y))
    (if (= g 1)
        (mod x phi)
        (error "Sem inverso modular"))))
