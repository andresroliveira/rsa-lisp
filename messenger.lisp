(load "rsa.lisp")

;; --- Conversores ---

(defun text->numbers (text)
  "Transforma 'OI' em (79 73)"
  (map 'list #'char-code text))

(defun numbers->text (nums)
  "Transforma (79 73) em 'OI'"
  (map 'string #'code-char nums))

;; --- Ações da Ana e do Beto ---

(defun ana-sends (message beto-public-key)
  "Ana cifra cada letra usando a chave pública do Beto"
  (let ((nums (text->numbers message)))
    (mapcar (lambda (n) (encrypt n beto-public-key))
            nums)))

(defun beto-receives (cipher-list beto-private-key)
  "Beto decifra cada número e transforma de volta em texto"
  (let ((nums (mapcar (lambda (c) (decrypt c beto-private-key))
                      cipher-list)))
    (numbers->text nums)))
