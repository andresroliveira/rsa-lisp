;;; padding.lisp

(defun generate-padding-string (len)
  "Gera uma lista de 'len' octetos aleatórios não nulos (1-255)."
  (loop repeat len
        collect (1+ (random 255))))

(defun pad-block (data-octets block-size)
  "Aplica preenchimento probabilístico conforme RFC 8017 Section 7.2.1."
  (let* ((data-len (length data-octets))
         ;; 3 bytes: cabeçalho (00 02) e o separador (00)
         (pad-len (- block-size data-len 3)))
    (if (< pad-len 8)
        (error "Mensagem grande demais para o bloco RSA.")
        (append '(0 2)
                (generate-padding-string pad-len) ;; Agora é aleatório!
                '(0)
                data-octets))))

(defun unpad-block (padded-octets)
  "Remove o preenchimento. A lógica permanece a mesma."
  (let ((separator-pos (position 0 padded-octets :start 2)))
    (if (not separator-pos)
        (error "Padding inválido: separador não encontrado.")
        (subseq padded-octets (1+ separator-pos)))))
