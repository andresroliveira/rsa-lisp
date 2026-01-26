;;; padding.lisp

(defun generate-padding-string (len &optional (type :encryption))
  "Gera string de padding. :encryption (0x02) usa aleatórios, :signature (0x01) usa 0xFF."
  (loop repeat len
        collect (if (eq type :encryption)
                    (1+ (random 255))
                    255)))

(defun pad-block (data-octets block-size &optional (type :encryption))
  "Aplica padding conforme RFC 8017. Type pode ser :encryption ou :signature."
  (let* ((data-len (length data-octets))
         (bt (if (eq type :encryption) 2 1))
         (pad-len (- block-size data-len 3)))
    (if (< pad-len 8)
        (error "Mensagem grande demais para o bloco.")
        (append (list 0 bt)
                (generate-padding-string pad-len type)
                (list 0)
                data-octets))))

(defun unpad-block (padded-octets)
  "Remove o preenchimento. A lógica permanece a mesma."
  (let ((separator-pos (position 0 padded-octets :start 2)))
    (if (not separator-pos)
        (error "Padding inválido: separador não encontrado.")
        (subseq padded-octets (1+ separator-pos)))))
