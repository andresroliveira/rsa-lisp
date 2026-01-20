;;; padding.lisp

(defun pad-block (data-octets block-size)
  "Aplica preenchimento determinístico (padrão RFC 8017 simplificado)."
  (let* ((data-len (length data-octets))
         ;; 3 bytes são ocupados pelo cabeçalho (00 02) e o separador (00)
         (pad-len (- block-size data-len 3)))
    (if (< pad-len 8) ;; A RFC exige pelo menos 8 bytes de padding para segurança
        (error "Mensagem grande demais para o bloco RSA.")
        (append '(0 2)
                (make-list pad-len :initial-element 255) ;; Bytes FF
                '(0)
                data-octets))))

(defun unpad-block (padded-octets)
  "Remove o preenchimento e extrai a mensagem real."
  ;; Procuramos o byte 0 que separa o padding da mensagem
  ;; O cabeçalho ocupa as duas primeiras posições (00 02)
  (let ((separator-pos (position 0 padded-octets :start 2)))
    (if (not separator-pos)
        (error "Padding inválido: separador não encontrado.")
        (subseq padded-octets (1+ separator-pos)))))
