(load "messenger.lisp")

(defun simular-conversa ()
  (format t "--- INÍCIO DA COMUNICAÇÃO ---~%")

  ;; 1. Beto gera as chaves e disponibiliza a pública
  (let* ((beto-keys (generate-keys 61 53))
         (beto-pub (getf beto-keys :pub))
         (beto-priv (getf beto-keys :priv))

         ;; 2. Ana escreve e cifra a mensagem
         (texto-original "LISP EH LEGAL")
         (pacote-cifrado (ana-sends texto-original beto-pub)))

    (format t "Ana enviou (cifrado): ~A~%" pacote-cifrado)

    ;; 3. Beto recebe e decifra
    (let ((texto-recebido (beto-receives pacote-cifrado beto-priv)))
      (format t "Beto leu: ~A~%" texto-recebido))))

(simular-conversa)
