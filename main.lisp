;;; main.lisp

(load "messenger.lisp")
(load "primes.lisp")

(defun simular-conversa ()
  (format t "--- INICIANDO SIMULAÇÃO ---~%~%")

  ;; 1. Beto define seus primos gigantes (Fatores do RSA-100)
  (let* ((p +rsa-100-p+)
         (q +rsa-100-q+)

         ;; 2. Beto gera as chaves
         (keys (generate-keys p q))
         (pub (getf keys :pub))
         (priv (getf keys :priv))

         ;; 3. Ana escreve a mensagem (pode ter qualquer tamanho)
         (msg "ESTA EH UMA MENSAGEM BEM LONGA PARA TESTAR O FATIAMENTO AUTOMATICO DO NOSSO SISTEMA RSA")
         ;; Note: 'tamanho' não é mais necessário para a decifragem
         )

    (format t "Módulo (n) de 100 dígitos gerado com sucesso.~%")
    (format t "Chave Pública (e): ~A~%~%" (first pub))

    ;; 4. Ana cifra a frase
    ;; Agora cada bloco contém Padding: [00][02][FF...FF][00][MENSAGEM]
    (let ((cifrado (rsa-encrypt-string msg pub)))
      (format t "Ana enviou ~A bloco(s) cifrado(s) com padding estruturado:~%~A~%~%"
              (length cifrado)
              cifrado)

      ;; 5. Beto recebe a lista e decifra
      ;; O padding permite que ele saiba onde a mensagem termina sem precisar do tamanho original
      (let ((recebido (rsa-decrypt-string cifrado priv)))
        (format t "Beto removeu o padding e reconstruiu a mensagem:~%\"~A\"~%" recebido)))))

;; Executa a simulação
(simular-conversa)
