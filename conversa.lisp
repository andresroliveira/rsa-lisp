;;; conversa.lisp

(load "messenger.lisp")
(load "primes.lisp")

(defun simular-conversa-rsa100 ()
  (format t "--- INICIANDO SIMULAÇÃO RSA-100 ---~%~%")

  ;; 1. Beto define seus primos gigantes (Fatores do RSA-100)
  (let* ((p +rsa-100-p+)
         (q +rsa-100-q+)

         ;; 2. Beto gera as chaves
         (keys (generate-keys p q))
         (pub (getf keys :pub))
         (priv (getf keys :priv))

         ;; 3. Ana escreve a mensagem
         ;; Teste com uma mensagem que pode ou não gerar vários blocos
         ;; (msg "LISP NO RSA-100 EH INCRIVEL")
         (msg "ESTA EH UMA MENSAGEM BEM LONGA PARA TESTAR O FATIAMENTO AUTOMATICO DO NOSSO SISTEMA RSA")
         (tamanho (length msg)))

    (format t "Módulo (n) de 100 dígitos gerado com sucesso.~%")
    (format t "Chave Pública (e): ~A~%~%" (first pub))

    ;; 4. Ana cifra a frase (gera uma LISTA de blocos cifrados)
    (let ((cifrado (rsa-encrypt-string msg pub)))
      (format t "Ana enviou ~A bloco(s) cifrado(s):~%~A~%~%"
              (length cifrado)
              cifrado)

      ;; 5. Beto recebe a lista, decifra bloco a bloco e reconstrói
      (let ((recebido (rsa-decrypt-string cifrado priv tamanho)))
        (format t "Beto decifrou a mensagem:~%\"~A\"~%" recebido)))))

(simular-conversa-rsa100)
