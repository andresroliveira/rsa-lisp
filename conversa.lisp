;;; conversa.lisp

(load "messenger.lisp")

(defun simular-conversa-rsa100 ()
  (format t "--- INICIANDO SIMULAÇÃO RSA-100 ---~%~%")

  ;; 1. Beto define seus primos gigantes (Fatores do RSA-100)
  (let* ((p 37975227936943673922808872755445627854565536638199)
         (q 40094690950920881030683735292761468389214899724061)

         ;; 2. Beto gera as chaves
         (keys (generate-keys p q))
         (pub (getf keys :pub))
         (priv (getf keys :priv))

         ;; 3. Ana escreve a mensagem
         (msg "LISP NO RSA-100 EH INCRIVEL")
         (tamanho (length msg)))

    (format t "Módulo (n) de 100 dígitos gerado com sucesso.~%")
    (format t "Chave Pública (e): ~A~%~%" (first pub))

    ;; 4. Ana cifra a frase inteira como UM ÚNICO número
    (let ((cifrado (rsa-encrypt-string msg pub)))
      (format t "Ana enviou o Super Número cifrado:~%~A~%~%" cifrado)

      ;; 5. Beto recebe, decifra e reconstrói
      (let ((recebido (rsa-decrypt-string cifrado priv tamanho)))
        (format t "Beto decifrou a mensagem:~%\"~A\"~%" recebido)))))

(simular-conversa-rsa100)
