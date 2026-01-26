;;; main.lisp


(load "messenger.lisp")
(load "primes.lisp")

(defun main ()
  (format t "=== RSA-2048 TOOL (RFC 8017 COMPLETO COM SHA-256) ===~%~%")

  (let* ((keys (generate-keys +rsa-2048-p+ +rsa-2048-q+))
         (pub (getf keys :pub))
         (priv (getf keys :priv))
         (msg "LISP NO RSA-2048 EH O PADRAO DE SEGURANCA DA INTERNET MODERNA."))

    (format t "[SISTEMA] Gerando assinatura digital (SHA-256)...~%")
    (let ((assinatura (rsa-sign-string msg priv)))
      (format t "[INFO] Assinatura gerada com sucesso.~%~%")

      (format t "[SISTEMA] Cifrando mensagem em blocos de 256 bytes...~%")
      (let ((cifrado (rsa-encrypt-string msg pub)))
        (format t "[ANA] Enviou ~D bloco(s) cifrado(s).~%~%" (length cifrado))

        (format t "[SISTEMA] Beto iniciando recepção...~%")
        (let ((decifrado (rsa-decrypt-string cifrado priv)))
          (format t "[BETO] Mensagem decifrada: \"~A\"~%" decifrado)

          (if (rsa-verify-string decifrado assinatura pub)
              (format t "[STATUS] ASSINATURA VALIDA: SHA-256 confirmado!~%")
              (format t "[STATUS] ERRO: Assinatura ou integridade falhou.~%")))))))

(main)
