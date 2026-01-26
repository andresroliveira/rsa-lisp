;;; main.lisp
(load "messenger.lisp")
(load "primes.lisp")

(defun main ()
  (format t "=== RSA-100 TOOL (RFC 8017 COMPLETO COM HASH) ===~%~%")

  (let* ((keys (generate-keys +rsa-100-p+ +rsa-100-q+))
         (pub (getf keys :pub))
         (priv (getf keys :priv))
         ;; Use a frase que quiser, o Hash resolve o erro de tamanho!
         (msg "LISP NO RSA-100 EH INCRIVEL E SEGURO. AGORA PODEMOS ESCREVER TEXTOS GIGANTES NA ASSINATURA."))

    ;; 1. ANA ASSINA A MENSAGEM
    (format t "[SISTEMA] Gerando assinatura digital (Digest de 20 bytes)...~%")
    (let ((assinatura (rsa-sign-string msg priv)))
      (format t "[INFO] Assinatura gerada com sucesso.~%~%")

      ;; 2. ANA CIFRA A MENSAGEM
      (format t "[SISTEMA] Cifrando mensagem em blocos...~%")
      (let ((cifrado (rsa-encrypt-string msg pub)))
        (format t "[ANA] Enviou ~D bloco(s) cifrado(s).~%~%" (length cifrado))

        ;; 3. BETO RECEBE E PROCESSA
        (format t "[SISTEMA] Beto iniciando recepção...~%")
        (let ((decifrado (rsa-decrypt-string cifrado priv)))
          (format t "[BETO] Mensagem decifrada: \"~A\"~%" decifrado)

          ;; 4. VERIFICAÇÃO DA ASSINATURA
          (if (rsa-verify-string decifrado assinatura pub)
              (format t "[STATUS] ASSINATURA VALIDA: Mensagem autentica e integra!~%")
              (format t "[STATUS] ERRO: Falha na veracidade da mensagem.~%")))))))

;; Executa o ponto de entrada
(main)
