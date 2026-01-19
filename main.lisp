(load "rsa.lisp")

(defun run-test ()
  (let* ((p 61) (q 53) ; Primos de exemplo
         (keys (generate-keys p q))
         (msg 42))
    (format t "Chaves: ~A~%" keys)
    (let* ((crypted (encrypt msg (getf keys :pub)))
           (decrypted (decrypt crypted (getf keys :priv))))
      (format t "Original: ~A | Cifrado: ~A | Decifrado: ~A~%"
              msg crypted decrypted))))

(run-test)
