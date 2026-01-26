;;; plot.lisp

(require :asdf)
(require :uiop)

(load "rsa.lisp")
(load "primes.lisp")

(defun generate-rsa-mapping-data (n e count)
  "Gera uma lista de pontos normalizados (m/n, c/n) usando aritmética racional."
  (loop repeat count
        collect (let* ((m (random n))
                       (c (exp-mod m e n)))
                  ;; Primeiro divide (gera um ratio), depois converte para float
                  (list (float (/ m n))
                        (float (/ c n))))))

(defun plot-rsa-mapping (public-key &key (points 3000) (filename "rsa_mapping.png"))
  "Gera o gráfico caótico e salva em um arquivo PNG."
  (let* ((e (first public-key))
         (n (second public-key))
         (data (generate-rsa-mapping-data n e points))
         ;; Removemos o -p (persist) para o script fechar após salvar
         (gnuplot (uiop:launch-program "gnuplot" :input :stream :wait nil)))

    (let ((out (uiop:process-info-input gnuplot)))
      ;; --- CONFIGURAÇÃO DE SAÍDA PARA ARQUIVO ---
      (format out "set terminal pngcairo size 800,850 background rgb 'black'~%")
      (format out "set output '~A'~%" filename)

      (format out "set size square~%") ; Força a proporção 1:1 entre X e Y
      (format out "set xrange [0:1]~%") ; Garante que o eixo X vá de 0 a 1
      (format out "set yrange [0:1]~%") ; Garante que o eixo Y vá de 0 a 1

      ;; Estética (Mantendo o que você gostou)
      (format out "set title 'RSA-2048 Mapping: Chaotic Distribution' tc rgb 'white' font 'Courier,14'~%")
      (format out "set xlabel 'm/n' tc rgb 'white'~%")
      (format out "set ylabel 'c/n' tc rgb 'white'~%")
      (format out "set border lc rgb 'white'~%")
      (format out "set grid lc rgb '#333333'~%")
      (format out "set key off~%")

      ;; Plotagem
      (format out "plot '-' with points pt 7 ps 0.4 lc rgb '#00FF00'~%")

      (dolist (point data)
        (format out "~F ~F~%" (first point) (second point)))

      (format out "e~%")
      (finish-output out)
      (uiop:close-streams gnuplot))
    (format t "[SISTEMA] Imagem salva com sucesso em: ~A~%" filename)))

(defun main ()
  (let* ((keys (generate-keys +rsa-2048-p+ +rsa-2048-q+))
         (pub (getf keys :pub)))
    (plot-rsa-mapping pub :points 3000)))

(main)
