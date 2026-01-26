;;; hash.lisp

(defun sha256-ror (x n)
  (logior (ldb (byte 32 0) (ash x (- n)))
          (ldb (byte 32 0) (ash x (- 32 n)))))

(defun sha256-ch (x y z) (logxor (logand x y) (logand (lognot x) z)))
(defun sha256-maj (x y z) (logxor (logand x y) (logand x z) (logand y z)))

(defun sha256-sigma0 (x) (logxor (sha256-ror x 2) (sha256-ror x 13) (sha256-ror x 22)))
(defun sha256-sigma1 (x) (logxor (sha256-ror x 6) (sha256-ror x 11) (sha256-ror x 25)))
(defun sha256-gamma0 (x) (logxor (sha256-ror x 7) (sha256-ror x 18) (ash x -3)))
(defun sha256-gamma1 (x) (logxor (sha256-ror x 17) (sha256-ror x 19) (ash x -10)))

(defparameter *sha256-k*
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun sha256 (octets)
  "Retorna o hash SHA-256 (lista de 32 octetos) de uma lista de entrada."
  (let ((h0 #x6a09e667) (h1 #xbb67ae85) (h2 #x3c6ef372) (h3 #xa54ff53a)
        (h4 #x510e527f) (h5 #x9b05688c) (h6 #x1f83d9ab) (h7 #x5be0cd19)
        (data (append octets (list 128))))
    ;; Preenchimento com zeros
    (loop while (/= (mod (length data) 64) 56) do (setf data (append data (list 0))))
    ;; Adiciona o tamanho (64 bits)
    (let ((len-bits (* 8 (length octets))))
      (loop for i from 56 downto 0 by 8 ;; Loop corrigido (56 até 0)
            do (setf data (append data (list (ldb (byte 8 i) len-bits))))))

    (let ((data-vec (coerce data '(vector (unsigned-byte 8)))))
      (loop for i from 0 below (length data-vec) by 64
            do (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
                 (loop for j from 0 below 16
                       do (let ((idx (+ i (* j 4))))
                            (setf (aref w j)
                                  (logior (ash (aref data-vec idx) 24)
                                          (ash (aref data-vec (+ idx 1)) 16)
                                          (ash (aref data-vec (+ idx 2)) 8)
                                          (aref data-vec (+ idx 3))))))
                 (loop for j from 16 below 64
                       do (let ((s0 (sha256-gamma0 (aref w (- j 15))))
                                (s1 (sha256-gamma1 (aref w (- j 2)))))
                            (setf (aref w j) (ldb (byte 32 0) (+ (aref w (- j 16)) s0 (aref w (- j 7)) s1)))))
                 (let ((a h0) (b h1) (c h2) (d h3) (e h4) (f h5) (g h6) (h h7))
                   (loop for j from 0 below 64
                         do (let* ((temp1 (ldb (byte 32 0) (+ h (sha256-sigma1 e) (sha256-ch e f g) (aref *sha256-k* j) (aref w j))))
                                   (temp2 (ldb (byte 32 0) (+ (sha256-sigma0 a) (sha256-maj a b c)))))
                              (setf h g g f f e e (ldb (byte 32 0) (+ d temp1)) d c c b b a a (ldb (byte 32 0) (+ temp1 temp2)))))
                   (setf h0 (ldb (byte 32 0) (+ h0 a)) h1 (ldb (byte 32 0) (+ h1 b)) h2 (ldb (byte 32 0) (+ h2 c))
                         h3 (ldb (byte 32 0) (+ h3 d)) h4 (ldb (byte 32 0) (+ h4 e)) h5 (ldb (byte 32 0) (+ h5 f))
                         h6 (ldb (byte 32 0) (+ h6 g)) h7 (ldb (byte 32 0) (+ h7 h)))))))
    (let ((result nil))
      (loop for val in (list h0 h1 h2 h3 h4 h5 h6 h7)
            do (loop for pos from 24 downto 0 by 8
                     do (push (ldb (byte 8 pos) val) result)))
      (reverse result))))
