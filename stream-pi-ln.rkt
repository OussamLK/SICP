#lang racket
(define (avg x y) (/ (+ x y) 2))

(define (first-n s n)
  (if (= n 0) 'done
      (begin (display (stream-first s))
             (display ", ")
             (first-n (stream-rest s) (- n 1)))))

(define (stream-average s1 s2)
  (let ((f1 (stream-first s1))
       (f2 (stream-first s2))
       (r1 (stream-rest s1))
       (r2 (stream-rest s2)))
  (stream-cons (avg f1 f2) (stream-average r1 r2))))

(define (num-stream-div n s)
  (stream-cons (/ n (stream-first s)) (num-stream-div n (stream-rest s))))


(define x 16)

(define (sqrt x)
  (define sqrt-stream (stream-cons 1 (stream-average sqrt-stream (num-stream-div x sqrt-stream))))
  (define (loop index)
    (let ((si (stream-ref sqrt-stream index))
          (si1 (stream-ref sqrt-stream (+ index 1))))
      (if (< (abs (- si si1)) 0.01) (* 1.0 si) (loop (+ index 1)))))
  (loop 0))

(define (get-ln2-summands)
  (define (ln2-partial n)
    (stream-cons (/ 1 n) (stream-map - (ln2-partial (+ n 1)))))
  (ln2-partial 1))
(define ln2-summands (get-ln2-summands))

(define (partial-sum s n) (if (= n 0) 0.0 (+ (stream-first s) (partial-sum (stream-rest s) (- n 1)))))

(define (approx-ln2 n) (partial-sum ln2-summands n))

(define (interleave s1 s2)
  (stream-cons (stream-first s1) (interleave s2 (stream-rest s1))))

(define (stream-shift s n)
  (stream-map (lambda (si) (+ si n)) s))

(define integers (stream-cons 0 (stream-shift integers 1)))
(define list-integers (stream-map (lambda (e) (list e)) integers))


(define (pairs s t)
  (let ((p1 (cons (stream-first s) (stream-first t)))
        (p2 (stream-map (lambda (ti) (cons (stream-first s) ti))
                        (stream-rest t) )))
    (stream-cons p1 (interleave p2 (pairs (stream-rest s) (stream-rest t))))))



(define int-pairs-i<j (pairs integers list-integers))


(define (invert-stream-pairs s)
  (let ((f (stream-first s))
        (r (stream-rest s)))
    (stream-cons (cons (cadr f) (car f)) (invert-stream-pairs (stream-rest s)))))

(define int-pairs (interleave (pairs integers integers)
                              (invert-stream-pairs
                               (pairs integers (stream-rest integers)))))

(define (pairs-louis-reasoner s t)
  ;;infinite recursion...
  ;;in the first version, the recursive call is inside a stream-cons so it is delayed
  ;;here it is inside the interleave hence it is evaluated before the call can be made
  (interleave (stream-map (lambda (ti) (list (stream-first s) ti)) t )
               (pairs-louis-reasoner (stream-rest s) (stream-rest t))))

(define int-pairs-i<j<k (pairs (pairs integers list-integers) list-integers))
(define intijk-flat (stream-map flatten int-pairs-i<j<k ))

(define (weighted-merge s1 s2 w)
  (let ((f1 (stream-first s1))
        (f2 (stream-first s2)))
    (if (< (w f1) (w f2)) (stream-cons f1 (weighted-merge (stream-rest s1) s2 w))
                          (stream-cons f2 (weighted-merge s1 (stream-rest s2) w)))))

(define test-wm (weighted-merge integers (stream-map (lambda (i) (* i 3)) integers ) (lambda (n) n) ))

(define (weighted-pair-merge s1 s2 w)
  (let ((piece1 (stream-map (lambda (e) (list (stream-first s1) e)) (stream-rest s2))))
  (stream-cons (list (stream-first s1) (stream-first s2))
               (weighted-merge piece1
                               (weighted-pair-merge (stream-rest s1)
                                                    (stream-rest s2)
                                                     w)
                               w))))
(define (weight pair)
   (apply + pair))

(define i-j (weighted-pair-merge integers integers weight))
;(first-n i-j 20)

(define (weight-b pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(define i-j-b (weighted-pair-merge integers integers weight-b))
(first-n i-j-b 20)







