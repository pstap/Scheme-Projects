#lang scheme

(define (sqrt x)
  (define (sqr x)
    (* x x))
  
  (define (abs x)
    (if (< x 0)
        (- x)
        x))
  
  (define (good-enough? guess x)
    (< (abs (- (sqr guess) x)) 0.0001))
  
  (define (improve-guess guess x)
    (* (+ (/ x guess) guess) 0.5))
  
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve-guess guess x) x)))
  
  (sqrt-iter 1 x))


(define (isprime? x)
  (define (even? x)
    (= 0 (modulo x 2)))
  
  (define (iter i lim x)
    (cond ((>= i lim) #t)
          ((= 0 (modulo x i)) #f)
          (else (iter (+ i 1) lim x))))
  
  (if (and (even? x) (not (= x 2))) #f
      (iter 2 (sqrt x) x)))

(define (isfactor? x y)
  (= 0 (modulo y x)))

(define (gpf x)
  (define (iter i x greatest)
    (if (> i x) greatest
        (if (isfactor? i x)
            (if (isprime? i)
                (iter (+ i 1) (/ x i) i)
                (iter (+ i 1) (/ x i) greatest))
            (iter (+ i 1) x greatest))))
  (iter 2 x 1))

(gpf 600851475143)