#lang scheme

(define (fib x)
  (if (>= 2 x)
      1
      (+ (fib (- x 1)) (fib (- x 2)))))

(define (even x)
  (= 0 (modulo x 2)))

(define (fibiter i sum lim)
  (if (>= (fib i) lim)
          sum
          (if (even (fib i))
              (fibiter (+ i 1) (+ sum (fib i)) lim)
              (fibiter (+ i 1) sum lim))))

(define (add-up-to x)
  (fibiter 1 0 x))

(add-up-to 4000000)
  