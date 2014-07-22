#lang scheme

(define (mul3or5? x)
  (or (= 0 (modulo x 3)) (= 0 (modulo x 5))))

(define (sumiter i lim sum)
  (if 
   (= lim i)
      sum
      (if (mul3or5? i)
          (sumiter (+ i 1) lim (+ sum i))
          (sumiter (+ i 1) lim sum))))

(define (addupto x)
  (sumiter 0 x 0))
  