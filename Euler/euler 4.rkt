#lang scheme

(define (palindrome? x)
  (define (iter s c lim)
    (if (>= c lim) #t
        (if (not (equal? 
                  (string-ref s c) 
                  (string-ref s (- (- (string-length s) 1) c)))) 
            #f
            (iter s (+ c 1) lim))))
  (iter (number->string x) 0 (floor (/ (string-length (number->string x)) 2))))

(define (create-terms i)
    (build-list i (lambda (x) (* (+ 100 x) i))))
(define (palindromes l)
    (filter palindrome? l))

(define (largestpalindrome)
 (define (iter i largest)
   (if (> i 999) largest
       (iter (+ i 1) (apply max (palindromes (create-terms i))))))
  (iter 100 1))
        