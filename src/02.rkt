#|
Day 2: I Was Told There Would Be No Math
========================================

The elves are running low on wrapping paper, and so they need to submit an order
for more. They have a list of the dimensions (length l, width w, and height h)
of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which
makes calculating the required wrapping paper for each gift a little easier:
find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also
need a little extra paper for each present: the area of the smallest side.

For example:

    - [x] A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square
          feet of wrapping paper plus 6 square feet of slack, for a total of 58
          square feet.
    - [x] A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42
          square feet of wrapping paper plus 1 square foot of slack, for a total
          of 43 square feet.

All numbers in the elves' list are in feet. How many total square feet of
wrapping paper should they order?
|#

#lang racket

(module+ test
    (require (prefix-in test: rackunit)))

;; purpose:
;;     given its dimensions, calculate the surface area of a box
;; contract:
;;     Integer Integer Integer -> Integer
(module+ test
    ;; a box with 2x3x4 should need 58 units of paper
    (test:check-equal? (surface-area-of-box 2 3 4)
                       58
                       "Failed for the 2x3x4 box")
    ;; a box with 1x1x10 should need 43 units of paper
    (test:check-equal? (surface-area-of-box 1 1 10)
                       43
                       "Failed for the 1x1x10 box"))
(define (surface-area-of-box length width height)
    (let ([surface-1 (* length width)]
          [surface-2 (* width height)]
          [surface-3 (* height length)])
          (+ (* 2 surface-1) (* 2 surface-2) (* 2 surface-3)
             (min surface-1 surface-2 surface-3))))

;; purpose:
;;     given a string of the format "LxWxH", find the individual dimensions of
;;     the box in a list
;; contract:
;;     String -> (Listof Integer)
(module+ test
    (test:check-equal? (get-box-sizes "1x2x3")
                       (list 1 2 3)
                       "Failed to grab information for the 1x2x3 sized box")
    (test:check-equal? (get-box-sizes "1x22x3")
                       (list 1 22 3)
                       "Failed to grab information for the 1x22x3 sized box"))
(define (get-box-sizes info-str)
    (map string->number
        (string-split info-str "x")))

;; purpose:
;;     given the file with the dimensions of each box, find the total amount of
;;     paper needed to wrap everything
;; contract:
;;     String -> Integer
(define (wrap-presents filename)
    (for/sum ([line (file->lines filename)])
        (apply surface-area-of-box (get-box-sizes line))))

(wrap-presents "02-input.txt") ;; => 1586300

#|
--- Part Two ---

The elves are also running low on ribbon. Ribbon is all the same width, so they
only have to worry about the length they need to order, which they would again
like to be exact.

The ribbon required to wrap a present is the shortest distance around its sides,
or the smallest perimeter of any one face. Each present also requires a bow made
out of ribbon as well; the feet of ribbon required for the perfect bow is equal
to the cubic feet of volume of the present. Don't ask how they tie the bow,
though; they'll never tell.

For example:

    - [ ] A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon
          to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a
          total of 34 feet.
    - [ ] A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon
          to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a
          total of 14 feet.

How many total feet of ribbon should they order?
|#

;; purpose:
;;     calculates a perimeter of a rectangle
;; contract:
;;     Integer Integer -> Integer
(define (rectangular-perimeter l1 l2)
    (+ (* 2 l1) (* 2 l2)))

;; purpose:
;;     calculate the needed amount of ribbon for a present
;; contract:
;;     String -> Integer
(module+ test
    (test:check-equal? (ribbon-dat-present 2 3 4)
                       34
                       "Failed for the 2x3x4 box")
    (test:check-equal? (ribbon-dat-present 1 1 10)
                       14
                       "Failed for the 1x1x10 box"))
(define (ribbon-dat-present box-len box-width box-height)
    (let ([perimeter-1 (rectangular-perimeter box-len box-width)]
          [perimeter-2 (rectangular-perimeter box-len box-height)]
          [perimeter-3 (rectangular-perimeter box-width box-height)]
          [volume      (* box-len box-width box-height)])
        (+ volume (min perimeter-1 perimeter-2 perimeter-3))))

;; purpose:
;;     given the file with the dimensions of each box, find the total amount of
;;     ribbon needed
;; contract:
;;     String -> Integer
(define (ribbon-all-presents filename)
    (for/sum ([line (file->lines filename)])
        (apply ribbon-dat-present (get-box-sizes line))))

(ribbon-all-presents "02-input.txt") ;; => 3737498
