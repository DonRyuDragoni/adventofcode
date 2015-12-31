#|
Day 1: Not Quite Lisp
=====================

Santa was hoping for a white Christmas, but his weather machine's "snow"
function is powered by stars, and he's fresh out! To save Christmas, he needs
you to collect fifty stars by December 25th.

Collect stars by helping Santa solve puzzles. Two puzzles will be made available
on each day in the advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

Here's an easy puzzle to warm you up.

Santa is trying to deliver presents in a large apartment building, but he can't
find the right floor - the directions he got are a little confusing. He starts
on the ground floor (floor 0) and then follows the instructions one character at
a time.

An opening parenthesis, (, means he should go up one floor, and a closing
parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will
never find the top or bottom floors.

For example:

    - [x] (()) and ()() both result in floor 0.
    - [x] ((( and (()(()( both result in floor 3.
    - [x] ))((((( also results in floor 3.
    - [x] ()) and ))( both result in floor -1 (the first basement level).
    - [x] ))) and )())()) both result in floor -3.

To what floor do the instructions take Santa?
|#

#lang racket

(require (prefix-in url: net/url))

(module+ test
    (require (prefix-in test: rackunit)))

;; my data for this puzzle
(define puzzle-input
  (read-line (open-input-file "01-input.txt")))

;; purpose:
;;     given the instructions, find the floor Santa is supposed to go
;; contract:
;;     String -> Integer
(module+ test
    ;; (()) and ()() both result in floor 0
    (test:check-equal? (find-floor "(())")
                       0
                       "Failed to find 0th floor in first case")
    (test:check-equal? (find-floor "()()")
                       0
                       "Failed to find 0th floor in second case")
    ;; ((( and (()(()( both result in floor 3
    (test:check-equal? (find-floor "(((")
                       3
                       "Failed to find 3rd floor in first case")
    (test:check-equal? (find-floor "(()(()(")
                       3
                       "Failed to find 3rd floor in second case")
    ;; ))((((( also results in floor 3
    (test:check-equal? (find-floor "))(((((")
                       3
                       "Failed to find 3rd floor in the 3rd test")
    ;; ()) and ))( both result in floor -1 (the first basement level).
    (test:check-equal? (find-floor "())")
                       -1
                       "Failed to find -1st floor in first case")
    (test:check-equal? (find-floor "))(")
                       -1
                       "Failed to find -1st floor in second case")
    ;; ))) and )())()) both result in floor -3.
    (test:check-equal? (find-floor ")))")
                       -3
                       "Failed to find -3rd floor in first case")
    (test:check-equal? (find-floor ")())())")
                       -3
                       "Failed to find -3rd floor in second case"))
(define (find-floor instruction-str)
  (for/sum ([c instruction-str])
    (cond
      [(char=? c #\() 1]
      [(char=? c #\)) -1]
      [else 0])))

(displayln (find-floor puzzle-input)) ;; => 232

#|
--- Part Two ---

Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.

For example:

    - [ ] ) causes him to enter the basement at character position 1.
    - [ ] ()()) causes him to enter the basement at character position 5.

What is the position of the character that causes Santa to first enter the basement?
|#

;; purpose:
;;     find the first character that makes the sum return -1
;; contract:
;;     String -> Integer
(module+ test
  ;; ) causes him to enter the basement at character position 1
  (test:check-equal? (find-basement ")")
                     1
                     "Failed to find basement in the first case")
  ;; ()()) causes him to enter the basement at character position 5.
  (test:check-equal? (find-basement "()())")
                     5
                     "Failed to find basement in the first case"))
(define (find-basement input-str [num-chars 0])
    (let ([input-len (string-length input-str)])
        (cond
            [(> num-chars input-len) -1] ;; return -1 in case no basement is found
            [(= -1
               (find-floor (substring input-str 0 num-chars)))
               num-chars]
            [else (find-basement input-str (add1 num-chars))])))

(find-basement puzzle-input)
