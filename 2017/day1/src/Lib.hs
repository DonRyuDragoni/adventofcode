{-
Day 1: Inverse Captcha

The night before Christmas, one of Santa's Elves calls you in a panic. "The
printer's broken! We can't print the Naughty or Nice List!" By the time you make
it to sub-basement 17, there are only a few minutes until midnight. "We have a
big problem," she says; "there must be almost fifty bugs in this system, but
nothing else can print The List. Stand in this square, quick! There's no time to
explain; if you can convince them to pay you in stars, you'll be able to--" She
pulls a lever and the world goes blurry.

When your eyes can focus again, everything seems a lot more pixelated than
before. She must have sent you inside the computer! You check the system clock:
25 milliseconds until midnight. With that much time, you should be able to
collect all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
millisecond in the advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

You're standing in a room with "digitization quarantine" written in LEDs along
one wall. The only door is locked, but it includes a small
interface. "Restricted Area - Strictly No Digitized Users Allowed."

It goes on to explain that you may only leave by solving a captcha to prove
you're not a human. Apparently, you only get one millisecond to solve the
captcha: too fast for a normal human, but it feels like hours to you.

The captcha requires you to review a sequence of digits (your puzzle input) and
find the sum of all digits that match the next digit in the list. The list is
circular, so the digit after the last digit is the first digit in the list.

For example:

    - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the
      second digit and the third digit (2) matches the fourth digit.

    - 1111 produces 4 because each digit (all 1) matches the next.

    - 1234 produces 0 because no digit matches the next.

    - 91212129 produces 9 because the only digit that matches the next one is
      the last digit, 9.

What is the solution to your captcha?

--- Part Two ---

You notice a progress bar that jumps to 50% completion. Apparently, the door
isn't yet satisfied, but it did emit a star as encouragement. The instructions
change:

Now, instead of considering the next digit, it wants you to consider the digit
halfway around the circular list. That is, if your list contains 10 items, only
include a digit in your sum if the digit 10/2 = 5 steps forward matches
it. Fortunately, your list has an even number of elements.

For example:

    - 1212 produces 6: the list contains 4 items, and all four digits match the
      digit 2 items ahead.

    - 1221 produces 0, because every comparison is between a 1 and a 2.

    - 123425 produces 4, because both 2s match each other, but no other digit
      has a match.

    - 123123 produces 12.

    - 12131415 produces 4.

-}

module Lib
  ( solveCaptcha
  ) where

import Data.Char

parseCaptcha :: String -> [Int]
parseCaptcha = map digitToInt . takeWhile isDigit

{-
    [1 2 3 4] len = 4
     | | | |
     | | | 3
     | | 2
     | 1
     0

     | | | 7 ??? = 3
     | | 6   ??? = 2
     | 5     ??? = 1
     4       ??? = 0

     ??? = mod len
-}
getNextElement :: [Int] -> Int -> Int
getNextElement lst curr =
  let
    len :: Int
    len = length lst

    distance :: Int
    distance = len `div` 2

    pos :: Int
    pos = (curr + distance) `mod` len
  in
    lst !! pos

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits lst =
  let
    sumDigits' :: [Int] -> Int -> Int -> Int

    -- Base case: the list was fully exausted, leaving the last element
    -- behind. This means the work is done, so return the result.
    sumDigits' [] acc _ = acc

    -- General case: the list still has elements in it. If the current head is
    -- equal to the element halfway around the list, add it to the count.
    sumDigits' (x:xs) acc cnt =
      if x == getNextElement lst cnt then
        sumDigits' xs (acc + x) (succ cnt)
      else
        sumDigits' xs acc (succ cnt)
  in
    -- Starting state for the recursion: total is zero and the list has a copy
    -- of the first element as its last.
    sumDigits' lst 0 0

solveCaptcha :: String -> Int
solveCaptcha = sumDigits . parseCaptcha
