module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "FizzBuzz!"
    | n `mod` 5  == 0 = "Fizz!"
    | n `mod` 3  == 0 = "Buzz!"
    | otherwise       = numberToWords n

numberToWords :: Int -> String
numberToWords n
    | n > 0 && n < 20 = lessThan20 n
    | n >= 20 && n < 100 =
        let (t, u) = (n `div` 10, n `mod` 10)
        in if u == 0 then tens t else tens t ++ " " ++ lessThan20 u
    | otherwise = "out of range"

lessThan20 :: Int -> String
lessThan20 n =
    let answers = words "one two three four five six seven eight nine ten " ++
                  words "eleven twelve thirteen fourteen fifteen sixteen " ++
                  words "seventeen eighteen nineteen"
    in answers !! (n - 1)

tens :: Int -> String
tens n =
    let answers = words "twenty thirty forty fifty sixty seventy eighty ninety "
    in answers !! (n - 2)
