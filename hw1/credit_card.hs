toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)

revList :: [a] -> [a]
revList [] = []
revList (x : xs) = (revList xs) ++ [x]

toDigits :: Integer -> [Integer]
toDigits n = revList (toDigitsRev n)

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x : []) = [2 * x]
doubleEveryOtherHelper (x : y : xs) = [2 * x] ++ [y] ++ (doubleEveryOtherHelper xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : xs) =
		 if length xs `mod` 2 == 1
		    then (doubleEveryOtherHelper (x : xs))
		    else x : (doubleEveryOtherHelper xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : []) = foldl (+) 0 (toDigits(x))
sumDigits (x : xs) = sumDigits(toDigits x) + sumDigits(xs)

