module Week1 where



toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
    | n < 0            = []
    | otherwise        = toDigits (div n 10) ++ [mod n 10]



toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
    | n < 0            = []
    | otherwise        = mod n 10 : toDigitsRev (div n 10)



intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs



isEven :: Integer -> Bool
isEven n
  | mod n 2 == 0   = True
  | otherwise      = False



doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nums = doubleEverOther' nums (isEven (intListLength nums))

doubleEverOther' :: [Integer] -> Bool-> [Integer]
doubleEverOther' [] _                 = []
doubleEverOther' (x:[]) isListEven
      | isListEven                    = [x]
      | otherwise                     = [x * 2]
doubleEverOther' (x:(y:zs)) isListEven
      | isListEven                    = x : y * 2 : doubleEverOther' zs isListEven
      | otherwise                     = x * 2 : y : doubleEverOther' zs isListEven


sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs)
      | x > 9 = sumDigits (toDigits x) + sumDigits xs
      | otherwise = x + sumDigits xs


validate :: Integer -> Bool
validate num
    | mod (sumDigits (doubleEveryOther(toDigitsRev num))) 10 == 0 = True
    | otherwise = False
