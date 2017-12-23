toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0     = let d = x `div` 10
                    m = x `mod` 10
                in  m : toDigitsRev d
  | otherwise = []


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x =
  let len = length x
      mult = if len `mod` 2 == 0 then [2,1] else [1,2]
      y = zip x (cycle mult)
  in  [a*b | (a,b) <- y]

sumDigits ::  [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigitsRev) 0

validate :: Integer -> Bool
validate = (==) 0 . (flip mod 10) .  sumDigits . doubleEveryOther . toDigits

{- The Towers of Hanoi -}
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to proxy = [(from, to)]
hanoi n from to proxy = (hanoi (n-1) from proxy to) ++ (hanoi 1 from to proxy) ++ (hanoi (n-1) proxy to from)
