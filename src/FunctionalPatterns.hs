module FunctionalPatterns where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigitWithDivMod :: Integral a => a -> a
tensDigitWithDivMod x = tens
  where (div, _)   = divMod x 10
        (_, tens)  = divMod div 10

nDigit :: Integral a => a -> a -> a
nDigit x n = digit
  where (div, _)   = divMod x (10^n)
        (_, digit) = divMod div 10

foldBoolMatch :: a -> a -> Bool -> a
foldBoolMatch x _ False = x
foldBoolMatch _ y True  = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b = 
  case b of
    True -> x
    False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
  | b = x
  | otherwise = y
  
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = ((f a), c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show
