module Lists where

import Data.Time

eftBool :: Bool -> Bool -> [Bool]
eftBool a b 
    | a > b     = []
    | a == b    = [a]
    | otherwise = [a, succ a]

eftChar :: Char -> Char -> [Char]
eftChar a b
    | a > b     = []
    | a == b    = [a]
    | otherwise = [a] ++ eftChar (succ a) b

split :: String -> Char -> [String]
split text char = splittedText
    where
        splittedText
            | text /= "" = [ firstSubstring ] ++ split remains char
            | otherwise  = []
        firstSubstring = takeWhile (/=char) text
        remains        = drop ((length firstSubstring) + 1) text

mySqr = [ x^2 | x <- [1..5] ]
myCube = [ y^3 | y <- [1..5] ]
myTuples = [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]

multiplesOf divisor = filter (\x -> mod x divisor == 0)
removeArticles sentence = filter (\x -> x /= "an" && x /= "a" && x/= "the") (split sentence ' ')

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys
myZipWith _ _ []          = []
myZipWith _ [] _          = []

myZip :: [a] -> [b] -> [(a,b)]
myZip x y = myZipWith (\x -> \y -> (x,y)) x y

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ 
      DbDate (UTCTime (fromGregorian 1911 5 1)
                      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, World!"
    , DbDate (UTCTime (fromGregorian 1921 5 1)
                      (secondsToDiffTime 34123))
    , DbNumber 8999
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = map (\(DbDate x) -> x) (filter isDbDate items)
    where
        isDbDate (DbDate _) = True
        isDbDate _          = False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = map (\(DbNumber x) -> x) (filter isDbNumber items)
    where
        isDbNumber (DbNumber _) = True
        isDbNumber _            = False 

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = maximum $ filterDbDate items

sumDb :: [DatabaseItem] -> Integer
sumDb items = sum $ filterDbNumber items

avgDb :: [DatabaseItem] -> Double
avgDb items = fromIntegral (div (sumDb items) (toInteger (length (filterDbNumber items))))

stops = "pbtdkg"
vowels = "aeiou"
makeTuples :: Eq a => [a] -> [a] -> [(a, a, a)]
makeTuples (s:ss) (v:vs) = doMakeTuples [] (s:ss) (v:vs) (s:ss)
    where
        doMakeTuples l (s1:s1s) (v1:v1s) (s2:s2s)
            | s1s /= [] = [(s1,v1,s2)] ++ doMakeTuples l s1s (v1:v1s) (s2:s2s)
            | v1s /= [] = [(s1,v1,s2)] ++ doMakeTuples l (s:ss) v1s (s2:s2s)
            | s2s /= [] = [(s1,v1,s2)] ++ doMakeTuples l (s:ss) (v:vs) s2s
            | otherwise = [(s1,v1,s2)] ++ l

makeTuplesStartingWith :: [Char] -> [Char] -> Char -> [(Char, Char, Char)]
makeTuplesStartingWith s v c = filter (\(x, y, z) -> x == c) (makeTuples s v)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = foldr (||) False (map f l)

myElem :: Eq a => a -> [a] -> Bool
myElem e l = foldr (||) False $ map (\x -> x == e) l

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e = any (\x -> x == e)

myReverse :: Eq a => [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> [(f x)] ++ y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr doFilter []
  where
    doFilter x l
      | f x       = [x] ++ l
      | otherwise = l

squish :: [[a]] -> [a]
squish = foldr (\x y -> x ++ y) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

