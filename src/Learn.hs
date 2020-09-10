module Learn where

appendExclamation :: [Char] -> [Char]
appendExclamation message = message ++ "!"

returnFifth :: [Char] -> Char
returnFifth message = message !! 4

returnTail :: [Char] -> [Char]
returnTail message = drop 9 message

data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

h :: (Num a, Num b) => a -> b -> b
h a b = b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = a

polymorfic1 :: a -> a -> a
polymorfic1 x y = x

polymorfic2 :: a -> a -> a
polymorfic2 x y = y

