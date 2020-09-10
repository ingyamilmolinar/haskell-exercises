module Typecheck where

  data Person = Person Bool deriving Show
  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  data Mood = Blah | Woot deriving (Eq, Show)
  settleDown x = if x == Woot
                 then Blah
                 else x
  
  type Subject = String
  type Verb = String
  type Object = String

  data Sentence = 
    Sentence Subject Verb Object
    deriving (Eq, Show)
  
  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

  data Rocks = 
    Rocks String deriving (Eq, Show)

  data Yeah = 
    Yeah Bool deriving (Eq, Show)
  
  data Papu = 
    Papu Rocks Yeah
    deriving (Eq, Show)
  
  phew = Papu (Rocks "chases") (Yeah True)
  equityForAll :: Papu -> Papu -> Bool
  equityForAll p p' = p == p'

  f :: RealFrac a => a
  f = 1.0

  freud :: Ord a => a -> a
  freud x = x
  
  freud' :: a -> a
  freud' x = x

  myX = 1 :: Int
  sigmund' :: Int -> Int
  sigmund' x = myX

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk f x y = f x == y 

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith f i x = f x + (fromInteger i) 
