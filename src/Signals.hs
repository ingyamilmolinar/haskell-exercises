module Signals where
  isJust :: Maybe a -> Bool
  isJust (Just a) = True
  isJust Nothing = False
  
  isNothing :: Maybe a -> Bool
  isNothing a = not $ isJust a

  mayybee :: b -> (a -> b) -> Maybe a -> b
  mayybee b f Nothing = b
  mayybee b f (Just a) = f a

  fromMaybe :: a -> Maybe a -> a
  fromMaybe a Nothing = a
  fromMaybe a (Just b) = b

  listToMaybe :: [a] -> Maybe a
  listToMaybe [] = Nothing
  listToMaybe (l:ls) = Just l

  maybeToList :: Maybe a -> [a]
  maybeToList Nothing = []
  maybeToList (Just a) = [a]

  catMaybes :: [Maybe a] -> [a]
  catMaybes [] = []
  catMaybes l = map (\(Just a) -> a) (filter (\x -> isJust x) l)

  flipMaybe :: [Maybe a] -> Maybe [a]
  flipMaybe l = doFlip l (Just [])
    where
      doFlip :: [Maybe a] -> Maybe [a] -> Maybe [a]
      doFlip [] o = o
      doFlip (Nothing:_) o = Nothing
      doFlip ((Just i):is) (Just o) = doFlip is (Just (o ++ [i]))
      doFlip _ Nothing = Nothing -- This will never happen

  lefts' :: [Either a b] -> [a] -- without foldr
  lefts' [] = []
  lefts' l = map unpackageLeft (filter (\e -> isLeft e) l)
    where
      unpackageLeft (Left a) = a
      unpackageLeft (Right _) = error "Right value passed" -- This should never happen
      isLeft (Left _) = True
      isLeft (Right _) = False

  leftsWithFold' :: [Either a b] -> [a]
  leftsWithFold' [] = []
  leftsWithFold' l = foldr unpackageLeft [] l     
    where
      unpackageLeft (Left a) l = [a] ++ l
      unpackageLeft (Right a) l = [] ++ l

  rightsWithFold' :: [Either a b] -> [b]
  rightsWithFold' [] = []
  rightsWithFold' l = foldr unpackageRight [] l     
    where
      unpackageRight (Left a) l = [] ++ l
      unpackageRight (Right a) l = [a] ++ l
 
  partitionEithers' :: [Either a b] -> ([a], [b]) 
  partitionEithers' l = foldr unpackageInTuple ([],[]) l
    where
      unpackageInTuple (Left a) (la,lb) = ([a]++la,lb)
      unpackageInTuple (Right b) (la,lb) = (la,[b]++lb)

  eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe' f (Left a) = Nothing
  eitherMaybe' f (Right b) = Just (f b)

  either' :: (a -> c) -> (b -> c) -> Either a b -> c
  either' fa fb (Left a) = fa a
  either' fa fb (Right b) = fb b

  myIterate :: (a -> a) -> a -> [a]
  myIterate f x = [x] ++ myIterate f (f x)

  myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  myUnfoldr f b = doUnfoldr f (f b) []
    where
      doUnfoldr f (Just (a, b)) l = [a] ++ doUnfoldr f (f b) l
      doUnfoldr f Nothing l       = l

  betterIterate :: (a -> a) -> a -> [a]
  betterIterate f a = myUnfoldr (\a -> Just (a, f a)) a

  data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

  unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
  unfold f a = 
