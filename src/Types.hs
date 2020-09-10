module Types where

  import Data.Char
  import Lists

  data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Show)
  data Date = Date DayOfWeek Int

 -- instance Ord DayOfWeek where
 --   compare Fri Fri = EQ
 --   compare Fri _ = GT
 --   compare _ Fri = LT
 --   compare _ _ = EQ
  
 -- instance Eq DayOfWeek where
 --   (==) Mon Mon = True
 --   (==) Tue Tue = True
 --   (==) Wed Wed = True
 --   (==) Thu Thu = True
 --   (==) Fri Fri = True
 --   (==) Sat Sat = True
 --   (==) Sun Sun = True
 --   (==) _ _     = False

 -- instance Show DayOfWeek where
 --   show Mon = "Mon"
 --   show Tue = "Tue"
 --   show Wed = "Wed"
 --   show Thu = "Thu"
 --   show Fri = "Fri"
 --   show Sat = "Sat"
 --   show Sun = "Sun"

  instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
         weekday == weekday' &&
         dayOfMonth == dayOfMonth'

  instance Show Date where
    show (Date weekday dayOfMonth) =
         show weekday ++ " " ++ show dayOfMonth

  data Price = Price Integer deriving (Eq, Show)
  data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
  data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
  data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)
  
  isCar :: Vehicle -> Bool
  isCar (Car _ _) = True
  isCar _ = False

  isPlane :: Vehicle -> Bool
  isPlane (Plane _) = True
  isPlane _ = False
  
  areCars :: [Vehicle] -> [Bool]
  areCars = map isCar

  getManu :: Vehicle -> Manufacturer
  getManu (Car m p) = m

  data GuessWhat = Chickenbutt deriving (Eq, Show)
  data Id a = MkId a deriving (Eq, Show)
  data Product a b = Product a b deriving (Eq, Show)
  data Sum a b = First a | Second b deriving (Eq, Show)
  data RecordProduct a b = RecordProduct { pfirst :: a,
                                           psecond :: b }
                                           deriving (Eq, Show)

  data OperatingSystem = Linux
                       | BSD
                       | Mac
                       | Windows
                       deriving (Eq, Show)

  data ProgLang = Haskell
                | Agda
                | Idris
                | PureScript
                deriving (Eq, Show)

  data Programmer = Programmer { os :: OperatingSystem,
                                 lang :: ProgLang }
                  deriving (Eq, Show)

  allOperatingSystems :: [OperatingSystem]
  allOperatingSystems = [ Linux, BSD, Mac, Windows ]
  
  allLanguages :: [ProgLang]
  allLanguages = [ Haskell, Agda, Idris, PureScript ]

  allProgrammers :: [Programmer]
  allProgrammers = generateAllProgrammers allOperatingSystems allLanguages []
    where
      generateAllProgrammers (os:oss) (lang:langs) l 
        | langs /= [] = generateAllProgrammers (os:oss) langs (l ++ [ Programmer { os = os, lang = lang } ])
        | oss /= []   = generateAllProgrammers oss allLanguages (l ++ [ Programmer { os = os, lang = lang } ])
        | oss == []   = l ++ [ Programmer { os = os, lang = lang } ]

  newtype Name  = Name String deriving Show
  newtype Acres = Acres Int deriving Show

  data FarmerType = DairyFarmer | WheatFarmer | SoyBeanFarmer deriving Show
  data Farmer = Farmer Name Acres FarmerType deriving Show
  
  isDairyFarmer :: Farmer -> Bool
  isDairyFarmer (Farmer _ _ DairyFarmer) = True
  isDairyFarmer _ = False

  data FarmerRec = FarmerRec { name       :: Name,
                               acres      :: Acres,
                               farmerType :: FarmerType }
                 deriving Show

  isDairyFarmerRec :: FarmerRec -> Bool
  isDairyFarmerRec farmer = 
    case farmerType farmer of
      DairyFarmer -> True
      _           -> False

  data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

  insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
  insert' b Leaf = Node Leaf b Leaf
  insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)

  mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

  preorder :: Eq a => BinaryTree a -> [a]
  preorder Leaf = []
  preorder (Node left a right) = [a] ++ (traverse left []) ++ (traverse right [])
    where
      traverse (Node left a right) l
        | left /= Leaf && right /= Leaf = [a] ++ traverse left l ++ traverse right l
        | left == Leaf && right /= Leaf = [a] ++ traverse right l
        | left /= Leaf && right == Leaf = [a] ++ traverse left l
        | otherwise                     = [a]

  inorder :: Eq a => BinaryTree a -> [a]
  inorder Leaf = []
  inorder (Node left a right) = (traverse left []) ++ [a] ++ (traverse right [])
    where
      traverse (Node left a right) l
        | left /= Leaf && right /= Leaf = traverse left l ++ [a] ++ traverse right l
        | left == Leaf && right /= Leaf = [a] ++ traverse right l
        | left /= Leaf && right == Leaf = traverse left l ++ [a]
        | otherwise                     = [a]

  postorder :: Eq a => BinaryTree a -> [a]
  postorder Leaf = []
  postorder (Node left a right) = (traverse left []) ++ (traverse right []) ++ [a]
    where
      traverse (Node left a right) l
        | left /= Leaf && right /= Leaf = traverse left l ++ traverse right l ++ [a]
        | left == Leaf && right /= Leaf = traverse right l ++ [a]
        | left /= Leaf && right == Leaf = traverse left l ++ [a]
        | otherwise                     = [a]

  foldTree :: Eq a => (a -> b -> b) -> b -> BinaryTree a -> b
  foldTree f b t = foldr f b (inorder t)

  vigenereCipher :: [Char] -> [Char] -> [Char]
  vigenereCipher plainText key = doCipher plainText key []
    where
      doCipher (pt:pts) (k:ks) l
        | pt == ' ' = doCipher (pts) (k:ks) (l ++ [pt])
        | ks == []  = doCipher (pts) key (l ++ [ cipherLetter pt k ]) 
        | pts /= [] = doCipher (pts) (ks) (l ++ [ cipherLetter pt k ])
        | otherwise = (l ++ [ cipherLetter pt k ])
        where 
          cipherLetter :: Char -> Char -> Char
          cipherLetter letter key
            | letter >= 'A' && letter <= 'Z' = chr (ord 'A' + (mod ((+) (ord letter - ord 'A') (ord key - ord 'A')) (ord 'Z' - ord 'A' + 1)))
            | letter >= 'a' && letter <= 'z' = chr (ord 'a' + (mod ((+) (ord letter - ord 'a') (ord key - ord 'a')) (ord 'z' - ord 'a' + 1)))

  isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
  isSubseqOf [] _ = True
  isSubseqOf _ [] = False
  isSubseqOf list1@(l1:l1s) (l2:l2s)
    | l1 == l2    = isSubseqOf l1s l2s
    | l1 /= l2    = isSubseqOf list1 l2s

  capitalizeWords :: String -> [(String, String)]
  capitalizeWords word = doCapitalizeWords (split word ' ') []
    where
      doCapitalizeWords :: [String] -> [(String, String)] -> [(String, String)]
      doCapitalizeWords words@(w:ws) list = doCapitalizeWords ws (list ++ [(w, ([toUpper (w !! 0)] ++ tail w))])
      doCapitalizeWords [] list = list
    
  capitalizeWord :: String -> String
  capitalizeWord word = [toUpper (word !! 0)] ++ tail word

  capitalizeParagraph :: String -> String
  capitalizeParagraph [] = []
  capitalizeParagraph (p:ps) = doCapitalizeParagraph ps [ toUpper p ]
    where
      doCapitalizeParagraph [] list = list
      doCapitalizeParagraph (p1:ps) list
        | p1 == '.' && length ps >= 2 && (ps !! 0) == ' ' = doCapitalizeParagraph (tail (tail ps)) (list ++ ". " ++ [(toUpper (ps !! 1))])
        | otherwise                                       = doCapitalizeParagraph ps (list ++ [ p1 ])

  telephoneMap = [ " ", "", "ABC", "DEF", "GHI", "JKL", "MNO", "PQRS", "TUV", "WXYZ" ]
  translatePhoneCommands :: String -> String
  translatePhoneCommands commands = doTranslation commands 0 []
    where
      doTranslation [] _ list = list
      doTranslation commands@(c:cs) count list
        | ord c - ord '0' > 9 || ord c - ord '0' < 0 = error "Invalid input"
        | cs /= [] && c == (cs !! 0)                 = doTranslation cs (count+1) list
        | cs == []                                   = list ++ [translateCommand c count]
        | otherwise                                  = doTranslation cs 0 $ list ++ [translateCommand c count]
        where
          translateCommand c count = phoneMapping !! (mod count (length phoneMapping))
            where
              phoneMapping = telephoneMap !! (ord c - ord '0')
 
  data Expr = Lit Integer | Add Expr Expr
  
  eval :: Expr -> Integer
  eval (Lit i) = i
  eval (Add e1 e2) = eval e1 + eval e2

  printExpr :: Expr -> String
  printExpr (Lit i) = show i
  printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
