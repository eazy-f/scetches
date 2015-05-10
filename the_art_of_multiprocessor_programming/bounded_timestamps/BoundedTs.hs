module BoundedTs where

data Element = E0 | E1 | E2 deriving (Eq, Show)
data BoundedTree = Nil |
                   BoundedTree Element BoundedTree BoundedTree
                               deriving Show

data Zipper a = ZipperLeft a | ZipperRight a

type BinTreeZip a = (a, a)

instance Ord Element where
  (<) E0 E1 = True
  (<) E1 E2 = True
  (<) E2 E0 = True

newtype SndFunctor a b = SndFunctor { sndFunctorTuple :: (a, b) } deriving Show
newtype BoundedTimestamp = BoundedTimestamp { timestamp :: [Element] } deriving Show

instance Bounded BoundedTimestamp where
  minBound = BoundedTimestamp {timestamp = []}
  maxBound = BoundedTimestamp {timestamp = []}

instance (Bounded a) => Monad (SndFunctor a) where
  return b = SndFunctor { sndFunctorTuple = (minBound, b) }
  (>>=) res f = f $ snd $ sndFunctorTuple res

higher E0 = E1
higher E1 = E2
higher E2 = E0

element (ZipperLeft  e) = e
element (ZipperRight e) = e 

updateElement element (BoundedTree _ left right) = 
    BoundedTree element left right

zero = empty E0
empty element = BoundedTree element Nil Nil

nextElement Nil = E0
nextElement (BoundedTree element _ _) = higher element

highest n tree0 = SndFunctor { sndFunctorTuple = (num, tree) }
    where
      unzipped = highestZip (n - 1) tree0 []
      tree = treeZip unzipped
      num = BoundedTimestamp { timestamp = reverse [ element move | (_, move) <- unzipped ] }

highestZip 0 tree zipper = zipper
highestZip n tree zipper = highestZip (n - 1) nextTree $ (parent, move) : zipper
    where
      (nextTree, move) =
          case tree of
            Nil ->
                (Nil, ZipperLeft E0)
            BoundedTree _ Nil right ->
                (Nil, ZipperLeft $ nextElement right)
            BoundedTree _ left Nil ->
                (Nil, ZipperRight $ nextElement left)
            BoundedTree _ left right ->
                case (left, right) of
                  (BoundedTree l _ _, BoundedTree r _ _)
                                   | l < r ->
                                       (right, ZipperRight r)
                                   | otherwise ->
                                       (left, ZipperLeft l)
      parent = case tree of
                 Nil -> zero
                 _   -> tree
      
treeZip unzipped = foldl treeZipFolder zero unzipped

treeZipFolder child0 (parent, move) = 
    case move of
      ZipperLeft _ ->
          BoundedTree pElement child pRight
      ZipperRight _ ->
          BoundedTree pElement pLeft child
    where
      BoundedTree pElement pLeft pRight = parent
      child = updateElement (element move) child0
