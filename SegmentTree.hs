import Data.Semigroup

{-
   A range tree is a data structure which augments a sequence.
   It allows querying the results of an associative binary operator,
   folded across all elements in a range of the original sequence,
   in O(lg n) time. For example, after constructing a range tree, the
   minimum element in any range of the original sequence can be found
   in O(lg n) time.
-}

type Range = (Int, Int)

data RangeTree a = Leaf Range a | Node Range a (RangeTree a) (RangeTree a)
  deriving (Eq, Show)

nodeMin :: RangeTree a -> a
nodeMin (Leaf _ a) = a
nodeMin (Node _ a _ _) = a

construct :: Semigroup a => [a] -> RangeTree a
construct [] = error "cannot construct range tree from empty list"
construct l = construct' l (0, length l - 1)
  where construct' [a] range = Leaf range a
        construct' l range@(bl, br) =
          let m = length l `quot` 2
              (l', l'') = splitAt m l
              left = construct' l' (bl, bl + m - 1)
              right = construct' l'' (bl + m, br)
          in Node range (nodeMin left <> nodeMin right) left right
