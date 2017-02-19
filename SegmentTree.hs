{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid
import qualified Data.Sequence as S

type Range = (Int, Int)

-- | Convention: The middle element of a range is in the _left_ split of
--               the range.
--   A range where the left is greater than the right is defined to be empty.
rangeMid :: Range -> Int
rangeMid (l, r) = (l + r) `quot` 2

data Overlap
  = Disjoint   -- ^ No overlap at all between the two ranges.
  | Contained  -- ^ First range is contained within the second.
  | Overlapped -- ^ First and second range have some overlap, but first is not contained in second.

rangeOverlap :: Range -> Range -> Overlap
rangeOverlap (l1, r1) (l2, r2)
  | l1 > r1 || l2 > r2   = Disjoint
  | l2 < l1 && r2 < l1   = Disjoint
  | l2 > r1 && r2 > r1   = Disjoint
  | l2 <= l1 && r2 >= r1 = Contained
  | otherwise            = Overlapped

-- | A segment tree allows for fast folding of an associative binary
--   operator over any range of a given input range; specifically, in
--   O(lg n) time.
--   For example, given an input list, we can figure out what the minimum
--   element in any given range of that list is, in O(lg n) time, once
--   we've constructed a segment tree.
data SegmentTree a
  = Empty
  | Node Range (SegmentTree a) a (SegmentTree a)
  deriving (Eq, Show)

segmentMin :: Monoid a => SegmentTree a -> a
segmentMin Empty = mempty
segmentMin (Node _ _ x _) = x

-- | O(n). Construct a segment tree out of the given monoid.
construct :: Monoid a => [a] -> SegmentTree a
construct l = construct' (0, S.length sl - 1)
  where -- construct' :: Monoid a => Range -> SegmentTree a
        construct' s@(l, r)
          | l > r     = Empty
          | l == r    = Node (l, r) Empty (lookup' l sl) Empty
          | otherwise =
            let m = rangeMid s
                (cl, cr) = (construct' (l, m), construct' (m+1, r))
            in Node (l, r) cl (segmentMin cl <> segmentMin cr) cr

        lookup' :: Int -> S.Seq a -> a
        lookup' n s =
          let (Just x) = S.lookup n s in x

        sl = S.fromList l

-- | O(lg n). Return the result of folding the monoid over a given
--   range. Warning: May not do reasonable things, if given a range that's
--   not a valid range of the original sequence!
query :: Monoid a => SegmentTree a -> Range -> a
query Empty _ = mempty
query (Node s1 f x b) s2 =
  case rangeOverlap s1 s2 of
    Disjoint -> mempty
    Contained -> x
    Overlapped -> query f s2 <> query b s2
