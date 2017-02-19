{-# LANGUAGE ScopedTypeVariables #-}

import Data.Semigroup
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
  = Leaf Range a
  | Node Range (SegmentTree a) a (SegmentTree a)
  deriving (Eq, Show)

segmentMin :: Semigroup a => SegmentTree a -> a
segmentMin (Leaf _ x) = x
segmentMin (Node _ _ x _) = x

-- | O(n). Construct a segment tree out of the given monoid.
--   List of elements must be nonempty.
construct :: forall a. Semigroup a => [a] -> SegmentTree a
construct xs = construct' (0, length sx - 1)
  where construct' :: Semigroup a => Range -> SegmentTree a
        construct' s@(l, r)
          | l > r = undefined  -- should never happen if input is nonempty
          | l == r = Leaf s (sx ! l)
          | otherwise =
            let m = rangeMid s
                (f, b) = (construct' (l, m), construct' (m+1, r))
            in Node s f (segmentMin f <> segmentMin b) b

        sx :: S.Seq a
        sx = S.fromList xs

        (!) :: S.Seq a -> Int -> a
        s ! i = (\(Just x) -> x) $ S.lookup i s

-- | O(lg n). Query a range of the original sequence to find the result
--   of folding the semigroup's operator over that given range.
query :: Semigroup a => SegmentTree a -> Range -> a
query t s = (\(Just x) -> x) $ query' t s
  where query' :: Semigroup a => SegmentTree a -> Range -> Maybe a
        query' (Leaf s1 x) s2 =
          case rangeOverlap s1 s2 of
            Disjoint -> Nothing
            Contained -> Just x
        query' (Node s1 f x b) s2 =
          case rangeOverlap s1 s2 of
            Disjoint -> Nothing
            Contained -> Just x
            Overlapped -> query' f s2 <> query' b s2

{-
   To see the correctness of the following definition of toList, consider
   this easier-to-understand, but inefficient version of toList:

   > toList :: SegmentTree a -> [a]
   > toList (Leaf _ x) = [x]
   > toList (Node _ f _ b) = toList f ++ toList b

   This is obviously correct, but O(n^2). So we want to
   optimize this. Can we add an accumulator? Let's define an
   auxilliary function which can take another list as a parameter.

   > toList' :: SegmentTree a -> [a] -> [a]
   > toList' t l = toList t ++ l

   We can plainly see that toList t = toList' t [].

   Now, by expanding toList' into its two cases based on whether or not
   the tree is a leaf or a node, and applying fold-unfold to remove the
   instances of toList within the definitions:

   toList' (Leaf _ x) l = toList (Leaf _ x) ++ l
                        = [x] ++ l
                        = x : l

   toList' (Node _ f _ b) l = toList (Node _ f _ b) ++ l
                            = toList f ++ toList b ++ l
                            = toList f ++ (toList' b l)
                            = toList' f (toList' b l)

   We're simply substituting the calls to toList with its conditional
   definitions, and then substituting calls to toList to calls to toList'
   by the simple definition of toList' above. Both of these are valid
   substitutions, since we're in a purely functional language.

   Therefore our new definition of toList below is equivalent to the naive
   version. QED

   What's the running time of toList'? Let T(n) be the time taken to run
   toList' on a segment tree with n nodes.

     T(n) = 2T(n/2) + O(1),

   since we know that the segment tree is balanced because we constructed it
   that way. Well, Master theorem away, and we get T(n) = O(n). QED
-}

-- | O(n). Get back the original elements of the sequence used to
--   construct the segment tree.
toList :: SegmentTree a -> [a]
toList t = toList' t []
  where toList' :: SegmentTree a -> [a] -> [a]
        toList' (Leaf _ x) l = x : l
        toList' (Node _ f _ b) l = toList' f $ toList' b l

-- | O(lg n). Update an element of the original sequence and return
--   the augmented segment tree.
--   Consequences are undefined if the given index is outside the range of
--   the original sequence.
update :: forall a. Semigroup a => Int -> a -> SegmentTree a -> SegmentTree a
update i x t = update' t
  where update' :: Semigroup a => SegmentTree a -> SegmentTree a
        update' (Leaf s _) = Leaf s x
        update' (Node s f y b) =
          let (f', b') = if i <= rangeMid s then (update' f, b)
                         else (f, update' b)
          in Node s f' (segmentMin f' <> segmentMin b') b'
