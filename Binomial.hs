import Data.Semigroup

{-
   A binomial tree is defined recursively as follows:

   + A binomial tree of rank 0 is a single node
   + A binomial tree of rank n, n > 0, consists of two binomial trees of
     rank n-1, with one being a child of the other.

   Note that under this definition, a binomial tree of rank n contains
   2^n nodes.
-}

-- | Here, the children binomial trees are stored in decreasing rank.
data BinomTree a = BinomTree Integer a [BinomTree a]
  deriving (Eq, Show)

instance Ord a => Semigroup (BinomTree a) where
  (<>) = mergeTrees

{-
   A binomial heap consists of a set of binomial trees, each which
   satisfies the heap invariant. There is at most 1 binomial tree of
   any given rank in a binomial heap. This allows us to store any
   integral number of nodes in our heap, since we can view each
   individual tree like a binary digit in a binary representation
   of our number of nodes.
-}

-- | Here, the children binomial trees are stored in increasing rank.
data BinomHeap a = BinomHeap [BinomTree a]
  deriving (Eq, Show)

mergeTrees :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
mergeTrees t1 t2 =
  case (t1, t2) of
    (BinomTree rank1 r1 cs1, BinomTree rank2 r2 cs2) ->
      if rank1 /= rank2 then error "can't merge binomial trees of different rank"
      else if r1 < r2 then BinomTree (rank1+1) r1 (t2:cs1)
      else BinomTree (rank2+1) r2 (t1:cs2)

singletonTree :: a -> BinomTree a
singletonTree x = BinomTree 0 x []

heapMin :: Ord a => BinomHeap a -> Maybe a
heapMin (BinomHeap []) = Nothing
heapMin (BinomHeap ts) = Just x
  where (BinomTree _rank x _, _) = removeMinTree ts

{-
Implementing lowest element removal using circular programming...
-}

removeMinTree :: Ord a => [BinomTree a] -> (BinomTree a, [BinomTree a])
removeMinTree ts = (t, ts')
  where (rank, _root, t, ts') = minerex rank ts

minRank :: Ord a => [BinomTree a] -> (Integer, a)
minRank [] = error "heap contains no trees"
minRank [BinomTree rank root _] = (rank, root)
minRank (BinomTree rank root _ : ts) =
  let (rank', root') = minRank ts in
    if root < root' then (rank, root)
    else (rank', root')

extract :: Integer -> [BinomTree a] -> (BinomTree a, [BinomTree a])
extract rank [] = error "heap contains no trees"
extract rank (t@(BinomTree rank' _ _) : ts) =
  if rank == rank' then (t, ts)
  else let (t', ts') = extract rank ts in
         (t', t:ts')

minerex :: Ord a
        => Integer
        -> [BinomTree a]
        -> (Integer, a, BinomTree a, [BinomTree a])
minerex rank [] = error "heap contains no trees"
minerex rank [t@(BinomTree rank' root _)] =
  (rank', root, t, [])
minerex rank (t@(BinomTree rank' root _) : ts) =
  (r, rt, k, ks)
  where (r, rt) = if root < rt' then (rank', root) else (r', rt')
        (k, ks) = if rank == rank' then (t, ts) else (k', t:ks')

        (r', rt', k', ks') = minerex rank ts
