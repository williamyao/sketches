data AST = Symbol String | Node [AST] | Empty
  deriving (Show)

gobble :: ([a] -> (Maybe b, [a])) -> [a] -> ([b], [a])
gobble _ [] = ([], [])
gobble f xs =
  case res of
    Nothing  -> ([], xs')
    (Just x) -> let (x', xs'') = gobble f xs' in (x:x', xs'')
  where (res, xs') = f xs

parse' :: [String] -> (Maybe AST, [String])
parse' [] = error "End of file while parsing."
parse' (")":t) = (Nothing, t)
parse' ("(":t) = (Just $ Node p, t')
  where (p, t') = gobble parse' t
parse' (t:ts) = (Just $ Symbol t, ts)

test :: [String]
test = ["(", "+", "5", "(", "-", "var", "6", ")", ")"]
