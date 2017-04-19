data BinTree e = BinTree e [BinTree e]
  deriving(Show)

type BinHeap e = [BinTree e]

add :: Ord e => e -> BinHeap e -> BinHeap e
add a b = (singletonHeap a) `unionBinHeaps` b

fromList :: Ord e => [e] -> BinHeap e
fromList p = foldr (add) [] p

singletonHeap :: e -> BinHeap e
singletonHeap a = [BinTree a []]

degree :: BinTree e-> Int
degree (BinTree _ as) = length as

merge :: Ord e => BinHeap e -> BinHeap e -> BinHeap e
merge x y = mergeT $ mergeDegree x y

mergeDegree :: BinHeap e -> BinHeap e -> BinHeap e
mergeDegree [] yy = yy
mergeDegree xx [] = xx
mergeDegree (x:xs) (y:ys)
  | degree x >= degree y    = y:mergeDegree (x:xs) ys
  | otherwise = x:mergeDegree xs (y:ys)

mergeT :: Ord e => BinHeap e -> BinHeap e
mergeT xx =
  case xx of
    (x:x':xs) ->
      if (degree x) == (degree x') && (null xs || (degree x') /= (degree (head xs)))
        then mergeT ((mergeT' x x'):xs)
        else x:mergeT (x':xs)
    x -> x

mergeT' :: Ord e => BinTree e -> BinTree e -> BinTree e
mergeT' xx@(BinTree x xs) yy@(BinTree y ys)
  | x > y = mergeT' yy xx
  | otherwise = (BinTree x (yy:xs))
