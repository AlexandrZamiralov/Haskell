data BinTree e = BinTree e [BinTree e]
  deriving(Show)

type BinHeap e = [BinTree e]

add :: Ord e => e -> BinHeap e -> BinHeap e
add a b = (singletonHeap a) `unionBinHeaps` b

replace :: Ord e => e -> BinHeap e -> BinHeap e
replace _ [] = []
replace v [x] = [replaceT v x]
replace v ((BinTree k next):(BinTree k1 next1):xs)
  | k < k1 = (BinTree k1 next1):(replace v ((BinTree k next):xs))
  | otherwise = (BinTree k next):(replace v ((BinTree k1 next1):xs))

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
      if (degree x) == (degree x')
        then mergeT ((mergeT' x x'):xs)
        else x:mergeT (x':xs)
    x -> x

mergeT' :: Ord e => BinTree e -> BinTree e -> BinTree e
mergeT' xx@(BinTree x xs) yy@(BinTree y ys)
  | x > y = (BinTree y (xx:ys))
  | otherwise = (BinTree x (yy:xs))
  
replaceT :: Ord e => e -> BinTree e -> BinTree e
replaceT v (BinTree e next)
  | v <= e = BinTree v next
  | otherwise = balanceT (BinTree v next)
  
balanceT :: Ord e => BinTree e -> BinTree e
balanceT = balanceT'
  where
    balanceT' (BinTree e []) = BinTree e []
    balanceT' b@(BinTree v [(BinTree e next)])
      | v <= e = b
      | otherwise = (BinTree e [balanceT' (BinTree v next)])
    balanceT' b@(BinTree v ((BinTree e next):(BinTree e1 next1):xs))
      | v <= (min e e1) = b
      | e < v = (BinTree e ((balanceT' (BinTree v next)):(BinTree e1 next1):xs))
      | otherwise = (BinTree e1 ((BinTree e next):(balanceT'(BinTree v next1)):xs))
