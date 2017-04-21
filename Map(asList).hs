import Data.Maybe(maybeToList)
data Map key value = Empty | Map key value (Map key value)
  deriving(Show,Eq)

--mk = map key
--mv = map value
--m = children map
--k = key
--м = value

get :: Eq k => k -> Map k v -> Maybe (k,v)
get = get'
  where
    get' _ Empty = Nothing
    get' k (Map mk mv m)
      | k == mk = Just (mk, mv)
      | otherwise = get' k m

gets :: Eq k => [k] -> Map k v -> [(k, v)]
gets _ Empty = []
gets [] _ = []
gets (x:xs) y = maybeToList(get x y) ++ gets xs y

put :: Eq k => (k, v) -> Map k v -> Map k v
put = put'
  where
    put' (k, v) Empty = Map k v (Empty)
    put' (k, v) (Map mk mv m)
      | k == mk = Map mk v m
      | otherwise = Map mk mv (put (k, v) m)

puts :: Eq k => Map k v -> [(k, v)] -> Map k v
puts m []  = m
puts m p = foldr (put) m p

remove :: Eq k => k -> Map k v -> Map k v
remove = remove'
  where
    remove' _ Empty = Empty
    remove' k (Map mk mv m)
      | k == mk = m
      | otherwise = Map mk mv (remove k m)
      
removeBy :: (k -> Bool) -> Map k v -> Map k v
removeBy = removeBy'
  where
    removeBy' _ Empty = Emty
    removeBy' cond (Map mk mv m) 
      | cond mk = removeBy' cond m
      | otherwise = Map mk mv (removeBy' cond m)

keys :: Map k v -> [k]
keys = keys'
  where
    keys' Empty = []
    keys' (Map mk mv m) = mk : (keys' m)

keysDifference :: Eq k => [k] -> [k] -> [k]
keysDifference xs ys = [ y | y <- ys,notIn y xs]
  where
    notIn y [] = True
    notIn y (x:xs)
      | y == x = False
      | otherwise = notIn y xs

values :: Map k v -> [v]
values = values'
  where
    values' Empty = []
    values' (Map mk mv m) = mv : (values' m)

union :: Ord k => Map k a -> Map k a -> Map k a
union m1 Empty  = m1
union Empty m2 = m2
union m1 m2 = puts m1  ((gets (keysDifference (keys m1) (keys m2)) m2))

