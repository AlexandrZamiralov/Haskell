

triplets :: [a] -> [[a]]
triplets list = zipWith3 makeTriple list (tail list) (drop 2 list)
  where makeTriple a b c = [a,b,c]



neibours :: [Integer] -> [Int]
neibours list = map snd $ filter (\(x:y:z:_, b) -> y == z - x ) $ zip (triplets list) [1..]


test1 = [
    neibours [1,3,4,7,0,7] == [1,2,4],
    neibours [2,3,5,2,11,13] == [1,4],
    neibours [-1,3,2,0,2,-7,-5] == [1,3,5],
    neibours [-1,1] == [],
    neibours [1] == [],
    neibours [] == []
  ]
