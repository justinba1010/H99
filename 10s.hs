-- 1
myLast :: [a] -> a

myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: Integral a => [b] -> a -> b
elementAt (x:xs) 0 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- 4
myLength :: Integral a => [b] -> a
myLength = foldl (\acc x -> acc + 1) 0 

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> (x:acc)) []

-- 6
isPalindrome x = x == (myReverse (x))

-- 7
flatten :: (NestedList a) -> [a]
data NestedList a = Elem a | List [NestedList a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List xs) = foldl (\acc x -> acc ++ flatten x) [] xs

-- 8
compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y = compress (x:xs)
  | otherwise = x:(compress (y:xs))

-- 9

