{-
-- EPITECH PROJECT, 2024
-- My.hs
-- File description:
-- Paradigms Seminar - First day of Haskell pool
-}

module My where

-- Function that increments an integer by 1
mySucc :: Int -> Int
mySucc x = x + 1

-- Function that checks if an integer is negative
myIsNeg :: Int -> Bool
myIsNeg x = x < 0

-- Function that returns the absolute value of an integer
myAbs :: Int -> Int
myAbs x
  | x < 0     = -x
  | otherwise = x

-- Function that returns the minimum of two integers
myMin :: Int -> Int -> Int
myMin x y
  | x < y     = x
  | otherwise = y

-- Function that returns the maximum of two integers
myMax :: Int -> Int -> Int
myMax x y
  | x > y     = x
  | otherwise = y

-- Function that returns a tuple of two elements
myTuple :: a -> b -> (a, b)
myTuple x y = (x, y)

-- Function that returns a tuple of three elements
myTruple :: a -> b -> c -> (a, b, c)
myTruple x y z = (x, y, z)

-- Function that returns the first element of a tuple
myFst :: (a, b) -> a
myFst (x, _) = x

-- Function that returns the second element of a tuple
mySnd :: (a, b) -> b
mySnd (_, y) = y

-- Function that swaps the elements of a tuple
mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)

-- Function that returns the first element of a list
myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x

-- Function that returns the tail of a list
myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (_:xs) = xs

-- Function that returns the length of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Function that returns the nth element of a list
myNth :: [a] -> Int -> a
myNth [] _ = error "Index out of bounds"
myNth (x:_) 0 = x
myNth (_:xs) n
  | n < 0     = error "Negative index"
  | otherwise = myNth xs (n - 1)

-- Function that returns the first n elements of a list
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs)
  | n > 0     = x : myTake (n - 1) xs
  | otherwise = error "Negative index"

-- Function that drops the first n elements of a list
myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs)
  | n > 0     = myDrop (n - 1) xs
  | otherwise = error "Negative index"

-- Function that appends two lists
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- Function that reverses a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

-- Function that returns all the elements of a list except the last one
myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit [_] = []
myInit (x:xs) = x : myInit xs

-- Function that returns the last element of a list
myLast :: [a] -> a
myLast (x:xs) = myNth (x:xs) (myLength (x:xs) - 1)

-- Function that zips two lists together
myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- Function that unzips a list of tuples
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b) : xs) = (a : x1, b : x2)
  where (x1, x2) = myUnzip xs

-- Function that maps a function to a list
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap func (x:xs) = func x : myMap func xs

-- Function that filters a list
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter func (x:xs) 
  | func x = x : myFilter func xs
  | otherwise = myFilter func xs

-- Function that folds a list from the left
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b [] = b
myFoldl func b (x:xs) = myFoldl func (func b x) xs

-- Function that folds a list from the right
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr func b (x:xs) = func x (myFoldr func b xs)

-- Function that partitions a list
myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition _ [] = ([], [])
myPartition func (x:xs)
  | func x = (x: x1, x2)
  | otherwise = (x1, x : x2)
  where (x1, x2) = myPartition func xs

-- Function that sorts a list
myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort function (x : xs) = myAppend (myQuickSort function part1)
  (x : myQuickSort function part2)
  where (part2, part1) = myPartition (function x) xs
