-- Lior Baruch
-- 327156998
-- Eitan Alroy
-- 316486497
--1
makeDistinctInt :: [Int] -> [Int]
makeDistinctInt numList = makeDistinctIntHelper numList []

makeDistinctIntHelper :: [Int] -> [Int] -> [Int]
makeDistinctIntHelper inputList outputList
  | null inputList = outputList
  | otherwise = 
    let listHead = head inputList
        restOfList = tail inputList
        in if listHead `elem` outputList 
        then makeDistinctIntHelper restOfList outputList 
        else makeDistinctIntHelper restOfList (outputList ++ [listHead])

--2
makeDistinctString :: String -> [Char]
makeDistinctString str = makeDistinctStringHelper str []

makeDistinctStringHelper :: [Char] -> [Char] -> [Char]
makeDistinctStringHelper inputList outputList
  | null inputList = outputList
  | otherwise = 
    let listHead = head inputList
        restOfList = tail inputList
        in if listHead `elem` outputList 
        then makeDistinctStringHelper restOfList outputList 
        else makeDistinctStringHelper restOfList (outputList ++ [listHead])

--3
multiplyLists :: [Int]->[Int] -> [(Int,Int)]
multiplyLists leftList = multiplyListsHelper leftList leftList

multiplyListsHelper :: [Int] -> [Int] -> [Int] -> [(Int,Int)]
multiplyListsHelper originalLeftList leftList rightList
  | null rightList = []
  | null leftList = multiplyListsHelper originalLeftList originalLeftList (tail rightList)
  | otherwise = (head leftList,head rightList) : 
  multiplyListsHelper originalLeftList (tail leftList) rightList

--4
listSquares :: Int -> [Int]
listSquares maxValue = listSquaresHelper maxValue 1

listSquaresHelper :: Int -> Int -> [Int]
listSquaresHelper maxValue curr
  | maxValue < curr * curr = []
  | otherwise = let sqr = curr * curr 
  in sqr : listSquaresHelper maxValue (curr + 1)

--5
roundSquare :: Int -> Int
roundSquare num = let sqroot = sqrt (fromIntegral num) in ceiling (sqroot - 0.5)

--6
twoSqaures :: Int -> (Int, Int)
twoSqaures prime = twoSqauresHelper prime 1 1

twoSqauresHelper :: Int -> Int -> Int -> (Int, Int)
twoSqauresHelper prime i j
  | i * i + j * j == prime = (j, i)
  | j * j > prime = twoSqauresHelper prime (i + 1) 1
  | i * i <= prime =  twoSqauresHelper prime i (j + 1)

--7
slowSort :: [Int] -> [Int]
slowSort [] = []
slowSort lst =
  let t1 = slowSortHelper lst
  in slowSort (init t1) ++ [last t1]
  
slowSortHelper :: [Int] -> [Int]
slowSortHelper (x:y:xs) =
  if x <= y then x:slowSortHelper (y:xs)
  else y:slowSortHelper (x:xs)
slowSortHelper xs = xs
