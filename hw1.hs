-- Lior Baruch
-- 327156998

--1
shiftString :: String -> String
shiftString str = last str : init str

-- 2
isShiftedForEquLessThanIterationTimes :: String -> String -> Int -> Bool
isShiftedForEquLessThanIterationTimes [] [] _ = True
isShiftedForEquLessThanIterationTimes str1 str2 iterationRemain = if str1 == str2 then True
else if iterationRemain <= 0 then False
else (isShiftedForEquLessThanIterationTimes str1 (shiftString str2) (iterationRemain - 1))


isShifted :: String -> String -> Bool
isShifted str1 str2 = if (length str1) /= (length str2) then False
else isShiftedForEquLessThanIterationTimes str1 str2 (length str1)

--3
stupidListOp :: [Int] -> [Int]
stupidListOp [] = []
stupidListOp (num:numList) = take num [num,num..] ++ stupidListOp numList 

--4
getNextLine :: [Int] -> [Int]
getNextLine numList
    | length numList == 0 = numList
    | length numList == 1 = []
    | otherwise = (sum (take 2 numList)) : getNextLine (tail numList)

pascalLine :: Int -> [Int]
pascalLine num
    | num < 0 = []
    | num == 0 = [1]
    | num == 1 = [1,1]
    | otherwise = let upperLine = if num < 0 then [] else pascalLine (num - 1) in [1] ++ getNextLine upperLine ++ [1]

--5.1
toList :: Integer -> [Integer]
toList 0 = []
toList num = toList (div num 10) ++ [(mod num 10)]

padWithZero :: [Integer] -> [Integer]
padWithZero integerList
    | length integerList < 9 = padWithZero (0 : integerList)
    | otherwise = integerList

toDigits :: Integer -> [Integer]
toDigits num = if num <= 0 then [] else padWithZero (toList num)

--5.2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther intList
    | length intList < 2 = intList
    | otherwise = [(intList !! 0)] ++ [2 * (intList !! 1)] ++ doubleEveryOther (drop 2 intList)

--5.3
sumDigits :: [Integer] -> Integer
sumDigits intList
    | length intList == 0 = 0
    | head intList > 9 = let firstNum = head intList in mod firstNum 10 + sumDigits((div firstNum 10) : (tail intList))
    | (head intList <= 9) && (head intList >= 0) = let firstNum = head intList in firstNum + sumDigits (tail intList)

--5.4
validate :: Integer -> Bool
validate idNum = (mod (sumDigits (doubleEveryOther (toDigits idNum))) 10) == 0
-- test print
--1 main = putStrLn (show (shiftString "abcd"))
--2 main = putStrLn (show (isShifted "123456" "61234"))
--3 main = putStrLn (show (stupidListOp [ 3, -2, 1]))
--4 main = putStrLn (show (pascalLine 5))
--5.1 main = putStrLn (show (toDigits 496351))
--5.2 main = putStrLn (show (doubleEveryOther [8, 7, 6, 5]))
--5.3 main = putStrLn (show (sumDigits [16, 7, 12, 5]))
--5.4 main = putStrLn (show (validate 58908021))