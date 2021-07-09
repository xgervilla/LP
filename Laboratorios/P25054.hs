
myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1+ myLength xs
--si esta vacio then 0, sino 1 + length de lo que falta

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = max x (myMaximum xs)
--si esta vacio then 0, sino max (actual, lo que falta)

buildPalindrom :: [Int] -> [Int]
buildPalindrom [] = []
buildPalindrom xs = x2++xs
      where x2 = reverse xs
--if xs vacio then [] else x2++xs where x2= reverse xs


flatten :: [[Int]] -> [Int]
flatten [[]] = []
flatten xs = concat xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens xs = (pars, impars)
     where
        pars = [x | x <- xs, even x]
        impars = [x | x <- xs, odd x]
--pars: x que pertenecen a xs tal que x par (even)
--impars: x que pertenecen a xs tal que x impar (odd)

remove :: [Int] -> [Int] -> [Int]
remove [] [] = []
remove xs xy = [x | x <- xs, not (elem x xy)]

primeDivisors :: Int -> [Int]
primeDivisors 0 = []
primeDivisors m = [y | y<- [2,3..m], (rem m y)== 0, isPrime y]
       where isPrime 1 = True
             isPrime 2 = True
             isPrime n
                  | (length [x | x <- 2:[3,5..ceiling (sqrt (fromIntegral n))], (mod n x) == 0])>0 = False
                  | otherwise = True

average :: [Int] -> Float
average [] = 0
average xs = avSum / avLeng
      where avSum = fromIntegral (sum xs) :: Float
            avLeng = fromIntegral (length xs) :: Float
--if xs vacio then 0 else (suma)/length xs 
