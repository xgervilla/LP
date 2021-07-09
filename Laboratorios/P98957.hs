ones :: [Integer]
ones = repeat 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = [0] ++ recInts 1
    where recInts i = [i,-i] ++ (recInts (i+1))

triangulars :: [Integer]
triangulars = [0,1] ++ recTri 2 1
    where recTri x y = [x+y] ++ (recTri (x+1) (x+y))

factorials :: [Integer]
factorials = scanl (*) 1 (iterate (+1) 1)

fibs :: [Integer]
fibs = [0,1] ++ recFib 0 1
     where recFib x y = [x+y] ++ (recFib y (x+y))

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n 
    | (length [x | x <- 2:[3,5..ceiling (sqrt (fromIntegral n))], (mod n x) == 0])>0 = False
    | otherwise = True

primes :: [Integer]
primes = [x | x<- (iterate (+1) 2), isPrime x]

hammings :: [Integer]
hammings = [x | x<- (iterate (+1) 1), length (filter (/=2) (filter (/=3) (filter (/=5) (primeDiv x)))) == 0]
    where primeDiv m = [y | y<- [2,3..m], mod m y == 0, isPrime y]

hamming2 = 1: merge3 (map (*2) hamming2) (map (*3) hamming2) (map (*5) hamming2)
    where merge3 xs ys zs = merge xs (merge ys zs)
          merge [] xs = xs
          merge xs [] = xs
          merge (x:xs) (y:ys) = if (x<y) then x:merge xs (y:ys) else if(y>x) then y:merge ys x(x:xs) else x : merge xs ys

lookNsay :: [Integer]
lookNsay = iterate recLNS 1
   where recLNS n = read (calculaCont (show n))

calculaCont [] = []
calculaCont (num:s) = (show long) ++ [num] ++ calculaCont left
      where long = length (takeWhile (==num) s)
            left = dropWhile (==num) s
-- 1, un uno (11), dos unos(21), un dos y un uno (1211), un uno un dos y dos unos (111221)..
-- contador del numero, numero (until o similar)
-- int x = 1; if((cin >> y)==x) ++cont; else [x++cont]
-- show x (pasa x a string) ; read x (pasa de x string al tipo que sea (int))

tartaglia :: [[Integer]]
tartaglia = [[]]