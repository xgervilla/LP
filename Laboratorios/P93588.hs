myMap :: (a->b) -> [a] -> [b]
myMap op xs = [op x | x<- xs]

myFilter :: (a->Bool)->[a]->[a]
myFilter cond xs = [x | x<-xs, cond x]

myZipWith :: (a->b->c) -> [a]->[b]->[c]
myZipWith op xs ys = [op (fst x) (snd x)| x <-(zip xs ys)]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify xs ys = [(x,y) | x<-xs, y<-ys, mod x y == 0]

factors :: Int -> [Int]
factors n = [x | x<- [1..n], mod n x == 0]

filterFoldl :: (Int->Bool) -> (Int -> Int -> Int) -> [Int]
filterFoldl cond op x0 xs = foldl op x0 (filter cond xs)