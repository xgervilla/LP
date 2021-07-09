eql :: [Int] -> [Int] -> Bool
eql xs [] = False
eql [] ys = False
eql xs ys = if length xs /= length ys || length ([x | x <- (zip xs ys), fst x /= snd x])>0 then False else True

prod :: [Int]->Int
prod [] = 0
prod (x:xs) = foldl (*) x xs

prodOfEvens :: [Int] -> Int
prodOfEvens [] = 0
prodOfEvens xs = prod (filter even xs)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct [] [] = 0
scalarProduct xs ys = sum [(fst x)*(snd x) | x <- (zip xs ys)]