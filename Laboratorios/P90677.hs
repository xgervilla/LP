myIterate :: (a -> a) -> a -> [a]
myIterate op x = x : myIterate op (op x)
-- iterate func x = [x, func x, func (func x)..] -> hasta el infinito
-- recursivo

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil cond op x = if cond x then x else myUntil cond op (op x)
-- iterate hasta que se cumple cond, printea el utlimo valor
-- recursivo ->if (cond) return x else myUntil cond op (op x)

myMap :: (a -> b) -> [a] -> [b]
myMap op [] = []
myMap op (y:xs) = (op y) : (myMap op xs)
-- map f x = [f x1, f x2..]
-- sin recursividad -> ????

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond xs = []
-- filter p xs = xs con los xi que cumplen p (if p x then x:xs, else xs)

myAll :: (a -> Bool) -> [a] -> Bool
myAll cond xs = length [x | x <- xs, cond x] == length xs
-- all cond xs = True si x1,x2.. todos cumplen cond ()

myAny :: (a -> Bool) -> [a] -> Bool
myAny cond xs = length [x | x <- xs, cond x] > 0
-- any cond xs = True si x1,x2.. alguno cumple cond

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
-- zip xs ys = [(a1,b1), (a2,b2)..]
-- recursivo



myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op x [] = x
myFoldl op x (y:xs) = op y (myFoldl op x xs)
-- de izquierda a derecha: ((x op x1) op x2) op x3..
-- recursivo -> (x op elem1) op (myFoldl (x op y) xs)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op x xs = [x]
-- de derecha a izquierda: x1 op (x2 op (x3.. op (xn op x)))
-- recursivo

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op xs ys = [xs]
-- zipWith op xs ys = [b1 op c1, b2 op c2..]