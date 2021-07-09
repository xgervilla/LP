import Data.Char

flatten :: [[Int]]->[Int]
flatten [[]] = []
flatten s = foldl (++) [] s
--sumamos un vector vacio [] a los diferentes vectores para concatenar, se acumulan los diferentes vectores

myLength :: String -> Int
myLength "" = 0
myLength s = foldl (\x _ -> x+1) 0 s
--por cada char de s aumentamos el contador (desde 0)

myReverse :: [Int]-> [Int]
myReverse [] = []
myReverse l = foldl (flip(:)) [] l
--acumulamos cada elemento x a xs (se acumula de derecha a izquierda, l queda revertida)
-- l = foldl (flip(:)) [] l

countIn :: [[Int]] -> Int -> [Int]
countIn [[]] _ = []
countIn xs n = map count xs
     where count = length . (filter (==n))
--aplicamos count a cada sub vector
--count: filtramos el sub vector segun ==n y contamos la longitud del vector

firstWord :: String -> String
firstWord "" = ""
firstWord s = takeWhile (not.isSpace) (dropWhile (isSpace) s)
--quitamos los espacios en blanco del principio
--sacamos los que van despues