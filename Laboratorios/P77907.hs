
absValue :: Int -> Int
absValue n = if n>0 then n else -n

power :: Int -> Int-> Int
power x 0 = 1
power x y = x * (power x (y-1))

isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n 
     | (length [x | x <- 2:[3,5..ceiling (sqrt (fromIntegral n))], (mod n x) == 0])>0 = False
     | otherwise = True
-- (length [x | x <- 2:[3,5..(n-1)], (mod n x) == 0])>0 = False

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n-1)+slowFib(n-2)
--fibonacci "basico"

quickFib :: Int -> Int
quickFib 0 = 0
quickFib 1 = 1
quickFib n = quickFibAux (n-2) 0 1
      where quickFibAux 0 prev act = prev+act
            quickFibAux m prev act = quickFibAux (m-1) act (prev+act)
--llamamos a una funcion auxiliar con 3 parametros (n, valor previo, valor actual) y un Int de retorno (fibonacci n), asi evitamos calculos repetidos al llamar a la recursiva con la suma ya calculada