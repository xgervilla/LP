import Data.Char


-- funcion auxiliar para comprobar si el nombre es femenino
checkFemeni :: [Char] -> Bool
checkFemeni nom = ((last nom) == 'a') || ((last nom) == 'A')

main = do
    nom <- getLine      --leemos el nombre
    if(checkFemeni nom) then putStrLn "Hola maca!" else putStrLn "Hola maco!"