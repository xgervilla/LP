fizzBuzz :: [Either Int String]
fizzBuzz = map fb [0..]
    where fb n
                | mod n 15 == 0 = Right "FizzBuzz"
                | mod n 3 == 0 = Right "Fizz"
                | mod n 5 == 0 = Right "Buzz"
                | otherwise = Left n