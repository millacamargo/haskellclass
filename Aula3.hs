module Aula3 where 

data Status = Saudavel | Gordo | Magro
    deriving Show

imc :: Double -> Double -> Status
imc peso altura 
    | z <= 18 = Magro
    | z <= 25 = Saudavel
    | otherwise = Gordo
    where 
        z = peso / (altura*altura)




-- RECURSAO
-- UMA FUNCAO QUE CAI EM LOOP INFINITO
-- É CHAMADA DE N TERMINAL.

fat :: Integer -> Integer 
fat n 
    | n <= 1 = 1
    | otherwise = n * fat(n-1)

-- 0 1 1 2 3 5 8 13 21 34 55
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 4
fib 3 = 7
fib 4 = 8
fib 5 = 12
fib n = fib(n-1) + fib(n-2) + fib(n-3) + fib(n-4) + fib(n-5) + fib(n-6)

elimVogal :: String -> String 
elimVogal [] = []
elimVogal (x:xs)
    | elem x "AEIOUaeiou" = elimVogal xs
    | otherwise = x : elimVogal xs

somar :: Int -> Int -> Int -> Int 
somar x y z = x+y+z

