module Aula1 where

-- atalho uma linha
{-*
    várias linhas
*-}

tamanho :: [Char] -> Int 
tamanho xs = 1 + length xs

somar :: Int -> Int -> Int
somar x y = x+y

dobro :: Double -> Double
dobro x = 2*x

u :: Int
u = 7

pin :: Int -> String
pin x 
    | mod x 4 == 0 = "PIN"
    | otherwise = show x
    
{-*
    [pin x | x <- [1...40]
*-}