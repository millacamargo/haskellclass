module Aula1-1 where 

-- TUPLA: UMA ESTRUTURA QUE CARREGA
-- VÁRIOS TIPOS AO MESMO E DE TAMANHO
-- FIXO (FINITO).

-- LISTA: CONTEINER, POSSIVELMENTE
-- INFINITO, DE ELEMENTOS DO MESMO
-- TIPO.

-- LIST COMPREHENSIONS:
-- GERAR LISTAS A PARTIR DE
-- UMA EXPRESSAO, ITERACOES E
-- FILTROS.

divisores :: Int -> [Int]
divisores n = 
    [x | x <- [1..(n-1)], mod n x == 0]

ehPrimo :: Int -> Bool
ehPrimo n = length (divisores n) == 1

l1 :: [Int]
l1 = [1..200]

tamanho :: [Char] -> Int 
tamanho xs = 1 + length xs

dobro :: Int -> Int
dobro x = 2*x

somar :: Int -> Int -> Int
somar x y = x+y

somart :: (Int, Int) -> Int
somart (x,y) = x+y

u :: Int
u = 7