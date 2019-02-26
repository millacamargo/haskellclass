module Cap2 where
import Data.Char
-- pag. 15 - EXERCICIOS

-- 2.1

a = [ 11^x | x<-[0..6] ]

b = [ x | x<-[1..40], mod x 4 /= 0 ]

c = [ "A"++[x]++"BB" | x<-['a'..'g'] ]

d = [ x | x<-[5,8..41], not (elem x [14,23,35]) ] -- a lista esta correta, porem o exercicio filtra os numeros 14, 23 e 35, deve ter alguma relação entre eles.

--e :: Double -> [Double]
e = [ x/2 | x<-[2.0,1.0,0.5,0.25,0.125,0.0625] ]

f = [ x | x<-[1,10..64] ]

g = [x | x <- [2,4..30], not (elem x [6, 14, 20, 26]) ]

h = [ x | x<-['@'..'L'], not (elem x "BFHIK") ] 

-- 2.2 crie uma função que verifique se o tamanho de uma string é par ou não, use bool como retorno
e22 :: String -> Bool
e22 "" = True
e22 s = mod (length s) 2 == 0

-- 2.3 escreva uma função que receba um vetor de strings e retorne uma lista com todos os elementos em ordem reversa
e23 :: [String] -> [String]
e23 x = [ reverse xs | xs <- x ] -- x = ["Jonathan", "Gustavo", "Gutemberg"] retorna ["nahtanoJ","ovatsuG","grebmetuG"]

-- 2.4  Escreva uma função que receba um vetor de String e retorne uma lista com o tamanho de cada String. As palavras de tamanho par devem ser 
-- excluídas da	respost
e24 :: [String] -> [Int]
e24 [] = []
e24 x = [ length xs | xs<-x, mod (length xs) 2 /= 0 ]

-- 2.5 Escreva a função head como composição de duas outras
(last.reverse)

-- 2.6 faça uma função que receba uma string e retorne True se for um palindromo, caso contrario, false
upp :: String -> String
upp x = [ toUpper xs | xs<-x ]

e26 :: String -> Bool
e26 x = upp (reverse x) == upp x


-- 2.7 faça uma função que recebe um inteiro e retorne uma tupla contendo:
    -- o dobro desse numero na primeira coordenada
    -- o triplo na segunda
    -- quadruplo na terceira 
    -- quintoplo na quarta
e27 :: Int -> (Int, Int, Int, Int)
e27 x = (x*2, x*3, x*4, x*5)

