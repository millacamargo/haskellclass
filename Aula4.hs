module Aula4 where

-- LAMBDA -> FUNCOES SEM ASSINATURA E SEM CORPO. SÃO ANONIMAS E POSSUEM A FORMA
-- \ (este significa lambda, simbolo do haskell) p1 p2 p3 ... pn -> EXPR
-- ex no ghci: (/ x y z -> x+y+z) 7 8 9, isto significa que 7 é x, 8 é y e z é 9 e depois
-- da -> ele irá somar esses valores
-- computer file canal no youtube para curiosidades computacionais

-- Currying voce pode chamar a funcao com menos parametros e pode chamar depois
-- ex :t (1 2 3), como tem 4 int abaixo ele mostra (somar 1 2 3) :: Int
-- numa funcao lambda seria let k = (\x y z -> x+y+z) 3 2, ou seja
-- ele assimila 3 a x e 2 a y = (\z -> 3 + 2 +z)
-- k 7 vai acontecer isso (\z -> 3+2+z) 7 = 12, ele joga o 7 no z

somar :: Int -> Int -> Int -> Int 
somar x y z = x+y+z

-- HIGH ORDER FUNCTIONS -> SÃO FUNCOES QUE RECEBEM E/OU RETORNAM FUNCOES.
-- foo significa que é uma funcao, em parenteses primeiro int é entrada e o segundo é uma saida sempre.
-- o terceiro int diz que a funcao é int a que é recebida pelo f.
foo :: (Int -> Int) -> Int --essa é a saida
foo f = 1 + f 5 -- esse f 5 é do int depois do ->, e o f do int antes do ->

-- No foo:
-- let dobro x = 2*x foo dobro 
-- dobro seria o f do lado do 5, x o primeiro int e o segundo Int seria o 2*x

-- duas funcoes pro verdadeiro e outra pro falso
-- recebe int e devolve string
-- funcao --entrada   --funcaoehpar       --funcaoMult3
myIf :: Int -> (Int -> String) -> (Int -> String) -> String -- saida
myIf x f g 
    | x <= 0 = f x 
    | otherwise = g x

-- ehImpar :: Int -> String
-- ehImpar x
 --   | odd x = "Eh Impar"
  --  | otherwise = "Eh Par"
    
ehPar :: Int -> String 
ehPar x
    | even x = "Eh Par"
    | otherwise = "Eh Impar"
    
ehMult3 :: Int -> String 
ehMult3 x 
    | mod x 3 == 0 = "Multiplo de 3"
    | otherwise = "Nao eh multiplo de 3"
    
-- no ghci myIf (-4) ehPar ehMult3

-- ate agora utilizamos funcoes monomorficas pois usamos so inteiros
-- agora veremos map, que é polimorfico e diz voce me passa uma funcao e uma sacola de bombom,
-- o map transformara todo mundo da sacola de bombom em lixo.
-- o map vai comer o saco de bombom inteiro e devolver um saco de lixo.
-- Exemplos no GHCI:
-- let dobro x = 2*x
-- map dobro [1,2,3,4,5,6] dará [2,4,6,8,10,12]
-- Outro exemplo no ghci:
-- map length ["HASKELL,"PHP","SKALA"] resultará o numero de palavras de cada ou seja [7,3,5]
-- Mais um: map reverse ["OLA","ANA","OVO","FATEC"] resultará em ["ALO","ANA","OVO","CETAF"]
-- map ehPar [1,2,5,3] devolve ["Eh Impar","Eh Par","Eh Impar","Eh Impar"]
 
-- filter: ele pega um predicado, e filtra elementos (AVA)
-- Ex: filter (\x -> x > 0) [-2,-5,2,7]
-- usando currying no map  map (2*) [1,2,3,4,5] eh currying porque falta algo no 2* e voce coloca num array
-- resultara [2,4,6,8,10]

-- map, filter e $ sao grandes exemplos de High order function
-- dobro $ 2+2 e dara 8
-- :t ($) dara ($) :: (a -> b) -> a -> b
-- $ tira os parenteses no caso se for dobro 2+2 so calculara o dobro de 2 e somara com 2
-- colocando o $ ele fara o mesmo que 2*(2+2)

-- reduce = foldl
                -- entradas   -- funcao
-- no ghci foldl (\aux vi -> aux+vi) 0 [12,3,4,5,6]
-- essa funcao soma tudo que ta no []
-- 0 é o aux
-- ou pode ser foldl (+) 0 [12,3,4,5,6] que dará 30 tambem
-- conta de acumulador e contador é com o foldl
-- seria em java:
-- public int somar (int[] v){
-- int aux = 0;
-- for(int i); i < v.length; i++){
--    aux = aux + v[i];}
-- return aux;}

contarPar :: Int -> Int -> Int
contarPar c l 
    | even l = c+1
    | otherwise = c
    
-- ultima high order function (.), faz a juncao de duas funcoes.Chama uma funcao dentro da outra.
-- calculasegundo  --primeiro   
-- (b -> c) -> (a -> b) -> a -> c

tamanho :: String -> Int 
tamanho xs = length xs

ehPar2 :: Int -> Bool
ehPar2 x = even x
 -- No ghci ehPar2 . tamanho $ "HASKELL"