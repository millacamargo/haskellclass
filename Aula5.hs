module Aula5 where

-- Nas funções polimórfica você não pode ver o que estão nas variáveis, apenas programá-las.
-- Uncurrying, o contrário do currying.

-- Exemplos de funções polimorficas:
mapp :: (a -> b) -> [a] -> [b]
mapp f [] = []
mapp f (a:as) = (f a) : mapp f as

-- Entrada é a tupla de a,b e c a saida, por isso c será f (variavel declarada depois) e a tupla.
foo :: ((a, b) -> c) -> a -> b -> c
foo f a b = f (a, b)

-- Esse Bool é ao quadrado porque tem apenas duas respostas e tuplas são multiplicações
-- Logo a saída será a^2;
-- g :: (Bool -> a) = (a,a)
-- g f = (f True, f False)

-- tipo monomorfico: fixo em int, voce declarou ja o que é, logo nao pode adicionar nada alem de int
-- Você quer saber o que tem dentro. Ex: No presente interessa o conteúdo.
-- kind 1 lista de int.
data Sacola = Sacola Int Int deriving Show

-- tipo polimorfico: o a é um tipo variavel.
-- funcoes entre tipos
-- Me preocupo com a caixa do presente, porque dentro da caixa posso colocar qualquer coisa.
-- no :kind ela recebe algo e devolve algo. * -> *, por conta do a.
-- Não pode retornar bool porque se voce declara o primeiro o segundo tem o mesmo valor
-- kind 2
data Bolsa a = Bolsa a a deriving Show

-- No Haskell tipos possuem tipos - Kind
-- No ghci :kind Int

-- :kind Mochila * -> * -> *
-- é como se fosse True c True -> Bool -> Char -> Bool
-- data (,) = (,) a b deriving Show
-- Pode guardar True, porque tem coisas diferentes a b.
-- kind 3
data Mochila a b = Mochila a b deriving Show

    -- kind 1 lista de Int
    -- kind 2 listas que precisa dizer do que, pilhas
    -- kind 3 tupla, voce pode grudar dois tipos diferentes. JSON: um tipo de campo e um valor. A seta de funcao, 
    -- porque pode receber 2qualquer parametro de entrada e saida
    -- kind 4 uma tupla de 3 elementos.
    
-- IO semana que vem.
-- IO :: * -> *
-- Controle do que o usuario insere, ele separa o que é puro para teste unitario. E teste de integracao
-- com IO.

-- Type Class: tenho uma funcao que quero mostrar todos os tipos que implementam.
-- tipo ex: data Curso = SI | ADS | LOG | GE | GP deriving Show
-- Se nao quiser mostrar SI, por exemplo, voce tem que tirar o deriving show 

-- Typeclasses: Solucao do Haskell
-- para o poilimorfismo ad-hoc.
-- Um typeclass possuira funcoes a 
-- serem implementadas de acordo 
-- com tipos específicos.
-- Show Int => Como mostrar inteiros na tela
-- Eq String => COmo comparar Strings
-- ...
-- So o tipo de instancia de show, porque tenho que programa-las.
-- sao restricoes aos tipos.

data Curso = SI | ADS | LOG | GE | GP

-- Eq sempre coloca em deriving. NUNCA FAZER ISSO!
-- instance Eq Curso where
--     SI == SI = False
--    ADS == _ = True
--    _ == _ = False
--    GE == GP = False
--    ADS == SI = True

instance Show Curso where 
    show SI = "Curso de design"
    show ADS = "Curso de COBOL"
    show LOG = "Curso de conteiner"
    show GP = "Curso de porto"
    show GE = "Curso de cafe-da-manha"
    
-- https://hoogle.haskell.org/
-- Eq leis:
-- Reflexivity
-- x == x = True
-- Symmetry
-- x == y = y == x
-- Transitivity
-- if x == y && y == z = True, then x == z = True
-- Substitutivity
-- if x == y = True and f is a "public" function whose return type is an instance of Eq, then f x == f y = True
-- Negation
-- x /= y = not (x == y)
    
instance Num Bool where 
    (+) = (||)
    (*) = (&&)
    True - True = False
    False - False = False 
    True - False =  True
    False - True = True
    abs True = True
    abs False = False 
    fromInteger i
        | even i = False
        | otherwise = True
    signum True = 1 
    signum False = 0
    
-- :t sum
-- sum :: Num a => [a] -> a
-- pega uma lista de numeros e soma.

class SimNao a where 
    simnao :: a -> Bool
    

instance SimNao Char where 
    simnao ' ' = False
    simnao _ = True
    
instance SimNao Bool where 
    simnao True = True
    simnao False = False
    
-- Toda vez que tiver kind 1 com kind 2 tem que fazer essa restricao
-- Pois tem que compara se os objetos dentro das bolsas sao iguais mesmo
-- A restricao Eq a => foi usada, pois,
-- :kind Eq => * -> Constraint e
-- :kind Bolsa => * -> *
instance Eq a => Eq (Bolsa a) where 
    Bolsa a1 a2 == Bolsa b1 b2 = (a1 == b1 && a2 == b2)
                                || (a1 == b2 && a2 == b1)