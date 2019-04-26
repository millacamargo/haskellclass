module Aula7 where 

-- TRANSFORMACOES NATURAIS: SAO FUNCOES POLIMORFICAS
-- QUE TROCAM TIPOS DE KIND * -> * 
-- SEMPRE POLIMORFICA E TEM QUE SER O MESMO 
-- TIPO TROCAR A CAIXA DO PRESENTE E NAO O PRESENTE EM SI

-- instance Monad Maybe where 
--     return x = Just x 
--     Nothing >>= f = Nothing
--     (Just x) >>= f = f x

-- ($) :: a -> (a -> b) -> b
-- ($) x f = f x

data Id a = Id a deriving Show

instance Functor Id where 
    fmap f (Id a) = Id (f a)

-- agiu no tipo de kind 2 no caso a lista
-- O a trocou o shape de lista para Maybe 
-- [] ~> Maybe
safeHead :: [] a  -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- Id ~> []
lista :: Id a -> [] a 
lista (Id a) = [a]

-- Id é mesmo que a 
-- Id a == a
-- Entao era shape lista para Id
-- [] ~> Id
cabeca :: [] a -> Id a 
cabeca x = Id (head x)

-- Monada será para kind 2
-- Monoid para kind 1

-- Monada (Monads)
-- Uma Monada em uma categoria C (Hask) é uma tripla (m, return, join)
-- Onde m :: * -> * (functor)
-- return :: a -> m a (transformacao natural) porque ele troca nada por m (pensar que é maybe)
-- join :: m(m a) -> m a (transformacao natural) porque ele é como se fosse um presente embrulhado 2x
-- ou seja, dois embrulhos é a mesma coisa que 1.
-- Uma Monada é um monoide na categoria dos endofuntores, ou seja,
-- um monoid onde voce ta tratando com kind.
-- mappend join e mempty return
-- classe deles: class Functor m => monad m where
-- return que tem tipo return :: a -> m a
-- (>>=) operador binario inves de join
-- (>>=)::m a -> (a -> m b) -> m b
-- tipo da funcao $ 
-- ($)::(a->b)->a->b
-- ($)::a->(a -> b) -> b
-- (>>=) e ($) é a mesma coisa
-- se tirar o a
-- (>>=)::m (m b) -> m b ou seja é o join
-- ghci:
-- Nothing >>= \x -> Just (x+1)
-- Nothing
-- Just 7 >>= \x -> Just (x+3) >>= \y -> Just (x+y)
-- do 
-- x <- just 7
-- y <- just (x+3)
-- return (x+y)

-- Maybe é impureza manter sempre do lado direito (valores monadicos)
-- Na esquerda sao puros por causa do lambda
func :: Int -> Maybe Int
func z = do
   x <- Just z  
   y <- Just (x+3)   
   return (x+y)
   
   -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
   -- a entrada (a) é o unico sem m por isso o lado esquerdo é puro