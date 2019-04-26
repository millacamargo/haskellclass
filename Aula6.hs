module Aula6 where 

data Tree a = Nulo 
            | Leaf a 
            | Branch a (Tree a) (Tree a) deriving Show

instance Functor Tree where 
    fmap f Nulo = Nulo
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)

emOrdem :: Tree a -> [a]
emOrdem Nulo = [] 
emOrdem (Leaf x) = [x]
emOrdem (Branch x l r) = 
    emOrdem l ++ [x] ++ emOrdem r

tamanho :: String -> Int
tamanho x = length x

-- data Maybe a = Nothing | Just a deriving Show

{-
instance Functor Maybe where 
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)
-}

-- fmap :: Functor f => (a -> b) -> f a -> f b
data Bolsa a = Bolsa Int a a deriving Show

instance Functor Bolsa where 
    fmap f (Bolsa x y z) = Bolsa x (f y) (f z)

instance Functor ((,,) a b) where 
    fmap f (x,y,z) = (x, y, f z)
    
    
    