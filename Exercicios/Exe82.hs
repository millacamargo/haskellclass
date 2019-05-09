module Exe82 where

import Control.Monad

data Caixa a = Um a | Dois a a | Tres a a a deriving (Eq, Show)

instance Functor Caixa where
    fmap c (Tres g h i) = Tres (c g) (c h) (c i)

instance Monad Caixa where
    return x = Um x
    (Um x) >>= f = (f x) 
    (Dois x y) >>= f = (f y)
    (Tres x y z) >>= f = (f z)

mult234	::	Double	->	Caixa	Double
mult234 x = return x >>= \d -> Tres (d*2) (d*3) (d*4) 