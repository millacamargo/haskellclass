module Cap5 where
    
import Data.Monoid

{- 5.2 - Crie uma função totalGeral	que	 recebe	 uma lista	 de
produtos e	retorna	o preço	total deles	usando	o monoide anterior. -}

--este é o  monoide anterior
data TipoProduto =  Escritorio | Informatica | Livro | Filme | Total deriving Show

data Produto = Produto {valor::Double, tp::TipoProduto} | Nada deriving Show

instance Monoid Produto where
    mempty = Nada
    mappend (Produto valorA tpA) (Produto valorB tpB) = Produto (valorA + valorB) Total
    mappend x Nada = x
    mappend Nada x = x
    
totalGeral :: [Produto] -> Produto 
totalGeral s = mconcat s

{-
5.4 - Crie uma função minAll que recebe	 um	 [Min] e
retorna	um	Min	contendo o menor valor.

No ghci colocar: minAll [Min 1, Min 2] ou qualquer comparativo
-}
data Min = Min Int deriving (Ord, Eq, Show)
instance Monoid Min where
    mempty = Min maxBound -- (maxBound representa o maior inteiro existente no Haskell)
    mappend (Min x) (Min y) = Min (min x y)
    
minAll:: [Min] -> Min
minAll xs = mconcat xs

{-
5.6 - A função max no Haskell retorna o	maior entre	 dois
números, por exemplo: max 4	5 =	5.
> Quanto vale a	expressão Max 10 <>	Max	13 <> Max 5	? ( Max 13 )
> Explique sua escolha para	o mempty. (Lembre-se mempty é valor neutro)
> Crie uma função maxAll que recebe um [Max] e retorna um
Max contendo o maior valor.
-}

-- Crie um tipo Max com um campo inteiro que seja instância de Ord, Eq e Show (deriving).
data Max = Max Int deriving (Ord, Eq, Show)

-- Crie uma instância de	Monoid para Max (minBound representa o menor inteiro	existente no Haskell).
instance Monoid Max where
    mempty = Max minBound
    mappend (Max x) (Max y) = Max (max x y)
    
maxAll:: [Max] -> Max
maxAll xs = mconcat xs

-- maxAll [Max 3, Max 5]
-- Sum 1 <> Sum 2   =   mconcat [Sum 1, Sum 2]