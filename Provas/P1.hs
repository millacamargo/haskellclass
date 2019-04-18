module P1 where 

import Data.Monoid

{-1. Considere o tipo Produto contendo os campos nome, valor e categoria. Os campos nome e valor sao String
e Double respectivamente, ao passo que categoria é um tipo Categoria que possui os valores Livro, Brinquedo e 
Escritorio. Crie os dois tipos com instancias necessarias para a solucao deste exercicio e implemente as funções.-}

data Categoria = Livro | Brinquedo | Escritorio
    deriving (Eq, Show)

data Produto = Produto {
    nome :: String,
    preco :: Double,
    categoria :: Categoria
} deriving (Eq, Show)

-- a) extrair :: [Produto] -> [Double] que retorna uma lista com todos os preços de produto

extrair :: [Produto] -> [Double]
extrair ps = map preco ps

-- b) minValor :: [Produto] -> Double que retorna o preço produto de valor minimo

minValor :: [Produto] -> Double
minValor = minimum . extrair 

-- c) livroEscr :: [Produto] -> [Produto] que retorna os produtos que não sejam brinquedos

livrEscr :: [Produto] -> [Produto]
livrEscr ps = filter (\x -> categoria x /= Brinquedo) ps

-- d) avgLivrEscr :: [Produto] -> Double que retorna a media dos produtos que nao sao brinquedos

avgLivrEscr :: [Produto] -> Double
avgLivrEscr ps = (sum . extrair . livrEscr $ ps) / fromIntegral (length ps)

-- e) maxMinLivr :: [Produto] -> (Double, Double) que retorna o maior e menor preço dos livros

maxMinLivr :: [Produto] -> (Double,Double)
maxMinLivr ps = (maximum ls, minimum ls)
    where 
        ls = extrair . filter (\x -> categoria x == Livro) $ ps

-- f) countBrinq :: [Produto] -> Int que retorna a quantidade de itens de brinquedo

countBrinq :: [Produto] -> Int
countBrinq ps = length ps - (length $ livrEscr ps)

-- 2. Sobre Monóides: 

-- a) Considere o tipo dat Sozinho = Sozinho, crie uma instancia de Monoid para este tipo;

data Sozinho = Sozinho 

instance Monoid Sozinho where 
    mempty = Sozinho 
    mappend _ _ = Sozinho

{- b) Considere o operador binário 
        ign :: String -> String -> String
        ign l m = m
O tipo String com esta operação ign e o elemento neutro [] formam um monoide? Justifique sua resposta.
-}
-- Não forma um monoide porque [] "Ola" = "OLA" e "Ola" [] = [].

-- 3. Considere o tipo data Dupla a = Dupla a [Int]

-- a) Qual o kind de Dupla Bool?
-- k: Dupla *

-- b) Crie uma instancia de Functor para Dupla.
instance Functor Dupla where 
    fmap f (Dupla a ls) = Dupla (f a) ls

-- c) Qual o tipo da expressao Dupla '5' [0,1]?
-- :t Dupla '5' [0,1]
-- Dupla '5' [0,1] :: Dupla Char

-- d) Qual o tipo da expressao Dupla?
-- a -> [Int] -> Dupla a

{- e) Crie uma instancia de Show que mostre na tela uma dupla em formato de tuplas do Haskell. Por exemplo,
Dupla 'k' [1,2,3] deverá ser mostrado (k,[1,2,3])-}

instance Show a => Show (Dupla a) where 
    show (Dupla a ls) = "(" ++ show a ++ "," ++ show ls ++ ")"

{- f) Faça uma função mostra :: Dupla a -> Either [Int] a que mostra a lista 
de inteiros caso o seu tamanho seja maior que zero ou o campo do tipo a caso contrario-}

mostra :: Dupla a -> Either [Int] a 
mostra (Dupla a ls)
    | length ls > 0 = Right a
    | otherwise = Left ls

-- 4. Dê o tipo das expressões abaixo de maneira mais genérica possível

-- a) \x -> x :: a -> a
-- b) id :: a -> a
-- c) 36 :: Num a => a
-- d) ("FATEC",False,'K') :: ([Char], Bool, Char)
-- e) [(False, False), (True, False), (False, True), (True, True)]  :: [(Bool, Bool)] 
-- f) filter id :: [Bool] -> [Bool]

-- 5. Considere data () = () e complete:

-- a) 

f1 :: (a,b) -> (b,a,a)
f1 (a,b) = (b,a,a)

-- b)

f2 :: a -> (a,a,a,())
f2 a = (a,a,a,())

-- c)

f3 :: Either a () -> Maybe a 
f3 (Left a) = Just a 
f3 (Right ()) = Nothing

-- d)

f4 :: (a -> b) -> (b -> z) -> (a -> z)
f4 f g = g.f

-- e)

f5 :: (a -> b) -> (c,a) -> (c,b)
f5 g = \(c,a) -> (c, g a)


