module Main where

import	Control.Monad
import	Text.Printf
import	System.Directory
import System.Random

{- 8.2 Crie	uma	função		mult234	::	Double	->	Caixa	Double	
 que	 receba	 uma	 parâmetro	 x	 e	 devolva	 o	 dobro	 de	 x	 na	 primeira
coordenada,	o	triplo	na	segunda	e	o	quádruplo	na	terceira	usando	o operador >>=	. -}

data Caixa a = Um a | Dois a a | Tres a a a

mult234	::	Double	->	Caixa	Double
mult234 x = Tres (x*2) (x*3) (x*4) 

-- Não tem sentido, porque não tem onde inserir um >>=

{- 9.4 Faça	 um	 programa	 que	 calcule	 uma	 equação	 do	 segundo
grau,	a	partir	dos	dados	digitados	pelo	usuário. 
main :: IO ()
main = putStrLn "Digite o valor de a: " >> 
       (readLn >>= \a -> putStrLn "Digite o valor de b: ") >>
       readLn >>= \b -> putStrLn "Digite o valor de c: " >> 
       readLn >>= \c ->
        if (d a b c) == 0 then 
             putStrLn $ "O resultado eh: " ++ show ((-b+sqrt(d a b c))/(2*a))
            else
            if (d a b c) > 0 then
                putStrLn $ "O resultado eh: " ++ show (((-b+sqrt(d a b c))/(2*a)),  ((-b-sqrt(d a b c))/(2*a)))
            else
                putStrLn $ "ERRO" 
     where d b a c = ((b*b) - ((4*a)*c))
       -} 

{- 9.5 Converta	 para	 o	 "estilo	 funcional"	 os	 7	 exemplos	 dados
neste	capítulo.	Basta	escrever	os	 trechos	da	notação		do		pelo		>>=
(bind)		e		>>	 


-- Exemplo	1:	soma de dois números (feito)

main' ::	IO ()
main' = putStrLn "Digite um número: " >>
    readLn >>= \x -> putStrLn "Digite outro número: " >>
    readLn >>= \y -> putStrLn $ "Resultado: " ++ show (x+y)
    
-- Exemplo	2:	loop	while	funcional

main' :: IO ()
main' = do
let	loop = do
    putStrLn "Qual seu nome?"
    nome <-	getLine
    if (nome ==	"")	then do
        putStrLn "Erro... "
        loop
    else
    putStrLn $ "Ola	" ++ nome
    loop
    putStrLn "Fim" 
    
main :: IO ()
main = 
    let loop = putStrLn "Qual seu nome?" >>
            getLine >>= \nome ->
                if (nome ==	"")	then do
                    putStrLn "Erro... "
                    loop
                else
                    putStrLn $ "Ola " ++ nome
    in loop >> putStrLn "Fim"
    
-- Exemplo	3:	loop for funcional

main' :: IO ()
main' = do
    z	<-	readLn
    forM_	[1..z]	$	\i	->	do
    print	i
    
main :: IO ()
main = readLn >>= \z ->
        forM_ [1..z] $ \i -> print i
 
   
-- Exemplo	4:	for	funcional

main :: IO ()
main = do
z <- readLn
let	dentro	i	=	do
putStrLn	$	"Número	"	++	(show	i)
    readLn
ns	<-	mapM	dentro	[1..z]
putStrLn	$	"Resultado:	"	++	(show	$	sum	ns)

main :: IO ()
main = let dentro i = (putStrLn $ "Número " ++ (show i)) >> readLn in
    readLn >>= \z -> 
        mapM dentro [1..z] >>= \ns ->
            putStrLn $ "Resultado: " ++ (show $ sum ns)-}
    


-- Exemplo 5: adivinhando uma carta do baralho
{-
data	Naipe	=	Ouros	|	Espadas	|	Copas	|	Paus	deriving	(Eq, Show,	Enum, Read)
data	Valor	=	Dois	|	Tres	|	Quatro	|	Cinco	|	Seis	|	
                                                    Sete	|	Oito	|	Nove	|	Dez	|	J	|	Q	|	K	|	A
                                                    deriving	(Eq, Show,	Enum, Read)
data	Carta	=	Carta	{valor	::	Valor,	naipe	::	Naipe}	deriving	(Eq, Show, Read)


main'''	::	IO	()
main'''	=	do
                let	acertou	True	=	"Você	acertou"
                                acertou	False	=	"Errou..."
                baralho	<-	return	[Carta	x	y	|	x<-[Dois	..	A],	y<-[Ouros	..	Paus]]
    cartaNum	<-	randomRIO	(1,	length	baralho)				
                carta	<-	return	$	baralho	!!	cartaNum
                putStrLn	"Escreva	a	carta	para	adivinhar:	"
                palpite	<-	readLn	
                putStrLn	$	"Sua	carta	foi	"	++	show	(valor	carta)	++	"	de	"	++
    show	(naipe	carta)
                putStrLn	$	acertou	$		carta	==	palpite 
                
main :: IO ()
main = return [Carta x y | x <- [Dois .. A], y <- [Ouros .. Paus]] >>= \baralho ->
        randomRIO (1, length baralho) >>= \cartaNum ->
        (return $ baralho !! cartaNum) >>= \carta ->
        putStrLn "Escreva a carta para adivinhar: " >>
        readLn >>= \palpite ->
        (putStrLn $ "Sua carta foi " ++ show (valor carta) ++ " de " ++ show (naipe carta)) >>
        (putStrLn $ acertou $ carta == palpite)
    where acertou True = "Você acertou"
          acertou False = "Errou..."
        

-- Exemplo 6: salário	 total	 e	 maior	 de	 uma	 lista	 de funcionários


main'''	::	IO	()
main'''	=	do
                lista	<-	fmap	(map	words	.	lines)	$	readFile	"func.dat"
                salarios	<-	return	$	map	(\(_:vl:_)	->	read	vl)	lista	::	IO	[Double]
                printf	"%.2f\n"	$	sum	salarios
                print	$	maximum	salarios-}
                
main :: IO ()
main = fmap (map words . lines) (readFile "func.dat") >>= \lista ->
        printf "%.2f\n" (sum $ f lista) >>
        print (maximum $ f lista)
    where f :: [[String]] -> [Double]
          f lista = map (\(_:v1:_) -> read v1) lista

{- 1. Le o arquivo, primeiro linhas e depois as palavras;
   2. \lista: uma lista com uma sublista 
   3. Pega a lista de listas e conver para uma lista de double
   4. Printa a soma dos salários no printf e o maior no print-}
                
-- Exemplo 7: uso	do	write	e	append 

main' :: IO ()
main' = putStrLn "Digite o nome do arquivo. Será criado caso não exista" >>
    getLine >>= \arq ->
    putStrLn "Digite uma mensagem" >> 
    getLine >>= \mensagem ->
    doesFileExist arq >>= \existe ->
        if existe then
            appendFile arq ("\n" ++ mensagem) 
            else writeFile arq mensagem 
            
{- getLine pega o IO String depois o >>= pega o valor monadico tirar de dentro da Monad e vai aplicar esse valor
numa função que retorna essa mesma Monad -}






