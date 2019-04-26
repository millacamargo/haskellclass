module Main where

import	Control.Monad

{- 8.2 Crie	uma	função		mult234	::	Double	->	Caixa	Double	
 que	 receba	 uma	 parâmetro	 x	 e	 devolva	 o	 dobro	 de	 x	 na	 primeira
coordenada,	o	triplo	na	segunda	e	o	quádruplo	na	terceira	usando	o operador >>=	. -}

{- 9.4 Faça	 um	 programa	 que	 calcule	 uma	 equação	 do	 segundo
grau,	a	partir	dos	dados	digitados	pelo	usuário. 
main''' :: IO ()
main''' = putStrLn "Digite o valor de a: " >> 
       readLn >>= \a -> putStrLn "Digite o valor de b: " >>
       readLn >>= \b -> putStrLn "Digite o valor de c: " >> 
       readLn >>= \c -> readLn >>= ((b*b) - ((4*a)*c))-}

{- 9.5 Converta	 para	 o	 "estilo	 funcional"	 os	 7	 exemplos	 dados
neste	capítulo.	Basta	escrever	os	 trechos	da	notação		do		pelo		>>=
(bind)		e		>>	 -}

-- Exemplo	1:	soma de dois números

main' ::	IO ()
main' = putStrLn "Digite um número: " >>
	readLn >>= \x -> putStrLn "Digite outro número: " >>
	readLn >>= \y -> putStrLn $ "Resultado: " ++ show (x+y)
	
-- Exemplo	2:	loop	while	funcional

main :: IO ()
main = do
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
	
-- Exemplo	3:	loop for funcional

main :: IO ()
main = do
	z	<-	readLn
	forM_	[1..z]	$	\i	->	do
	print	i
	
-- Exemplo	4:	for	funcional

main :: IO ()
main = do
z <- readLn
let	dentro	i	=	do
putStrLn	$	"Número	"	++	(show	i)
	readLn
ns	<-	mapM	dentro	[1..z]
putStrLn	$	"Resultado:	"	++	(show	$	sum	ns)




