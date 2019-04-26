module Main where

-- IO nada ignorando a saida na tela, tipo void
-- () significa nada

--   main :: IO ()
--   main = putStrLn "Ola mundo!"
 
 -- como executar
 -- no bash:
 -- cd nomedapasta
 -- ghc Main.hs -o Main
 -- ./Main
 
safeHead :: [a] -> Maybe a 
safeHead [] = Nothing
safeHead (x:_) = Just x

main''' :: IO ()
main''' = do 
    putStrLn "Digite uma palavra: "
    str <- getLine
    case safeHead str of
        Nothing -> putStrLn "ERRO"
        Just letra -> putStrLn $ "A letra eh: " ++ show letra
    
-- Pode ser assim
main :: IO ()
main = putStrLn "Digite um numero: " >> 
       readLn >>= \x -> putStrLn "Digite outro numero: " >>
       readLn >>= \y -> putStrLn $ "O resultado eh: " ++ show (x+y)

-- Ou assim
main' :: IO ()
main' = do
    putStrLn "Digite um numero: "
    x <- readLn
    putStrLn "Digite outro numero: "
    y <- readLn
    putStrLn $ "O resultado eh: " ++ show (x+y)