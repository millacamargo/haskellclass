module Aula2 where

-- o que esta em data sempre maiuscula na primeira letra

-- Outro exemplo
data Unidade = Kmh | Mph deriving Show

data Velocidade = Velocidade {
    valor :: Double,
    unidade :: Unidade
} deriving Show

conversao :: Velocidade -> Velocidade 
conversao (Velocidade v Kmh) = Velocidade (0.625*v) Mph
conversao (Velocidade v Mph) = Velocidade (1.6*v) Kmh

-------------------------------------------------------

data Curso = ADS | SI | GP | LOG | GE
        deriving (Eq, Show)

--TIPOS COM CAMPOS
-- Tipo de tupla String Curso Double, como se fosse uma classe.
--data Aluno = Aluno String Curso Double 
       -- deriving (Eq, Show) 
        
-- RECORD SYNTAX: DAR AOS CAMPOS E ESSES NOMES SAO FUNCOES DE PROJECAO (GETTERS)
--nome (Aluno "Camila" SI 4000) no GHCI vai aparecer so o nome.
data Aluno = Aluno {
    nome :: String,
    curso :: Curso,
    salario :: Double
} deriving (Eq, Show)

-- Pattern Matching
aSalario :: Aluno -> Aluno
-- tres campos String Curso Double, tres variaves n de nome, c de curso, s de salario
-- Para rodar no GHCI colocar aSalario (Aluno "Camila" SI 12000)
aSalario (Aluno n c s) = Aluno n c (1.2*s)

-- Sem Pattern Matching
bSalario :: Aluno -> Aluno 
bSalario a = Aluno (nome a) (curso a) (1.2*(salario a))

-- A diferenca é que com pattern matching nao precisa fazer gets
-------------------------------------------------

data Dia = Domingo | Segunda | Terca | Quarta
         | Quinta  | Sexta   | Sabado
         deriving (Show, Eq, Enum, Read)

-- O que tudo isso abaixo quer dizer:
-- Se voce chamar a funcao agenda com Sexta vai dar ""Dia de Fabio Assuncao""
-- Se voce chamar a funcao com qualquer outra coisa ou seja _ vai dar "Dia de bosta"

agenda :: Dia -> String
agenda Segunda = "Dia de trabalhar"
agenda Quarta =  "Dia de futebol" 
agenda Quinta = "Dia de Haskell"
agenda Sexta = "Dia de Fabio Assuncao"
agenda Sabado = "Dia de aula"
agenda _ = "Dia de bosta"

-- 1) CRIE A FUNÇÃO toDia QUE RECEBA UM INTEIRO
-- E DEVOLVA UM DIA CONFORME A REGRA: 1 -> Domingo,
-- 2 -> Segunda, ..., 7 -> Sabado.

-- FUNCOES QUE NAO POSSUEM TODAS AS ENTRADAS
-- DO PATTERN MATCHING SAO CHAMADAS DE
-- PARCIAIS. Esta era um exemplo disso.
-- A partir do momento que coloca-se either vira funcao completa.
-- Either: ou String ou Dia. Somando dois tipos. Somando a String com o dia.
-- Tudo que tiver na direita é acerto, esquerda erro, por isso o right e left.
-- toDia :: Int -> String + Dia
-- (String + 7)^Dia
toDia :: Int -> Either String Dia 
toDia 1 = Right Domingo
toDia 2 = Right Segunda
toDia 3 = Right Terca
toDia 4 = Right Quarta
toDia 5 = Right Quinta
toDia 6 = Right Sexta
toDia 7 = Right Sabado
toDia _ = Left "Erro: Dia invalido"

-- 2) CRIE UM TIPO CHAMADO DAY QUE POSSUA OS DIAS
-- DA SEMANA, EM INGLES, COMO VALORES. CRIE, TAMBEM,
-- A FUNCAO traduzir QUE TRADUZA DO INGLES PARA
-- O PORTUGUES.

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday 
            deriving Show

traduzirPT :: Day -> Dia
traduzirPT Sunday = Domingo
traduzirPT Monday = Segunda
traduzirPT Tuesday = Terca
traduzirPT Wednesday = Quarta
traduzirPT Thursday = Quinta
traduzirPT Friday = Sexta
traduzirPT Saturday = Sabado

-- Para fazer rodar no GHCI colocar nome da funcao + tipo que quer que apareca
-- Ex: traduzirPT Sunday