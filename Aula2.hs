module Aula2 where

-- o que esta em data sempre maiuscula na primeira letra

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