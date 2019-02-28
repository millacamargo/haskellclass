module Aula2 where


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

toDia :: Int -> Dia
toDia 1 = Domingo
toDia 2 = Segunda
toDia 3 = Terca
toDia 4 = Quarta
toDia 5 = Quinta
toDia 6 = Sexta
toDia 7 = Sabado

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