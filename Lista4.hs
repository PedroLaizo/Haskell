--1. 
--Crie uma função que recebe uma lista e retorna o menor e o maior valor dessa lista.
maxmin :: (Ord a) => [a] -> (a, a)
maxmin (h:t) = maxminaux t h h
    where
        maxminaux [] maior menor = (maior, menor)
        maxminaux (c:d) maior menor 
            |c > maior = maxminaux d c menor
            |c < menor = maxminaux d maior c
            |otherwise = maxminaux d maior menor  
        
-- 2. Crie uma função que verifica se um inteiro é primo. Essa função retorna uma tupla(Bool, Int)
-- na qual o Bool indica se o inteiro é primo ou não e o Int indica qual número conseguiu dividir o inteiro
-- caso ele não seja primo.
primo :: Int -> (Bool, Int)
primo 0 = (False, 0)
primo n = primoaux n 2
    where 
        primoaux :: Int -> Int -> (Bool, Int)
        primoaux n d
            | d > div n 2 = (True, 0)
            | mod n d == 0 =(False, d)
            | otherwise = primoaux n (d+1) 


-- 3. Crie uma função que recebe dois inteiros e retorna um tupla com o MDC e o MMC.


-- 4. Crie uma função que recebe um ano e um mês e retorna o número de dias do mês,considerando anos bisextos
-- e não bissextos.
numdias :: Int -> Int -> Int
numdias ano mes 
    | mes == 2 = if bissexto ano then 29 else 28
    | elem mes [4, 6, 9, 11] = 30
    | otherwise = 31
    where
        bissexto :: Int -> Bool
        bissexto ano
            | ano `mod` 400 == 0 = True
            | ano `mod` 100 == 0 = False
            | ano `mod` 4 == 0 = True
            | otherwise = False


-- 5. Crie o tipo Data (Int,Int,Int). Crie uma função que recebe duas datas e verificam se elas são iguais.
-- Crie uma função que recebe duas datas e retorna a quantidade de dias entreelas.
type Data = (Int, Int, Int)
ig :: Data -> Data -> Bool
ig (dia1, mes1, ano1) (dia2, mes2, ano2)
    | dia1 == dia2 && mes1 == mes2 && ano1 == ano2 = True
    | otherwise = False



-- 6. Crie o tipo Compromisso com Descrição (String) e Data. Crie um tipo Agenda que é uma lista de
-- Compromissos. Crie uma função que recebe uma Agenda e uma Data e retorna uma lista com as descrições
-- de Compromisso naquela Data.
type Descricao = String
type Compromisso = (Descricao, Data)
type Agenda = [Compromisso]

agenda :: Agenda
agenda = 
  [ ("Dentista", (7, 5, 2025)),
    ("Reuniao",  (7, 5, 2025)),
    ("Cinema",   (8, 5, 2025))
  ]

comp :: Agenda -> Data -> [Descricao]
comp [] _ = []
comp ((desc, dat):t) dia 
    | dia == dat = desc : comp t dia
    | otherwise = comp t dia


-- 7. Crie uma função que recebe uma Agenda e retorna a Data que possui mais Compromissos.


-- 8. Crie uma Função que recebe uma Agenda, uma Descrição de Compromisso e a Dataatual e retorna
-- a Data e quantidade de dias que faltam para aquele compromisso. Se não houver Compromisso com a
-- Descrição passada, retorne a Data atual e 0 dias de diferença.