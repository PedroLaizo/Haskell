module Biblioteca.Util where

import System.IO

-- Crio o tipo algebrico de data
-- possui 3 campos dia, mes e ano
-- deriva as instancias permitindo mostrar, ler e comparar
data Data = Data { dia :: Int, mes :: Int, ano :: Int } deriving (Show, Read, Eq)

-- Função que recebe a data e retorna a string
-- recebe um objeto data e retorna uma string, no formato dd/mm/aaaa
dataStr :: Data -> String
dataStr (Data d m a) = check d ++ "/" ++ check m ++ "/" ++ show a where
    -- função auxiliar usada para adicionar um zero à esquerda para dias e meses menores que 10
    check :: Int -> String
    check x = if x < 10
        then "0" ++ show x
        else show x 

-- Função para ler a data
-- solicita ao usuario dia, mes e ano e entao retorna um objeto data construido a partir da entrada
-- usa getInt para ler um inteiro
lerData :: IO Data
lerData = do
    putStrLn "Dia:"
    d <- getInt
    putStrLn "Mês:"
    m <- getInt
    putStrLn "Ano:"
    a <- getInt
    return (Data d m a)

-- Aqui até formata é para formatar a string que vai aparecer na tela durante a listagem
-- cria uma lista repetindo o elemento a um determinado numero de vezes
repete :: a -> Int -> [a]
-- caso base
repete _ 0 = []
-- caso recursivo
repete a 1 = [a]
-- caso geral
repete a x = a : repete a (x-1)

-- funcao que faz o alinhamento da string à esquerda
alinhaEsq :: String -> Char -> Int -> String
alinhaEsq str c x
-- se o comprimento da string for maior ou igual ao comprimento desejado x, ele usa functake para pegar os primeiros x caracteres
    | length str >= x = functake str x
    -- caso contrario, adiciona o caractere c ao final da string e chama a alinhaEsq recursivamente
    | otherwise       = alinhaEsq (str ++ [c]) c x

-- funcao que faz o alinhamento da string à direita
alinhaDir :: String -> Char -> Int -> String
-- caso base
alinhaDir str c 0 = str
-- caso geral, adiciona x-length(str) repetições do caractere c ao inicio da string
alinhaDir str c x = repete c (x - length str) ++ str 

-- Função que formata um par chave-valor (String, String) para exibição
-- a chave deve ser alinhada à esquerda com pontos (.) em 29 caracteres e o valor é alinhado à direita com espaços ( ) em 50 caracteres, separado por dois pontos
formata :: String -> String -> String
formata chave valor = alinhaEsq chave '.' 29 ++ ":" ++ alinhaDir valor ' ' 50

-- Função que faz o take
-- Uma função auxiliar que implementa o comportamento de take. Retorna os primeiros n elementos de uma lista.
functake :: [a] -> Int -> [a]
-- caso base
functake _ 0 = []
-- Caso recursivo: pega a cabeça da lista e chama recursivamente para o restante da lista e n-1.
functake (h:t) n = h : functake t (n-1)

-- Função que le inteiro
-- Lê uma linha da entrada padrão (console) e tenta convertê-la para um valor inteiro.
getInt :: IO Int
getInt = do
    dado <- getLine
    return (read dado :: Int)

-- Função que divide uma String em uma lista de Strings, função lines
-- Divide uma String em uma lista de Strings, usando o caractere de nova linha (\n) como delimitador.
splitLines :: String -> [String]
-- caso base
splitLines "" = []
-- caso geral
splitLines s = 
    -- Usa a função auxiliar dividelistas para encontrar a primeira linha e o restante da string.
  let (linha, resto) = dividelistas (== '\n') s
    -- Adiciona a linha à lista resultante e processa o resto recursivamente. Ele lida com o caso em que o restante é vazio ou começa com \n.
  in linha : case resto of
      ""     -> []          
      ('\n':r) -> splitLines r
      _ -> splitLines resto

-- Função criada para dividir a lista em partes baseado no '\n'
-- função auxiliar generalizada para dividir uma lista em duas partes com base em um predicado p. A primeira parte contém os elementos antes do primeiro elemento que satisfaz o predicado, e a segunda parte contém o restante da lista, começando com o elemento que satisfez o predicado.
dividelistas :: (a -> Bool) -> [a] -> ([a], [a])
-- caso base
dividelistas _ [] = ([], [])  -- Se a lista for vazia, retornamos duas listas vazias.
-- caso geral
dividelistas p (h:t)
  | p h       = ([], h:t)  -- Se a cabeça h satisfaz o predicado p, a primeira parte é vazia e a segunda parte é a lista original.
  | otherwise = let (before, after) = dividelistas p t
                in (h:before, after) -- Caso contrário, h é adicionado à parte before da chamada recursiva.

-- Concatena uma lista de strings em uma única string, adicionando um caractere de nova linha (\n) entre cada string.
unline :: [String] -> String
-- caso base
unline [] = ""
-- Caso recursivo: concatena a cabeça, uma nova linha e a chamada recursiva para a cauda.
unline (h:t) = h ++ "\n" ++ unline t  

-- Verifica se um elemento x está presente em uma lista.
contem :: Eq a => a -> [a] -> Bool
-- caso base
contem x [] = False
-- caso geral
contem x (h:t)
  | x == h = True -- Se o elemento x for igual à cabeça h, ele está contido.
  | otherwise = contem x t -- Caso contrário, verifica recursivamente na cauda da lista.

-- Ela exibe o menu principal do sistema da biblioteca e solicita ao usuário que escolha uma opção, retornando a opção escolhida como uma String.
menuPrincipal :: IO String
menuPrincipal = do
    putStrLn "=== Sistema de Biblioteca ==="
    putStrLn "Escolha uma opção:"
    putStrLn "1. Menu Aluno"
    putStrLn "2. Menu Livro"
    putStrLn "3. Menu Emprestimo"
    putStrLn "0. Sair"
    opcao <- getLine
    return opcao