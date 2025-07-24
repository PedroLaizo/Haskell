module Biblioteca.Dados where

import Data.Proxy
--  o tipo Proxy, que é usado para passar informações de tipos
-- em tempo de compilação sem precisar de um valor real.

import Biblioteca.Util
-- Criação da classe Dado a, que é usada no programa inteiro, dentro da classe tem as assinaturas das
-- funções que vamos usar

class Dado a where
    -- função que transforma em string
    toString :: a -> String
    -- função que coloca em um tupla chave valor (exemplo: chave = nome, valor = nome_real)
    atributos :: a -> [(String, String)]
    -- função para imprimir.
    -- mapM executa as ações sem salvar o resultado
    -- o () todo = recebe uma tupla (chave, valor) e formata isso usando a função do modulo Util
    imprimir :: a -> IO()
    imprimir x = mapM_ (\(chave, valor) -> putStrLn (formata chave valor)) (atributos x)
    -- função para cadastrar, IO a, retorna o tipo a (Aluno, livro ou emprestimo)
    cadastrar :: IO a
    -- função que obtem todos os itens de um modulo. retorna o Set que é uma Estrutura de Dado feita aqui
    obter :: IO (Set a)
    -- função para buscar um item nos arquivos. Tenho que olhar pra usar ela no tp kkk
    buscar :: Int -> IO (Maybe a)
    -- função para apagar um item no arquivo. Retorna maybe, pois se o aluno tiver emprestimo ativo não pode
    -- ser deletado
    apagar :: Int -> IO (Maybe a)
    -- Função que chama o menu, usa o proxy.
    showmenu :: Proxy a -> IO String

-- Aqui defino a ED, com função de inserir, remover, apagar e buscar
data Set a = St [a]

-- Função que permite que um Set 'a' seja convertido em uma String para exibição.
-- Representação de String de um Set é uma concatenação dos elementos da lista interna separados por um caracter '|'.
instance (Show a) => Show (Set a) where
    show (St []) = "|"
    show (St (h:t)) = "|" ++ show h ++ show (St t)

-- Insere um elemento X no Set. Se o elemento já existir (verificado pela função sbuscar), o conjunto não se altera.
-- Caso contrário, o elemento X é adicionado ao conjunto.
sinserir :: (Dado a) => a -> Set a -> Set a
sinserir x (St xs) = if sbuscar x (St xs) then St xs else St (x:xs)

-- Remove um elemento X do Set. Se o elemento X nao existir, o conjunto nao se altera.
sremover :: (Eq a) => a -> Set a -> Set a
sremover _ (St []) = St []
sremover x (St (h:t))
    | x == h    = St t
    | otherwise = let St t' = sremover x (St t) in St (h : t')

-- Verifica se um elemento X pertence ao Set. A comparação é feita usando a função toString do tipo Dado.
sbuscar :: (Dado a) => a -> Set a -> Bool
sbuscar _ (St []) = False
sbuscar x (St (h:t))
  | toString x == toString h = True
  | otherwise = sbuscar x (St t)

-- Verifica se o Set esta vazio
svazio :: Set a -> Bool
svazio (St []) = True
svazio _ = False

-- Converte um Set para uma String, onde cada elementos do conjunto é separada na String resultante.
-- Ele utiliza a função show de cada elemento e unlines para formatar a saida.
setToStr :: (Show a) => Set a -> String
setToStr (St xs) = unline (map show xs)