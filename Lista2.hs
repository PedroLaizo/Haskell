import Prelude
import Data.List
import Data.Char

-- 1
-- Implemente uma função que recebe uma lista do tipo Double e retorne a média nos elementos dessa lista.
media :: [Double] -> Double
media [] = 0
media (a:b) = sum(a:b) / fromIntegral(length(a:b))

-- 2
-- Implemente uma função que inverte os elementos de uma lista (não use a função reverse).
inverte [] = []
inverte (h:t) = inverte t ++ [h]

-- 3
-- Implemente uma função que informe o n-ésimo membro de uma lista (não use o operador !!).
elementoN lista 0 = 0
elementoN lista x = (last (take x lista))

-- 4
-- Implemente uma função que recebe uma String e a retorna com todas as letras em maiúscula.
maiuscula :: String -> String
maiuscula [] = []
maiuscula str = map toUpper str

-- 5
-- Implemente uma função que recebe uma frase (String) e a retorna as letras iniciais de cada palavra em maiúscula.
incial_maiuscula :: String -> String
incial_maiuscula [] = []
--incial_maiuscula 

-- 6
-- Implemente uma função que recebe uma String e retorna outra somente com as letras.
soletras :: String -> String
soletras [] = []
soletras (h:t)
    | elem h ['A'..'Z'] || elem h ['a'..'z'] = h : soletras t 
    | otherwise = soletras t

-- 7
-- Implemente uma função que recebe uma lista e um inteiro t e retorna uma lista de listas
-- que corresponde a lista original dividida em listas de tamanho t.
dividida :: [Int] -> Int -> [[Int]]
dividida [] _ = []
dividida lista x = take x lista : dividida (drop x lista) x

-- 8
-- Implemente uma função que verifica se uma lista é ou não um palíndromo.
-- Um palíndromo é uma lista que se invertida se mantém exatamente igual à original.
palindromo [] = True
palindromo [a] = True
palindromo lista 
    | head lista == last lista = palindromo(init(tail lista))
    | otherwise = False

-- 9
-- Implemente uma função que recebe um inteiro e retorna uma lista de todos os 
-- números primos iguais ou inferiores a esse inteiro.
primos :: Int -> [Int]
primos n = aux_primos n 2
    where
        aux_primos :: Int -> Int -> [Int]
        aux_primos n d
            | d > n = []
            | primo d = d : aux_primos n (d+1)
            | otherwise = aux_primos n (d+1)

primo :: Int -> Bool
primo n = primo_aux n 2
    where
        primo_aux :: Int -> Int -> Bool
        primo_aux n d
            | d > div n 2 = True 
            | mod n d == 0 = False
            | otherwise = primo_aux n (d+1)


-- 10
-- Implemente uma função que recebe uma lista de listas e retorna uma lista normal com todosos
-- elementos das listas internas (não use a função concat).
concatena :: [[Int]] -> [Int]
concatena [] = []
concatena (h:t) = h ++ concatena t

-- 11
-- Implemente uma função que recebe uma lista de listas do tipo Double
-- e retorna um lista com a média dos elementos de cada lista interna.
mediaLista :: [[Double]] -> [Double]
mediaLista [] = []
mediaLista [[a]] = [a]
mediaLista (h:t) = sum h / fromIntegral (length(h)) : mediaLista t