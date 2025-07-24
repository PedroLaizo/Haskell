-- 1. Crie funções que fazem a mesma coisa que as funções take e drop usando tipos polimórficos.
functake :: [a] -> Int -> [a]
functake _ 0 = []
functake (h:t) n = h : functake t (n-1)

funcdrop :: [a] -> Int -> [a]
funcdrop lista 0 = lista
funcdrop (h:t) n = funcdrop t (n-1)

-- 2. Crie uma função que faz a mesma coisa que o operador ++ usando tipos polimórficos.
funcconcat :: [a] -> [a] -> [a]
funcconcat [] lista2 = lista2
funcconcat (h:t) lista2 = h : funcconcat t lista2


-- 3. Crie uma função que faz a mesma coisa que a função last usando tipos polimórficos
-- e retornando Nothing caso receba uma lista vazia.
funclast :: [a] -> Maybe a
funclast [] = Nothing
funclast [a] = Just a
funclast (h:t) = funclast t 


-- 4. Crie uma função que recebe uma lista e retorne o menor elemento. Use tipos qualificados
-- e trate a possibilidade da lista ser vazia.
funcmenor :: Ord a => [a] -> Maybe a
funcmenor [] = Nothing
funcmenor lista = Just (minimum lista)


-- 5. Crie uma função que implementa o algoritmo bubblesort sobre uma lista de qualquer tipo ordenado.
-- funcbubble :: Ord a => [a] -> [a]
