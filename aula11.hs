-- 1. Defina uma função process :: [Int] -> Int -> Int -> Int de forma que process lst n m toma o n-ésimo
-- e o m-ésimo elementos da lista lst e retorna a soma de todos os elementos entre n e m. A função deve
-- retornar zero se quaisquer dos números n ou m não forem índices da lista lst ou se n for maior que m.

process :: [Int] -> Int -> Int -> Int
process [] _ _ = 0
process lst n m = if n > m
    then 0
    else somador 0 (take (m - n + 1) (drop n lst))

somador :: Int -> [Int] -> Int
somador _ [] = 0
somador count (h:t) = (count + h) + somador count t

process2 :: [Int] -> Int -> Int -> Int
process2 [] _ _ = 0
process2 lst n m = if n > m
    then 0
    else sum (take (m - n + 1) (drop n lst))

-- 2. Crie uma função polimórfica que recebe uma lista e um inteiro i e retorna a substring da 
-- posição 0 até a posição (i-1). A função deve lançar uma exceção e encerrar a execução caso i
-- seja negativo. Se i for maior que o tamanho da lista, deve-se retornar a lista inteira.
substring :: [a] -> Int -> [a]
substring lst i
    | i < 0 = error "Numero negativo"
    | (i - 1) < 0 = error "i muito pequeno"
    | i > (length lst) = lst
    | otherwise = take (i - 1) lst 


-- 3. Crie uma função polimórfica que recebe uma lista de funções f e uma lista elementos a retorna
-- uma lista do tipo Maybe b, cujos elementos são: [Just f1 a1, Just f2 a2, …, Just fn an]. Caso as 
-- listas de funções e de elementos tenham tamanhos diferentes, o resultado deve ser Nothing para os
-- elementos que não possuam um correspondente na outra lista.

getLista :: [a -> b] -> [a] -> [Maybe b]
getLista [] [] = []
getLista (_:t) [] = Nothing : getLista t []
getLista [] (_:cd) = Nothing : getLista [] cd
getLista (h:t) (cbc:cd) = Just (h cbc) : getLista t cd