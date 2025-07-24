-- 1
-- Crie uma função que inverte uma lista usando recursão em cauda.


-- 2 Crie uma função que recebe uma lista de notas (Float) de alunos,
-- calcula a média e retorna uma String como resultado, sendo que: se a média < 4.0,
-- o resultado é “REPROVADO”; se a 4.0 <= média < 6.0, o resultado é “RECUPERACAO”;
-- e se média >= 6.0, o resultadoé“APROVADO”.
notas :: [Float] -> String
notas [] = "Reprovado"
notas lista
    | (sum lista) / fromIntegral(length lista) < 4 = "Reprovado"
    | (sum lista) / fromIntegral(length lista) >= 4 && (sum lista) / fromIntegral(length lista) < 6 = "Recuperacao"
    | (sum lista) / fromIntegral(length lista) >= 6 = "Aprovado"

-- 3
-- Crie uma função que recebe um inteiro n e retorna uma lista com os n primeiros
-- elementos da série de Fibonacci.
fibo :: Int -> [Int]
fibo n = fiboaux n 0 1
    where
        fiboaux :: Int -> Int -> Int -> [Int]
        fiboaux 0 _ _ = []
        fiboaux n a b = a : fiboaux (n-1) b (a+b)


-- 4
-- Crie a função equacao 2 grau a b c que retorna as raízes reais da equação do segundo grau
-- (ax² + bx + c = 0) em uma lista (a não pode ser zero).
-- Use definição local para calcular o delta
segundograu :: Float -> Float -> Float -> [Float]
segundograu 0 _ _ = []
segundograu a b c 
    | delta < 0 = []
    | delta == 0 = [raiz]
    | delta > 0 = [raiz1, raiz2]
        where
            delta = sqrt ((b * b) - (4 * a * c))
            raiz = -b / (2 * a)
            raiz1 = (b *(-1) + delta) /  (2 * a)
            raiz2 = (b *(-1) - delta) /  (2 * a)


--5
-- Crie uma função que recebe um inteiro n e uma lista e retorna uma lista de listas que corresponde
-- à lista original dividida em n partes.

listaparte :: Int -> [a] -> [[a]]
listaparte 0 [] = [[]]
listaparte n (h:t) 
    | n == 1 = [h] : listaparte n t
    | n > 1 = listacorte n 1 (h:t) : listaparte n t
    where
        listacorte :: Int -> Int -> [a] -> [a]
        listacorte n contador (h:t) 
            | n == contador = [h]
            | n < contador = h : listacorte n (contador + 1) t