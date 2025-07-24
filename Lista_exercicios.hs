-- 1
-- Escreva uma função que leia três números (Float ou Double) e calcule a média.
media a b c = (a + b + c) / 3

-- 2
-- Faça uma função que calcule o volume de uma esfera.
esfera raio = (4 * pi * raio**3) / 3

-- 3
-- O custo ao consumidor de um carro novo é a soma do custo de fábrica coma percentagem do distribuidor e dos impostos
-- (aplicados ao custo de fábrica). Supondo que a percentagem do distribuidor seja de 28% e os impostos de 45%,
-- escrever uma função que leia o custode fábricade um carro e escreva o custo ao consumidor. 
valor_carro fabrica = fabrica + (fabrica * 0.28) + (fabrica * 0.45) 

-- 4
-- Fornecidos três valores, a, b e c, elaborar uma função que retorne quantos desses três númerossão maiores que o valor médio entre eles
maior a b c
    |a > (media a b c) && b > (media a b c) && c > (media a b c) = 3
    |a > (media a b c) && (b > (media a b c) || c > (media a b c)) = 2
    |a > (media a b c) || b > (media a b c) || c > (media a b c) = 1
    |otherwise = 0

-- 5
-- Faça uma função que solicite um tempo em segundos e converta para horas, minutos e segundos, imprimindo no formato h:m:s.
horario tempo = show(div tempo 3600) ++ ":" ++ show(div (mod tempo 3600) 60) ++ ":" ++ show(mod(mod tempo 3600) 60)

-- 6
--Fazer uma função que solicite 3 inteiros e retorna uma String comos números em ordem decrescente.
decrescente a b c
    |(a > b) && (b > c) = show(a) ++ " " ++ show(b) ++ " " ++ show(c)
    |(a > c) && (c > b) = show(a) ++ " " ++ show(c) ++ " " ++ show(b)
    |(b > a) && (b > c) = show(b) ++ " " ++ show(b) ++ " " ++ show(c)