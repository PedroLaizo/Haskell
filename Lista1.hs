-- 1
-- Crie uma função que recebe o raio (Double) de um círculo e retorna o perímetro dele.
perimetro :: Double -> Double
perimetro raio = 2*(pi * r)

-- 2
-- Crie uma função que recebe 3 valores Float e calcula a média entre eles.
mediaFloat :: Float -> Float -> Float -> Float
mediaFloat a b c = (a+b+c) / 3

-- 3
-- Crie uma função que recebe um Char e indica se é um dígito ou não.
isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'

-- 4
-- Crie uma função que converte uma letra mínuscula (Char) para maiúscula.
toUp :: Char -> Char
toUp c = if c >= 'a' && c <= 'z'
    then toEnum (fromEnum c - 32)
    else c

-- 5
-- Crie um operador que calcula a média entre 2 valores do tipo Double.
mm :: Double -> Double -> Double
valor1 `mm` valor2 = (valor1 + valor2) / 2

-- 6
-- Crie uma função que calcula a soma entre dois inteiros (conjunto fechado).
somatorio :: Int -> Int -> Int
somatorio a b =
    if a == b
    then a
    else
        if a > b
        then b + somatorio a (b+1)
        else a + somatorio (a+1) b
            
-- 7
-- Crie um operador que calcula a média dos números entre dois inteiros (conjunto fechado).
somatorio2 :: Int -> Int -> Int
somatorio2 a b = div (somatorio a b) 2 

-- 8
-- Crie uma função que calcula a potência entre dois inteiros (não use o operador ^).
pow :: Int -> Int -> Int
pow a 1 = a
pow 1 b = 1
pow a b = a * pow a (b-1)

-- 9
-- Crie um operador que calcula o resto da divisão entre inteiros positivos (não use as funções mod ou rem).
(%%) :: Int -> Int -> Int
a %% b
    | a < b = a
    | otherwise = (a-b) %% b

-- 10
-- Crie uma função que calcula o mdc entre dois inteiros positivos (não use a função gcd).

-- 11
-- Crie uma função que calcula o mmc entre dois inteiros positivos (não use a função lcm).

-- 12
-- Crie uma função que recebe um Int e retorna quantos algarismos ele possui.
algarismos :: Int -> Int
algarismos a = if a >= 0 && a <= 9
    then 1
    else 1 + algarismos (a `div` 10)

-- 13
-- Crie uma função que recebe dois Int, sendo um primeiro um inteiro positivo ou negativo com qualquer
-- quantidade de algarismos e o segundo um único algarismo (positivo). Afunção deve retornar quantas vezes
-- o segundo algarismo aparece no primeiro inteiro.
aparece :: Int -> Int -> Int
aparece a b
    | a == 0 = 0 
    | a `mod` 10 == b = 1 + aparece (a `div` 10) b
    | otherwise = aparece (a `div` 10) b

-- 14
-- Crie uma função que recebe um inteiro positivo e retorna uma String que correpondeaointeiro convertido para binário.
binario :: Int -> String
binario a 
    | a < 0     = "Numero menor que 0"
    | a == 0    = "0"
    | otherwise = binario (a `div` 2) ++ show (a `mod` 2)

-- 15
-- Crie uma função que calcule a soma dos algarismos de um número inteiro. Por exemplo,se a entrada for 123,
-- a saída deverá ser 1+2+3 = 6.
soma_algarismo :: Int -> Int
soma_algarismo a
    | a < 10    = a
    | otherwise = (a `mod` 10) + soma_algarismo (a `div` 10)

-- 16
-- Crie uma função que calcula um elemento da série de Ackermann, que recebe doisinteirosnão negativos
-- e é definida por:
funcao :: Int -> Int -> Int
funcao a b
    | a == 0 = b+1
    | a > 0 && b == 0 = funcao (a-1) 1
    | a > 0 && b > 0 = funcao (a-1) (funcao a (b-1))
