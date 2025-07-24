-- 1. Explique o que faz o código sequence (map print [1,2,3,4,5])?

-- 2. Altere a função leiaAte (slide 15), acrescentando informações de saída para tornar a função mais
-- legível ao usuário. Inclua uma função main para chamar leiaAte e compile o programa.
leiaAte :: IO ( )
leiaAte = do
        putStr "Digite seu nome ou fim para encerrar: "
        nome <- getLine
        if nome == "fim"
            then return ()
            else do putStr "Digite a primeira nota: "
                    n1 <- getDouble
                    putStr "Digite a segunda nota: "
                    n2 <- getDouble
                    putStr "Digite a terceira nota: "
                    n3 <- getDouble
                    putStrLn ("Nome: " ++ nome ++ "\tMedia: " ++ show ((n1+n2+n3)/3.0))
                    leiaAte

getDouble :: IO Double
getDouble = do
    dado <- getLine
    return (read dado :: Double)

-- 3. Crie uma função que solicita do usuário uma string, imprime ela invertida e informa se a mesma
-- é um palíndromo ou não.

palindromo :: IO()
palindromo = 
    do putStr "Digite uma String: "
       str <- getLine
       putStrLn (reverse str)
       if ispalindromo str
        then putStrLn "É palíndromo!"
        else putStrLn "Não é palíndromo!"

ispalindromo :: String -> Bool
ispalindromo [] = True
ispalindromo [a] = True
ispalindromo lista 
    | head lista == last lista = ispalindromo(init(tail lista))
    | otherwise = False

-- 4. Crie uma função que solicita do usuário uma sequência de 3 inteiros e imprime se cada um é par ou ímpar.
getInt :: IO Int
getInt = do
    dado <- getLine
    return (read dado :: Int)

seq3num :: IO()
seq3num = 
    do putStr "Digite o primeiro número: "
       x <- getInt
       if odd x then putStrLn (show x ++ " É ímpar") else putStrLn (show x ++ " É par")
       putStr "Digite o segundo número: "
       y <- getInt
       if odd y then putStrLn (show y ++ " É ímpar") else putStrLn (show y ++ " É par")
       putStr "Digite o terceiro número: "
       z <- getInt
       if odd z then putStrLn (show z ++ " É ímpar") else putStrLn (show z ++ " É par")
       return()
       


-- 5. Faça um programa compilado que mostre ao usuário um menu com as opções sair do programa,
-- somar 2 números e multiplicar 2 números e fatorial de um inteiro.
-- O programa só encerra se ousuário optar por sair.

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Programa Funcional"
    putStrLn "\tMenu"
    putStrLn "Somar: Soma dois numeros"
    putStrLn "Multi: Multiplica dois numeros"
    putStrLn "Fat: Fatorial de um numero"
    putStrLn "Sair: sair do programa"
    putStr "Digite a opção: "
    escolha <- getLine
    case escolha of
        "Somar" -> somar
        "Multi" -> multi
        "Fat"   -> fat
        "Sair"  -> return ()
        _       -> main

somar :: IO ()
somar = do
    putStr "Digite o primeiro numero: "
    x <- getLine
    putStr "Digite o segundo numero: "
    y <- getLine
    let a = read x :: Int
    let b = read y :: Int
    putStrLn ("A soma de " ++ show a ++ " e " ++ show b ++ " é = " ++ show (a + b))
    main

multi :: IO ()
multi = do
    putStr "Digite o primeiro numero: "
    x <- getLine
    putStr "Digite o segundo numero: "
    y <- getLine
    let a = read x :: Int
    let b = read y :: Int
    putStrLn ("A multiplicação de " ++ show a ++ " por " ++ show b ++ " é = " ++ show (a * b))
    main

fat :: IO ()
fat = do
    putStr "Digite o numero: "
    x <- getLine
    let n = read x :: Int
    putStrLn ("Resultado do fatorial de " ++ show n ++ " é = " ++ show (resulFat n))
    main

resulFat :: Int -> Int
resulFat 0 = 1
resulFat n = n * resulFat (n - 1)