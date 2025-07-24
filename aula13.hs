import System.IO

-- 1. Crie uma função que solicita do usuário o caminho de um arquivo e retorna o tamanho dele em caracteres.
main :: IO ()
main = do
    putStrLn "Digite o caminho do arquivo"
    caminho <- getLine
    handle <- openFile caminho ReadMode
    contents <- hGetContents handle
    hClose handle
    putStrLn ("Tamanho do arquivo em caracteres = " ++ show (length contents) )
    return ()

-- 2. Crie uma função que solicita do usuário o caminho de um arquivo e retorna o tamanho dele em linhas.
main2 :: IO ()
main2 = do
    putStrLn "Digite o caminho do arquivo"
    caminho <- getLine
    handle <- openFile caminho ReadMode
    contalinha handle 0
    hClose handle
    return ()

contalinha :: Handle -> Int -> IO ()
contalinha handle contador = do
    fim <- hIsEOF handle
    if fim
        then putStrLn ("Tamanho = " ++ show contador)
        else do
            content <- hGetLine handle
            contalinha handle (contador + 1)

-- 3. Crie uma função que solicita do usuário o caminho de um arquivo e permite que ele escreva mais linhas
-- ao final do arquivo. Para finalizar, o usuário deve digitar “EOF”.
main3 :: IO ()
main3 = do
    putStrLn "Digite o caminho do arquivo"
    caminho <- getLine
    handle <- openFile caminho AppendMode
    adiciona handle
    hClose handle
    return ()

adiciona :: Handle -> IO ()    
adiciona handle = do
    putStrLn "Insira a linha ou EOF para sair"
    conteudo <- getLine
    if conteudo == "EOF"
        then return ()
        else do
            hPutStrLn handle conteudo
            adiciona handle


-- 4. Crie uma função que solicita que o usuário digite “sum” para realizar uma soma ou “sub” para
-- realizar uma subtração. Em seguida, ele solicita que o usuário digite dois operandos e realiza a
-- operação selecionada, imprimindo o resultado na tela no formato “a op b = s”, onde op pode ser ‘+’ou ‘-’
-- a e b são os operandos e r é o resultado. O usuário pode repetir quantas vezes quiser até digitar
-- “fim” no lugar da operação. O programa deve escrever em um arquivo todas as operações realizadas
-- no formato “a op b = s”, cada uma em uma linha.
main4 :: IO ()
main4 = do
    putStrLn "Digite sum para soma"
    putStrLn "Ou sub para subtracao"
    op <- getLine
    if op == "fim"
        then return ()
        else do
            putStrLn "Digite o primeiro operando"
            a <- getInt
            putStrLn "Digite o segundo operando"
            b <- getInt
            handle <- openFile "q4" AppendMode
            if op == "sum"
                then do
                    let soma = a + b
                    putStrLn (show a ++ " + " ++ show b ++ " = " ++ show soma)
                    hPutStrLn handle (show a ++ " + " ++ show b ++ " = " ++ show soma)
                    hClose handle 
                    main4
                else do
                    let sub = (subtract a b)
                    putStrLn (show a ++ " - " ++ show b ++ " = " ++ show sub)
                    hPutStrLn handle (show a ++ " - " ++ show b ++ " = " ++ show sub)
                    hClose handle 
                    main4

getInt :: IO Int
getInt = do
    dado <- getLine
    return (read dado :: Int)