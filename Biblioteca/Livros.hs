module Biblioteca.Livros where

import System.IO
import Biblioteca.Dados
import Biblioteca.Util

-- tipo algebrico Livro
-- armazena informações sobre um livro especifico
-- deriving show para mostrar na tela, read para ler do arquivo na obter e EQ para comparar
data Livro = Livro { registro :: Int, titulo :: String, edicao :: Int } deriving (Show, Read, Eq)

-- instancia Dado Livro: o modulo Livros.hs torna Livro uma instancia da classe Dado (Biblioteca.Dados).
instance Dado Livro where
    -- Retorna uma lista de pares chave-valor (String, String) que representam os atributos de um livro, como Registro, Título e Edição.
    atributos l = [("Registro", show (registro l)), ("Título", titulo l), ("Edição", show (edicao l))]

    -- transformando um livro em string
    toString a = show a

    -- cadastro: solicita o registro, o titulo e a edicao do livro e salva no arquivo
    cadastrar = do
        putStrLn "Cadastro de Livro"
        putStrLn "Insira o Registro:"
        registro <- getInt
        putStrLn "Insira o Titulo:"
        titulo <- getLine
        putStrLn "Insira a Edição:"
        edicao<- getInt
        let livro = Livro registro titulo edicao
        -- abre o arquivo como appendMode (adiciona ao final do arquivo), se nao existir ele cria
        handle <- openFile "livros.txt" AppendMode
        -- escreve a representação do livro no arquivo
        hPutStrLn handle (show livro)
        -- fecha o arquivo
        hClose handle
        putStrLn "Livro cadastrado com sucesso!"
        -- retorna o livro
        return livro
    
    -- obter: obtem o conjunto de livros do arquivo
    obter = do
        -- abre o arquivo para leitura
        handle <- openFile "livros.txt" ReadMode
        -- lê todo o conteudo do arquivo
        conteudo <- hGetContents handle
        -- divide o conteudo em linhas
        let linhas = splitLines conteudo
            -- mapeia cada linha para um objeto livro usando read
            livros = map read linhas :: [Livro]
            -- insere todos os livros em um Set usando foldr sinserir
            conjunto = foldr sinserir (St []) livros
        -- garante que todos os dados sejam lidos antes de fechar o arquivo
        mapM_ (const (return ())) livros
        hClose handle
        -- retorna o Set de livros
        return conjunto
    
    -- busca o livro pelo numero de registro
    buscar cod = do
        -- obtem todos os livros, atraves da funcao obter
        conjunto <- obter
        -- chama a funcao auxiliar buscarNoSet para encontrar o livro no Set
        -- retorna Maybe Livro, se nao encontrar retorna Nothing
        return (buscarNoSet cod conjunto)

    -- apaga o livro pelo numero de registro
    apagar cod = do
        -- obtem todos os livros, atraves da funcao obter
        lista <- obter
        -- se o livro nao for encontrado retorna Nothing, caso contrario retorna Just a
        case buscarNoSet cod lista of
             Nothing -> return Nothing
             Just a -> do
                let novoSet = sremover a lista
                -- abre o arquivo para escrita e apaga o livro
                handle <- openFile "livros.txt" WriteMode
                hPutStr handle (setToStr novoSet)
                hClose handle
                return (Just a)

    showmenu _ = do
        putStrLn "=== Menu Livro ==="
        putStrLn "0 - Voltar"
        putStrLn "1 - Visualizar"
        putStrLn "2 - Cadastrar"
        putStrLn "3 - Apagar"
        putStrLn "Digite a opção desejada:"
        getLine

buscarNoSet :: Int -> Set Livro -> Maybe Livro
buscarNoSet _ (St []) = Nothing
buscarNoSet cod (St (x:xs))
    | registro x == cod = Just x
    | otherwise = buscarNoSet cod (St xs)
