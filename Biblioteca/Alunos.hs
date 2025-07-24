module Biblioteca.Alunos where

-- importo os dados e o modulo util
import System.IO
import Biblioteca.Dados
import Biblioteca.Util

-- crio o tipo algebrico Aluno, deriving show para mostrar na tela, read para ler do arquivo na função
-- obter e EQ para comparar tipos 
data Aluno = Aluno { codigo :: Int, nome :: String, email :: String } deriving (Show, Read, Eq)

-- agora é implementar cada função criada nos dados
instance Dado Aluno where
    -- fazendo a tupla de atributos
    atributos a = [("Código", show (codigo a)), ("Nome", nome a), ("Email", email a)]

    -- transformando em string
    toString a = show a

    -- cadastro
    -- peço os dados do aluno e depois salvo no arquivo
    cadastrar = do
        putStrLn "Cadastro de Aluno"
        putStrLn "Insira o Código:"
        -- getInt é uma função que eu crei no Util, que pega int e não string normal
        codigo <- getInt
        putStrLn "Insira o Nome:"
        nome <- getLine
        putStrLn "Insira o E-mail:"
        email <- getLine
        -- crio um aluno, como se fosse struct
        let aluno = Aluno codigo nome email
        -- abri o arquivo como append, se não tiver ele cria, se ja existir ele apenas abre.
        handle <- openFile "alunos.txt" AppendMode
        hPutStrLn handle (show aluno)
        hClose handle
        putStrLn "Aluno cadastrado com sucesso!"
        return aluno
    
    -- função para obter a lista de alunos
    -- abro o arquivo e pego todo o conteudo
    obter = do
        handle <- openFile "alunos.txt" ReadMode
        conteudo <- hGetContents handle
        let linhas = splitLines conteudo
            alunos = map read linhas :: [Aluno]
            -- Insere na Estrutura de dados
            conjunto = foldr sinserir (St []) alunos
        -- função que segura para ler tudo antes de fechar
        mapM_ (const (return ())) alunos
        hClose handle
        return conjunto

    -- Busca no arquivo por cod
    buscar cod = do
        conjunto <- obter
        return (buscarNoSet cod conjunto)

    -- função que apaga o item
    apagar cod = do
        -- começa pegando todos os itens e salvando na lista
        lista <- obter
        -- busca no Set se tem algum cod. 
        case buscarNoSet cod lista of
             Nothing -> return Nothing
             Just a -> do
                -- achou e removeu, abrindo e fechando o arquivo
                let novoSet = sremover a lista
                handle <- openFile "alunos.txt" WriteMode
                hPutStr handle (setToStr novoSet)
                hClose handle
                return (Just a)

    -- menu do aluno
    showmenu _ = do
        putStrLn "=== Menu Aluno ==="
        putStrLn "0 - Voltar"
        putStrLn "1 - Visualizar"
        putStrLn "2 - Cadastrar"
        putStrLn "3 - Apagar"
        putStrLn "Digite a opção desejada:"
        getLine


-- função que busca no set, recebe o cod, a lista de alnos e retorna talvez aluno        
buscarNoSet :: Int -> Set Aluno -> Maybe Aluno
buscarNoSet _ (St []) = Nothing
buscarNoSet cod (St (x:xs))
    | codigo x == cod = Just x
    | otherwise = buscarNoSet cod (St xs)
