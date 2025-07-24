module Biblioteca.Emprestimo where --define o modulo

import System.IO
import Biblioteca.Alunos
import Biblioteca.Livros
import Biblioteca.Util
import Biblioteca.Dados

-- crio o tipo algebrico Emprestimo
data Emprestimo = Emprestimo {
    numero :: Int,
    aluno :: Aluno,
    dataEmp :: Data,
    dataDev :: Data,
    livros :: [Livro]
} deriving (Show, Read, Eq) --deriva as instancias permitindo mostrar, ler e comparar

-- O modulo Emprestimo.hs faz de Emprestimo uma instancia da classe Dado (Definida em Bilbioteca.Dados).
-- Significa que Emprestimo deve imprementar todas as funções definidas na classe Dado.
instance Dado Emprestimo where
    -- retorna uma lista de pares chave-valor (String, String), representando os atributos do emprestimo.
    -- Inclui informações do número do emprestimo, código, nome e e-mail do aluno, data de empréstimo e data de devolução.
    -- Também inclui informações sobre os livros emprestados.
    
    atributos e = 
        [ ("Número", show (numero e)),
          ("Aluno - Código", show (codigo (aluno e))),
          ("Aluno - Nome", nome (aluno e)),
          ("Aluno - Email", email (aluno e)),
          ("Data de Empréstimo", dataStr (dataEmp e)),
          ("Data de Devolução", dataStr (dataDev e))
        ] ++ concatMap atributosLivro (livros e)
    
    -- transformando em string
    toString a = show a

    -- cadastro
    cadastrar = do
        putStrLn "Cadastro de Empréstimo"
        putStrLn "Insira o número do empréstimo:"
        num <- getInt
        -- para obter o aluno associado ao emprestimo
        aluno <- buscarAluno
        -- para obter os livros associados ao emprestimo
        livros <- buscarLivros []
        -- solicita as datas de emprestimo e devolucao, atraves da funcao lerData de Biblioteca.Util
        putStrLn "Insira a data de empréstimo (dia, mes, ano):"
        dataEmp <- lerData
        putStrLn "Insira a data de devolução (dia, mes, ano):"
        dataDev <- lerData
        -- criando o objeto emprestimo com os dados coletados
        let emprestimo = Emprestimo num aluno dataEmp dataDev livros
        -- abre o arquivo como appendMode (adiciona ao final do arquivo), se nao existir ele cria
        handle <- openFile "emprestimos.txt" AppendMode
        -- escreve a representação do emprestimo no arquivo
        hPutStrLn handle (show emprestimo)
        -- fecha o arquivo
        hClose handle
        putStrLn "Empréstimo cadastrado com sucesso!"
        return emprestimo
    
    obter = do
        -- abre o arquivo para leitura
        handle <- openFile "emprestimos.txt" ReadMode
        -- lê todo o conteudo do arquivo
        conteudo <- hGetContents handle
        let linhas = splitLines conteudo
            -- divide o conteudo em linhas e mapeia cada linha para um objeto emprestimo
            emprestimos = map read linhas :: [Emprestimo]
            -- insere todos os emprestimos em um Set usando foldr sinserir.
            conjunto = foldr sinserir (St []) emprestimos
        -- garante que todos os dados sejam lidos antes de fechar o arquivo
        mapM_ (const (return ())) emprestimos
        hClose handle 
        return conjunto
    
    -- busca o emprestimo por numero do codigo
    buscar cod = do
        -- obtem todos os emprestimos
        conjunto <- obter
        -- chama a função auxiliar buscarNoSetEmprestimo para encontrar o emprestimo no Set.
        -- retorna Maybe Emprestimo
        return (buscarNoSetEmprestimo cod conjunto)

    -- apaga o emprestimo pelo numero do codigo
    apagar cod = do
        -- obtem a lista de todos os emprestimos
        lista <- obter
        -- tenta buscar o emprestimo pelo codigo
        case buscarNoSetEmprestimo cod lista of -- se o emprestimo for encontrado, (Just a), remove o emprestimo do Set
             Nothing -> return Nothing
             Just a -> do
                let novoSet = sremover a lista
                -- abre o arquivo para escrita e apaga o emprestimo do codigo solicitado
                handle <- openFile "emprestimos.txt" WriteMode
                -- novo Set (sem o emprestimo) eh escrito no arquivo
                hPutStr handle (setToStr novoSet)
                -- o arquivo é fechado e o emprestimo apagado é retornado em um Just
                -- se o emprestimo nao for encontrado, retorna Nothing
                hClose handle
                return (Just a)

    -- exibe o menu do emprestimo
    showmenu _ = do
        putStrLn "=== Menu Empréstimo ==="
        putStrLn "0 - Voltar"
        putStrLn "1 - Visualizar"
        putStrLn "2 - Cadastrar"
        putStrLn "3 - Apagar"
        putStrLn "Digite a opção desejada:"
        getLine

-- auxiliar: busca o emprestimo (dentro de um Set Emprestimo) pelo numero do codigo.
-- percorre recursivamente a lista interna do Set ate encontrar o emprestimo ou esgotar a lista.
buscarNoSetEmprestimo :: Int -> Set Emprestimo -> Maybe Emprestimo
buscarNoSetEmprestimo _ (St []) = Nothing
buscarNoSetEmprestimo cod (St (x:xs))
    | numero x == cod = Just x
    | otherwise = buscarNoSetEmprestimo cod (St xs)

-- Função para buscar o aluno (loop até encontrar)
-- solicita o codigo do aluno, usa a funcao buscar (da instancia Dado Aluno) para buscar o aluno
-- caso o aluno seja encontrado, retorna o aluno
-- caso contrario, informa ao usuario e chama a si mesma recursivamente ate encontrar um aluno.
buscarAluno :: IO Aluno
buscarAluno = do
    putStrLn "Insira o código do aluno:"
    cod <- getInt
    resultado <- buscar cod :: IO (Maybe Aluno)
    case resultado of
        Just a  -> return a
        Nothing -> do
            putStrLn "Aluno não encontrado, tente novamente."
            buscarAluno

-- Função para buscar livros (loop para múltiplos livros)
-- Caso SIM, solicita o codigo do livro, usa a funcao buscar (da instancia Dado Livro) para buscar o livro
-- caso o livro seja encontrado, adiciona na lista de livros atuais e chama buscarLivros recursivamente com a lista atualizada
-- caso contrario, informa ao usuario e chama a si mesma recursivamente com a lista atual.
-- Caso NAO, retorna a lista de livros acumulada
-- para qualquer outra resposta, informa que é invalida e chama a buscarLivros recursivamente.
buscarLivros :: [Livro] -> IO [Livro]
buscarLivros livrosAtuais = do
    putStrLn "Deseja adicionar um livro? (s/n)"
    resp <- getLine
    case resp of
        "s" -> do
            putStrLn "Insira o registro do livro:"
            reg <- getInt
            resultado <- buscar reg :: IO (Maybe Livro)
            case resultado of
                Just l  -> buscarLivros (livrosAtuais ++ [l])
                Nothing -> do
                    putStrLn "Livro não encontrado, tente novamente."
                    buscarLivros livrosAtuais
        "n" -> return livrosAtuais
        _   -> do
            putStrLn "Resposta inválida, digite 's' ou 'n'."
            buscarLivros livrosAtuais
    
-- auxiliar: retorna o conjunto de atributos do livro em formato de chave-valor
atributosLivro :: Livro -> [(String, String)]
atributosLivro l =
    [ ("Livro - Registro", show (registro l)),
      ("Livro - Título", titulo l),
      ("Livro - Edição", show (edicao l))
    ]

-- Função que verifica se tem emprestimo dado um cod
temEmprestimo :: Int -> IO Bool
temEmprestimo codigoBuscado = do
    emprestimos <- obter :: IO (Set Emprestimo)
    return (existeEmprestimo codigoBuscado emprestimos)

-- Função auxiliar para verificar se o código está em algum empréstimo ativo
existeEmprestimo :: Int -> Set Emprestimo -> Bool
existeEmprestimo _ (St []) = False
existeEmprestimo cod (St (e:es))
    | cod == codigo (aluno e) = True
    | cod `contem` map registro (livros e) = True
    | otherwise = existeEmprestimo cod (St es)