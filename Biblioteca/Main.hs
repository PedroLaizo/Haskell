module Main where

import Data.Proxy
import System.IO
import Biblioteca.Alunos
import Biblioteca.Livros
import Biblioteca.Emprestimo
import Biblioteca.Util 
import Biblioteca.Dados

-- menu principal
main :: IO ()
main = do
    -- primeira ação da main é chamar menuPrincipal
    opcao <- menuPrincipal
    -- em seguida, dependendo da opcao, chamar o menu correspondente
    case opcao of
        -- caso seja 1, chamar o menu de alunos
        "1" -> do
            op <- showmenu (Proxy :: Proxy Aluno)
            case op of
                -- caso seja 1, visualizar alunos
                "1" -> do
                    --obtem todos os alunos usando obter
                    alunos <- obter :: IO (Set Aluno)
                    -- imprime todos os alunos
                    imprimirTodos alunos
                    -- volta para o menu principal
                    main
                "2" -> do
                    -- caso seja 2, cadastrar aluno
                    cadastrar :: IO Aluno
                    -- volta para o menu principal
                    main
                -- caso seja 3, apagar aluno 
                "3" -> do
                    -- pede o codigo do aluno para apagar
                    putStrLn "Informe o cod do Aluno"
                    -- pega o codigo digitado
                    cod <- getInt
                    -- verifica se o aluno tem emprestimo
                    tem <- temEmprestimo cod 
                    if tem
                        -- caso tenha, informa ao usuario e volta para o menu principal
                        then do
                            putStrLn "Aluno tem emprestimo ativo, não é possivel apagar!"
                            main
                        -- caso nao tenha, apaga o aluno e volta para o menu principal
                        else do
                            putStrLn "Apagando"
                            apagar cod :: IO (Maybe Aluno)
                            main
                -- caso seja 0, volta para o menu principal
                "0" -> main
                -- caso seja outra coisa, informa ao usuario e volta para o menu principal
                _ -> do
                    putStrLn "Opção invalida"
                    main
        -- caso seja 2, chamar o menu de livros 
        "2" -> do
            op <- showmenu (Proxy :: Proxy Livro)
            case op of
                -- obtem todos os livros, atraves da funcao obter
                "1" -> do
                    livros <- obter :: IO (Set Livro)
                    -- imprime todos os livros
                    imprimirTodos livros
                    -- volta para o menu principal
                    main
                -- cadastra um livro
                "2" -> do
                    cadastrar :: IO Livro
                    -- volta para o menu principal
                    main
                -- apaga um livro
                "3" -> do
                    -- pede o reg do livro
                    putStrLn "Informe o reg do livro"
                    -- pega o reg digitado
                    reg <- getInt
                    -- verifica se o livro tem emprestimo
                    tem <- temEmprestimo reg
                    if tem
                        -- caso tenha, informa ao usuario e volta para o menu principal
                        then do
                            putStrLn "Livro está emprestado, não é possivel apagar"
                            main
                        -- caso nao tenha, apaga o livro e volta para o menu principal
                        else do
                            putStrLn "Apagando"
                            apagar reg :: IO (Maybe Livro)
                            main
                -- volta para o menu principal
                "0" -> main
                -- caso seja outra coisa, informa ao usuario e volta para o menu principal
                _ -> do
                    putStrLn "Opção invalida"
                    main
        -- caso seja 3, chamar o menu de emprestimos
        "3" -> do
            op <- showmenu (Proxy :: Proxy Emprestimo)
            case op of
                -- obtem todos os emprestimos
                "1" -> do
                    -- obtem todos os emprestimos
                    emprestimos <- obter :: IO (Set Emprestimo)
                    -- imprime todos os emprestimos
                    imprimirTodos emprestimos
                    -- volta para o menu principal
                    main
                -- cadastra um emprestimo
                "2" -> do
                    cadastrar :: IO Emprestimo
                    main
                -- apaga um emprestimo
                "3" -> do
                    -- pede o cod do emprestimo
                    putStrLn "Informe o cod do emprestimo"
                    -- pega o cod digitado
                    cod <- getInt
                    -- apaga o emprestimo
                    putStrLn "Apagando"
                    apagar cod :: IO (Maybe Emprestimo)
                    -- volta para o menu principal
                    main
                -- volta para o menu principal
                "0" -> main
                -- caso seja outra coisa, informa ao usuario e volta para o menu principal
                _ -> do
                    putStrLn "Opção invalida"
                    main
        -- Sai do programa
        "0" -> putStrLn "Saindo ..."
        _ -> do
            putStrLn "Opção invalida"
            main
        
        
imprimirTodos :: Dado a => Set a -> IO ()
imprimirTodos (St []) = putStrLn "(Nenhum registro encontrado)"
imprimirTodos (St (h:t)) = do
    putStrLn ""
    imprimir h
    putStrLn "-----------------------------"
    imprimirTodos (St t)
