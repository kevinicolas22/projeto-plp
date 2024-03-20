module MenuFuncionario where

import System.IO
import Funcionario
import System.Directory
import System.Exit
import FuncionarioService
import Data.Maybe (mapMaybe)

-- Função principal
menuFuncionario :: IO ()
menuFuncionario = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Adicionar um funcionário"
    putStrLn "2. Ler informações de um funcionário por Id"
    putStrLn "3. Atualizar um funcionário"
    putStrLn "4. Remover um funcionário"
    putStrLn "5. Listar todos os funcionarios"
    putStrLn "6. Sair"
    opcao <- getLine
    case opcao of
        "1" -> adicionarFuncionarioOpcao
        "2" -> lerFuncionarioOpcao
        "3" -> atualizarFuncionarioOpcao
        "4" -> removerFuncionarioOpcao
        "5" -> lerTodosFuncionarios
        "6" -> putStrLn "Saindo..."
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

-- Opção para adicionar um funcionário
adicionarFuncionarioOpcao :: IO ()
adicionarFuncionarioOpcao = do
    novoFuncionario <- criarFuncionario
    adicionarFuncionario novoFuncionario
    putStrLn "Funcionário adicionado com sucesso!"
    main

-- Opção para ler todos funcionarios
lerTodosFuncionarios :: IO()
lerTodosFuncionarios = 
    listarTodosFuncionarios >> main

-- Opção para ler informações de um funcionário por id
lerFuncionarioOpcao :: IO ()
lerFuncionarioOpcao = do
    putStrLn "Digite o ID do funcionário que deseja buscar:"
    id <- getLine
    lerFuncionarioPorId (read id)
    main  

-- Opção para atualizar um funcionário
atualizarFuncionarioOpcao :: IO ()
atualizarFuncionarioOpcao = do
    putStrLn "Digite o ID do funcionário que deseja atualizar:"
    id <- getLine
    putStrLn "Escolha o dado do funcionário a ser atualizado:"
    putStrLn "1. Nome"
    putStrLn "2. CPF"
    putStrLn "3. Endereço"
    putStrLn "4. Telefone"
    putStrLn "5. Data de Ingresso"
    putStrLn "6. Salário"
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite o novo nome:"
            novoNome <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = novoNome, cpf = "", endereco = "", telefone = "", data_ingresso = "", salario = 0.0})
        "2" -> do
            putStrLn "Digite o novo CPF:"
            novoCPF <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = novoCPF, endereco = "", telefone = "", data_ingresso = "", salario = 0.0})
        "3" -> do
            putStrLn "Digite o novo endereço:"
            novoEndereco <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = novoEndereco, telefone = "", data_ingresso = "", salario = 0.0})
        "4" -> do
            putStrLn "Digite o novo telefone:"
            novoTelefone <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = novoTelefone, data_ingresso = "", salario = 0.0})
        "5" -> do
            putStrLn "Digite a nova data de ingresso:"
            novaData <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = "", data_ingresso = novaData, salario = 0.0})
        "6" -> do
            putStrLn "Digite o novo salário:"
            novoSalario <- readLn :: IO Float
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = "", data_ingresso = "", salario = novoSalario})
        _   -> putStrLn "Opção inválida."
    main

-- Opção para remover um funcionário
removerFuncionarioOpcao :: IO ()
removerFuncionarioOpcao = do
    putStrLn "Digite o ID do funcionário que deseja remover:"
    id <- getLine
    removerFuncionarioPorId (read id)
    putStrLn "Funcionário removido com sucesso!"
    main
