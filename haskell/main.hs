import System.IO
import System.Directory
import System.Exit
import Funcionario
import Data.Maybe (mapMaybe)

-- Função principal
main :: IO ()
main = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Adicionar um funcionário"
    putStrLn "2. Ler informações de um funcionário"
    putStrLn "3. Atualizar um funcionário"
    putStrLn "4. Remover um funcionário"
    putStrLn "5. Sair"
    opcao <- getLine
    case opcao of
        "1" -> adicionarFuncionarioOpcao
        "2" -> lerFuncionarioOpcao
        "3" -> atualizarFuncionarioOpcao
        "4" -> removerFuncionarioOpcao
        "5" -> putStrLn "Saindo..."
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

-- Opção para adicionar um funcionário
adicionarFuncionarioOpcao :: IO ()
adicionarFuncionarioOpcao = do
    novoFuncionario <- criarFuncionario
    adicionarFuncionario novoFuncionario
    putStrLn "Funcionário adicionado com sucesso!"
    main

-- Opção para ler informações de um funcionário
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
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite o novo nome:"
            novoNome <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = novoNome, cpf = "", endereco = "", telefone = "", data_ingresso = ""})
        "2" -> do
            putStrLn "Digite o novo CPF:"
            novoCPF <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = novoCPF, endereco = "", telefone = "", data_ingresso = ""})
        "3" -> do
            putStrLn "Digite o novo endereço:"
            novoEndereco <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = novoEndereco, telefone = "", data_ingresso = ""})
        "4" -> do
            putStrLn "Digite o novo telefone:"
            novoTelefone <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = novoTelefone, data_ingresso = ""})
        "5" -> do
            putStrLn "Digite a nova data de ingresso:"
            novaData <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = "", data_ingresso = novaData})
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
