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
    putStrLn "Funcionário encontrado:"
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
        "1" -> atualizarNomeFuncionario (read id)
        "2" -> atualizarCpfFuncionario (read id)
        "3" -> atualizarEnderecoFuncionario (read id)
        "4" -> atualizarTelefoneFuncionario (read id)
        "5" -> atualizarDataIngressoFuncionario (read id)
        _   -> putStrLn "Opção inválida." >> main

-- Opção para atualizar o nome de um funcionário
atualizarNomeFuncionario :: Id -> IO ()
atualizarNomeFuncionario id = do
    putStrLn "Digite o novo nome:"
    novoNome <- getLine
    atualizarFuncionario id (\f -> f { nome = novoNome })
    putStrLn "Nome atualizado com sucesso!"
    main

-- Opção para atualizar o CPF de um funcionário
atualizarCpfFuncionario :: Id -> IO ()
atualizarCpfFuncionario id = do
    putStrLn "Digite o novo CPF:"
    novoCpf <- getLine
    atualizarFuncionario id (\f -> f { cpf = novoCpf })
    putStrLn "CPF atualizado com sucesso!"
    main

-- Opção para atualizar o endereço de um funcionário
atualizarEnderecoFuncionario :: Id -> IO ()
atualizarEnderecoFuncionario id = do
    putStrLn "Digite o novo endereço:"
    novoEndereco <- getLine
    atualizarFuncionario id (\f -> f { endereco = novoEndereco })
    putStrLn "Endereço atualizado com sucesso!"
    main

-- Opção para atualizar o telefone de um funcionário
atualizarTelefoneFuncionario :: Id -> IO ()
atualizarTelefoneFuncionario id = do
    putStrLn "Digite o novo telefone:"
    novoTelefone <- getLine
    atualizarFuncionario id (\f -> f { telefone = novoTelefone })
    putStrLn "Telefone atualizado com sucesso!"
    main

-- Opção para atualizar a data de ingresso de um funcionário
atualizarDataIngressoFuncionario :: Id -> IO ()
atualizarDataIngressoFuncionario id = do
    putStrLn "Digite a nova data de ingresso:"
    novaDataIngresso <- getLine
    atualizarFuncionario id (\f -> f { data_ingresso = novaDataIngresso })
    putStrLn "Data de ingresso atualizada com sucesso!"
    main

-- Função para atualizar um funcionário pelo ID
atualizarFuncionario :: Id -> (Funcionario -> Funcionario) -> IO ()
atualizarFuncionario targetId updateFunc = do
    conteudo <- readFile "funcionario.txt"
    let linhas = lines conteudo
        funcionarios = mapMaybe (parseFuncionario . words) linhas
        funcionariosAtualizados = map (\func -> if funcId func == targetId then updateFunc func else func) funcionarios
        novoConteudo = unlines (map toStringFuncionario funcionariosAtualizados)
    writeFile "funcionario.txt" novoConteudo

-- Opção para remover um funcionário
removerFuncionarioOpcao :: IO ()
removerFuncionarioOpcao = do
    putStrLn "Digite o ID do funcionário que deseja remover:"
    id <- getLine
    removerFuncionarioPorId (read id)
    putStrLn "Funcionário removido com sucesso!"
    main
