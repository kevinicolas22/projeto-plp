module Main where

import System.IO
import Funcionario
import System.Directory
import System.Exit
import FuncionarioService
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)
import AvaliacaoFisica

-- Função principal
main :: IO ()
main = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Adicionar um funcionário"
    putStrLn "2. Ler informações de um funcionário por Id"
    putStrLn "3. Atualizar um funcionário"
    putStrLn "4. Remover um funcionário"
    putStrLn "5. Listar todos os funcionarios"
    putStrLn "6. Menu de Avaliação Física"
    putStrLn "7. Menu de Treino"
    putStrLn "8. Sair"
    opcao <- getLine
    case opcao of
        "1" -> adicionarFuncionarioOpcao
        "2" -> lerFuncionarioOpcao
        "3" -> atualizarFuncionarioOpcao
        "4" -> removerFuncionarioOpcao
        "5" -> lerTodosFuncionarios
        "6" -> menuAvaliacaoFisica
        "7" -> menuTreinoF
        "7" -> putStrLn "Saindo..."
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
    putStrLn "2. cpfG"
    putStrLn "3. Endereço"
    putStrLn "4. Telefone"
    putStrLn "5. Data de Ingresso"
    putStrLn "6. Salário"
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite o novo nome:"
            novoNome <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = novoNome, cpfG = "", endereco = "", telefone = "", data_ingresso = "", salario = 0.0})
        "2" -> do
            putStrLn "Digite o novo cpfG:"
            novocpfG <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpfG = novocpfG, endereco = "", telefone = "", data_ingresso = "", salario = 0.0})
        "3" -> do
            putStrLn "Digite o novo endereço:"
            novoEndereco <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpfG = "", endereco = novoEndereco, telefone = "", data_ingresso = "", salario = 0.0})
        "4" -> do
            putStrLn "Digite o novo telefone:"
            novoTelefone <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpfG = "", endereco = "", telefone = novoTelefone, data_ingresso = "", salario = 0.0})
        "5" -> do
            putStrLn "Digite a nova data de ingresso:"
            novaData <- getLine
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpfG = "", endereco = "", telefone = "", data_ingresso = novaData, salario = 0.0})
        "6" -> do
            putStrLn "Digite o novo salário:"
            novoSalario <- readLn :: IO Float
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpfG = "", endereco = "", telefone = "", data_ingresso = "", salario = novoSalario})
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

-- Função para o menu de avaliação física
menuAvaliacaoFisica :: IO ()
menuAvaliacaoFisica = do
    putStrLn "Menu de Avaliação Física"
    putStrLn "1. Realizar Avaliação Física"
    putStrLn "2. Visualizar Avaliações Físicas Anteriores"
    putStrLn "3. Voltar ao Menu Principal"
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Realizando avaliação física..."
            adicionarAvaliacaoFisica
            putStrLn "Avaliação física concluída."
            main  
        "2" -> visualizarAvaliacoesAnteriores
        "3" -> putStrLn "Voltando ao Menu Principal..."
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAvaliacaoFisica
      


-- Função para visualizar avaliações físicas anteriores
visualizarAvaliacoesAnteriores :: IO ()
visualizarAvaliacoesAnteriores = putStrLn "Visualizando avaliações físicas anteriores..."

--TREINO
menuTreinoF :: IO()
menuTreinoF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Treinos:            ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Gerar Treinos                         ║"
    putStrLn "║   b. Visualizar Treino(s)                  ║"
    putStrLn "║   c. Progresso do aluno                    ║"
    putStrLn "║   d. Avaliação fisíca do aluno             ║"
    putStrLn "║   e. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoTreinoF <- getLine
    case opcaoTreinoF of
        "a" -> funcionarioCriaTreino
        --"b" -> lerTreinoAluno
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuTreinoF



--Função auxiliar para ler múltiplas linhas, até encontrar o caracter de parada
lerLinhas :: Char -> IO String
lerLinhas stopChar = do
    linha <- getLine
    if stopChar `elem` linha
        then return ""
        else do
            restante <- lerLinhas stopChar
            return (linha ++"/"++ restante)

--Função auxiliar para validar as entradas de cada opção de treino
validarOpcaoTreino :: Int -> Bool
validarOpcaoTreino n = n >= 1 && n <= 8

--Função auxiliar para ficar lendo recursivamente, até inserir a opção correta
opcaoCorreta :: Int -> IO Int
opcaoCorreta valor =
    if validarOpcaoTreino valor
        then return valor
        else do
            putStrLn "Opção inválida. Por favor, insira um número entre 1 e 8."
            novaOpcao <- getLine
            let novoValor = read novaOpcao :: Int
            opcaoCorreta novoValor

--Função para o funcionario criar treino de um aluno
funcionarioCriaTreino :: IO()
funcionarioCriaTreino = do
    --falta verificar se existe aluno tratar caso não existie
    --falta puxar aluno
    --provisorio
    putStrLn "Digite a matricula do aluno: "
    matricula <- readLn :: IO Int

    putStrLn "Digite o tipo de treino (PS - Personalizado ou PR - Padronizado): "
    tipo_treino <- getLine

    let opcao = map toUpper tipo_treino
    case opcao of
        "PS" -> do
            putStrLn "Insira o treino personalizado( ! :parar finalizar): "
            personalizado <- lerLinhas '!'
            cadastraTreino matricula "PS" personalizado
        "PR" -> do
            putStrLn "Escolha o treino padrão: \n\
                        \1 - Treino Cardiovascular\n\
                        \2 - Treino de Definição\n\
                        \3 - Treino de Forca\n\
                        \4 - Treino Funcional\n\
                        \5 - Treino HIIT\n\
                        \6 - Treino de Hipertrofia\n\
                        \7 - Treino de Resistência Muscular\n\
                        \8 - Treino Terapêutico\n"

            escolha <- readLn :: IO Int
            opcaoValidada <- opcaoCorreta escolha

            case opcaoValidada of
                1 -> do
                    cadastraTreino matricula "PR" "Treino Cardiovascular"
                2 -> do
                    cadastraTreino matricula "PR" "Treino de Definição"
                3 -> do
                    cadastraTreino matricula "PR" "Treino de Forca"
                4 -> do
                    cadastraTreino matricula "PR" "Treino Funcional"
                5 -> do
                    cadastraTreino matricula "PR" "Treino HIIT"
                6 -> do
                    cadastraTreino matricula "PR" "Treino de Hipertrofia"
                7 -> do
                    cadastraTreino matricula "PR" "Treino de Resistência Muscular"
                8 -> do
                    cadastraTreino matricula "PR" "Treino Terapêutico"
    main      