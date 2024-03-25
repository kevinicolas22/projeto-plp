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
        "8" -> putStrLn "Saindo..."
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

-- Função para o menu de avaliação física
menuAvaliacaoFisica :: IO ()
menuAvaliacaoFisica = do
    putStrLn "Menu de Avaliação Física"
    putStrLn "1. Realizar Avaliação Física"
    putStrLn "2. Buscar Avaliação por ID"
    putStrLn "3. Listar Todas as Avaliações Físicas"
    putStrLn "4. Atualizar Avaliação Física"
    putStrLn "5. Verificar IMC"
    putStrLn "6. Remover Avaliação Física"
    putStrLn "7. Voltar ao Menu Principal"
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Realizando avaliação física..."
            nova_avaliacao <- criarAvaliacaoFisica
            adicionarAvaliacaoFisica nova_avaliacao
            putStrLn "Avaliação física concluída."
            menuAvaliacaoFisica 
        "2" -> do 
            putStrLn "Digite o ID da avaliação física que deseja visualizar:"
            id <- readLn :: IO Int
            lerAvaliacaoFisicaPorId id
            menuAvaliacaoFisica
        "3" -> do
            putStrLn "Listando todas as avaliações físicas..."
            listarTodasAvaliacoesFisicas
            menuAvaliacaoFisica
        "4" -> atualizarAvaliacaoFisicaOpcao
        "5" -> do
            putStrLn "Verificar IMC. Digite o ID da avaliação fisica aluno para verificar o IMC:"
            id <- readLn :: IO Int
            verificarIMC id
            menuAvaliacaoFisica
        "6" -> do
            putStrLn "Digite o ID da avaliação física que deseja remover:"
            id <- readLn :: IO Int
            removerAvaliacaoFisicaPorId id
            menuAvaliacaoFisica
        "7" -> do 
            putStrLn "Voltando ao Menu Principal..."
            main 
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAvaliacaoFisica


atualizarAvaliacaoFisicaOpcao :: IO ()
atualizarAvaliacaoFisicaOpcao = do
    putStrLn "Digite o ID da avaliação física que deseja atualizar:"
    id <- getLine
    putStrLn "Escolha o dado da avaliação física a ser atualizado:"
    putStrLn "1. Data da Avaliação"
    putStrLn "2. Peso"
    putStrLn "3. Altura"
    putStrLn "4. Idade"
    putStrLn "5. Objetivo"
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite a nova data da avaliação:"
            novaData <- getLine
            atualizarAvaliacaoFisicaPorId (read id) (AvaliacaoFisica {avaliacaoId = read id, dataAvaliacao = novaData, peso = 0.0, altura = 0.0, idade = 0, objetivo = ""})
        "2" -> do
            putStrLn "Digite o novo peso:"
            novoPeso <- readLn :: IO Float
            atualizarAvaliacaoFisicaPorId (read id) (AvaliacaoFisica {avaliacaoId = read id, dataAvaliacao = "", peso = novoPeso, altura = 0.0, idade = 0, objetivo = ""})
        "3" -> do
            putStrLn "Digite a nova altura:"
            novaAltura <- readLn :: IO Float
            atualizarAvaliacaoFisicaPorId (read id) (AvaliacaoFisica {avaliacaoId = read id, dataAvaliacao = "", peso = 0.0, altura = novaAltura, idade = 0, objetivo = ""})
        "4" -> do
            putStrLn "Digite a nova idade:"
            novaIdade <- readLn :: IO Int
            atualizarAvaliacaoFisicaPorId (read id) (AvaliacaoFisica {avaliacaoId = read id, dataAvaliacao = "", peso = 0.0, altura = 0.0, idade = novaIdade, objetivo = ""})
        "5" -> do
            putStrLn "Digite o novo objetivo:"
            novoObjetivo <- getLine
            atualizarAvaliacaoFisicaPorId (read id) (AvaliacaoFisica {avaliacaoId = read id, dataAvaliacao = "", peso = 0.0, altura = 0.0, idade = 0, objetivo = novoObjetivo})
        _   -> putStrLn "Opção inválida."
    menuAvaliacaoFisica

--TREINO
menuTreinoF :: IO()
menuTreinoF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Treinos:            ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Gerar Treinos                         ║"
    putStrLn "║   b. Visualizar Treino(s)                  ║"
    putStrLn "║   c. Visualizar Treinos da academia        ║"
    putStrLn "║   d. Deletar Treino                        ║"
    putStrLn "║   e. Alterar Treino                        ║"
    putStrLn "║   f. Voltar para o menu principal          ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoTreinoF <- getLine
    case opcaoTreinoF of
        "a" -> funcionarioCriaTreino
        "b" -> lerTreinoAluno
        "c" -> lerTodosTreinosAcademia
        "d" -> excluirTreino
        "e" -> atualizarTreinoOpcao
        "f" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuTreinoF


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

    putStrLn "Digite a data do treino (DDMMAAAA): "
    dataTreino <- getLine
    dataValidada <- dataCorreta dataTreino

    let opcao = map toUpper tipo_treino
    case opcao of
        "PS" -> do
            putStrLn "Insira o treino personalizado( ! :parar finalizar): "
            personalizado <- lerLinhas '!'
            cadastraTreino matricula "PS" personalizado dataValidada
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
                    cadastraTreino matricula "PR" "Treino Cardiovascular" dataValidada
                2 -> do
                    cadastraTreino matricula "PR" "Treino de Definição" dataValidada
                3 -> do
                    cadastraTreino matricula "PR" "Treino de Forca" dataValidada
                4 -> do
                    cadastraTreino matricula "PR" "Treino Funcional" dataValidada
                5 -> do
                    cadastraTreino matricula "PR" "Treino HIIT" dataValidada
                6 -> do
                    cadastraTreino matricula "PR" "Treino de Hipertrofia" dataValidada
                7 -> do
                    cadastraTreino matricula "PR" "Treino de Resistência Muscular" dataValidada
                8 -> do
                    cadastraTreino matricula "PR" "Treino Terapêutico" dataValidada
    menuTreinoF     

--Função para ler treino pela matricula
lerTreinoAluno :: IO()
lerTreinoAluno = do
    putStrLn "Digite a matricula do aluno para visualizar seu treino(s): "
    matricula <- readLn :: IO Int
    viewTreinoAluno matricula
    menuTreinoF

--Função para ler todos os treinos da academia
lerTodosTreinosAcademia :: IO()
lerTodosTreinosAcademia = do
    viewAllTreino
    menuTreinoF

--Função para escolher um treino, caso tenha mais de um e em seguida o excluir
excluirTreino :: IO()
excluirTreino = do
    putStrLn "Para excluir um treino digite a matricula do aluno: "
    matricula <- readLn :: IO Int
    
    qtdeTreinos <- quantidadeTreinoAluno matricula
    putStrLn ("Aluno possui " ++ (show qtdeTreinos) ++ " treino(s)")

    if not (qtdeTreinos == 0)
        then do
            if qtdeTreinos == 1
                then do
                    putStrLn "Matricula cadastrada em treino"
                    deleteTreinoMatriculaComUmTreino matricula
                    menuTreinoF
                else do
                    putStrLn "Segue as posições abaixo, escolha apenas UMA Opcão para excluir"
                    putStrLn "(OBS: A opção está na ordem em que a lista geral é apresentada)"
                    posicoes <- viewPosicoesTreinosMatricula matricula
                    exibirPosicoesMatricula posicoes
        
                    escolha <- readLn :: IO Int
                    posicaoValidada <- verificaPosicaoDeleteCorreta escolha posicoes
        
                    deleteTreinoMatriculaComVariosTreinos matricula escolha
                    menuTreinoF
        else do 
            putStrLn "Não existe nenhum treino cadastrado com essa matricula"
            menuTreinoF

--Função para atualizar treino
atualizarTreinoOpcao :: IO ()
atualizarTreinoOpcao = do
    putStrLn "Digite a matrícula do aluno que deseja atualizar:"
    matricula <- readLn :: IO Int
    putStrLn "Escolha o dado da avaliação física a ser atualizado:"
    putStrLn "1. Tipo Treino"
    putStrLn "2. Descrição"
    putStrLn "3. Data"
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite o novo tipo de treino:"
            novotipoTreino <- getLine
            atualizarTreinoPelaMatricula (matricula) (Treino {matricula = matricula, tipoTreino = novotipoTreino, decricao = "", dataTreino = ""})
        "2" -> do
            putStrLn "Digite a nova descrição:"
            novoDescricao <- getLine
            atualizarTreinoPelaMatricula (matricula) (Treino {matricula = matricula, tipoTreino = "", decricao = novoDescricao, dataTreino = ""})
        "3" -> do
            putStrLn "Digite a nova data do treino:"
            novaData <- getLine
            atualizarTreinoPelaMatricula (matricula) (Treino {matricula = matricula, tipoTreino = "", decricao = "", dataTreino = novaData})
        _   -> putStrLn "Opção inválida."
    menuAvaliacaoFisica

--FUNÇÕES AUXILIARES DE VALIDAÇÃO/IMPRESSÃO

--Falta verificar se aluno existe 
--Função recursiva para validar se a posicao passada pelo usuario está correta
verificaPosicaoDeleteCorreta :: Int -> [Int] -> IO Int
verificaPosicaoDeleteCorreta n lista = do
    if n `elem` lista 
        then return n
        else do 
            putStrLn "Posição inserida inválida, tente novamente" 
            novaPosicao <- readLn :: IO Int
            verificaPosicaoDeleteCorreta novaPosicao lista

--Função para exibir as posições dos treinos de uma única matricula
exibirPosicoesMatricula :: [Int] -> IO ()
exibirPosicoesMatricula [] = return ()
exibirPosicoesMatricula (x:xs) = do
    --treinoTemp <- visualizarDadosTreino x
    putStrLn ("Opção:("  ++ show x ++ ")")
    exibirPosicoesMatricula xs


--Funçãa recursiva para encontrar uma matricula valida no treino.txt
matriculaCorreta :: Int -> IO Int
matriculaCorreta matricula = do 
    matriculaValidada <- verificaMatricula matricula
    if matriculaValidada
        then return matricula
        else do
            putStrLn "Treino com determinada matricula ainda não foi cadastrado, digite outra matricula"
            novaOpcao <- getLine
            let novaMatricula = read novaOpcao :: Int
            matriculaCorreta novaMatricula

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

dataCorreta :: String -> IO String
dataCorreta dataTreino = do
    case delimitarData dataTreino of
        Just x -> return x
        Nothing -> do
            putStrLn "Data não está no formato DDMMAAAA, digite novamente: "
            novaData <- getLine
            dataCorreta novaData