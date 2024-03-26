module MainFuncionario where

import System.IO
import Funcionario
import System.Directory
import System.Exit
import FuncionarioService
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)
import AvaliacaoFisica
import Treino
import MainAluno(limparTerminal)
import AlunoController
-- menu voltado pra testes
-- Função principal
main :: IO ()
main = do
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║                Funcionário Codefit                    ║"
    putStrLn "║                                                       ║"
    putStrLn "║   1. Cadastrar Aluno                                  ║"
    putStrLn "║   2. Adicionar um funcionário                         ║"
    putStrLn "║   3. Ler informações de um funcionário por Id         ║"
    putStrLn "║   4. Atualizar um funcionário                         ║"
    putStrLn "║   5. Remover um funcionário                           ║"
    putStrLn "║   6. Listar todos os funcionarios                     ║"
    putStrLn "║   7. Menu de Avaliação Física                         ║"
    putStrLn "║   8. Menu de Treinos                                  ║"
    putStrLn "║   9. Sair                                             ║"
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite a opção:                                   ║" 
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> do 
            criarAluno
            main
        "2" -> adicionarFuncionarioOpcao
        "3" -> lerFuncionarioOpcao
        "4" -> atualizarFuncionarioOpcao
        "5" -> removerFuncionarioOpcao
        "6" -> lerTodosFuncionarios
        "7" -> menuAvaliacaoFisica
        "8" -> menuTreinoF
        "9" -> putStrLn "Saindo..." 
        _  -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

--Esse menu menuFuncionarioF vai ser usado pelo gestor
menuFuncionarioF :: IO()
menuFuncionarioF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Funcionario:        ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Criar                                 ║"
    putStrLn "║   b. Listar por id                         ║"
    putStrLn "║   c. Listar todos funcionarios             ║"
    putStrLn "║   d. Alterar                               ║"
    putStrLn "║   e. Excluir                               ║"
    putStrLn "║   f. Voltar para o menu principal          ║"
    putStrLn "╚════════════════════════════════════════════╝"
    putStrLn "Digite a opção: "
    opcaoFuncionarioF <- getLine
    let opcao = map toUpper opcaoFuncionarioF
    case opcao of
        "A" -> adicionarFuncionarioOpcao
        "B" -> lerFuncionarioOpcao
        "C" -> lerTodosFuncionarios
        "D" -> atualizarFuncionarioOpcao
        "E" -> removerFuncionarioOpcao
        "F" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionarioF

-- Opção para adicionar um funcionário
adicionarFuncionarioOpcao :: IO ()
adicionarFuncionarioOpcao = do
    limparTerminal
    novoFuncionario <- criarFuncionario
    adicionarFuncionario novoFuncionario
    putStrLn "Funcionário adicionado com sucesso!"
    main

-- Opção para ler todos funcionarios
lerTodosFuncionarios :: IO()
lerTodosFuncionarios = do
    limparTerminal
    listarTodosFuncionarios >> main

-- Opção para ler informações de um funcionário por id
lerFuncionarioOpcao :: IO ()
lerFuncionarioOpcao = do
    limparTerminal
    putStrLn "Digite o ID do funcionário que deseja buscar:"
    id <- getLine
    lerFuncionarioPorId (read id)
    main  

-- Opção para atualizar um funcionário
atualizarFuncionarioOpcao :: IO ()
atualizarFuncionarioOpcao = do
    limparTerminal
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
    limparTerminal
    putStrLn "Digite o ID do funcionário que deseja remover:"
    id <- getLine
    removerFuncionarioPorId (read id)
    putStrLn "Funcionário removido com sucesso!"
    main

-- Função para o menu de avaliação física
menuAvaliacaoFisica :: IO ()
menuAvaliacaoFisica = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║         Menu de Avaliação Física           ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Realizar Avaliação Física             ║"            
    putStrLn "║   b. Buscar Avaliação por ID               ║"
    putStrLn "║   c. Listar Todas as Avaliações Físicas    ║"
    putStrLn "║   d. Atualizar Avaliação Física            ║"
    putStrLn "║   e. Verificar IMC                         ║"
    putStrLn "║   f. Remover Avaliação Física              ║"
    putStrLn "║   g. Voltar ao Menu Principal              ║"
    putStrLn "╚════════════════════════════════════════════╝"
    putStrLn "Digite a opção: "
    opcaoAvaliacaoFisica <- getLine
    let opcao = map toUpper opcaoAvaliacaoFisica
    case opcao of
        "A" -> do
            putStrLn "Realizando avaliação física..."
            nova_avaliacao <- criarAvaliacaoFisica
            adicionarAvaliacaoFisica nova_avaliacao
            putStrLn "Avaliação física concluída."
            menuAvaliacaoFisica 
        "B" -> do 
            putStrLn "Digite o ID da avaliação física que deseja visualizar:"
            id <- readLn :: IO Int
            lerAvaliacaoFisicaPorId id
            menuAvaliacaoFisica
        "C" -> do
            putStrLn "Listando todas as avaliações físicas..."
            listarTodasAvaliacoesFisicas
            menuAvaliacaoFisica
        "D" -> atualizarAvaliacaoFisicaOpcao
        "E" -> do
            putStrLn "Verificar IMC. Digite o ID da avaliação fisica aluno para verificar o IMC:"
            id <- readLn :: IO Int
            verificarIMC id
            menuAvaliacaoFisica
        "F" -> do
            putStrLn "Digite o ID da avaliação física que deseja remover:"
            id <- readLn :: IO Int
            removerAvaliacaoFisicaPorId id
            menuAvaliacaoFisica
        "G" -> do 
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
    putStrLn "Digite a opção: "
    opcaoTreinoF <- getLine
    let opcao = map toUpper opcaoTreinoF
    case opcao of
        "A" -> funcionarioCriaTreino
      --  "B" -> lerTreinoAluno
      --  "C" -> lerTodosTreinosAcademia
      --  "D" -> excluirTreino
      --  "E" -> atualizarTreinoOpcao
       -- "F" -> main
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuTreinoF


--Função para o funcionario criar treino de um aluno
funcionarioCriaTreino :: IO()
funcionarioCriaTreino = do
    --falta verificar se existe aluno tratar caso não existie
    --falta puxar aluno
    --provisorio
    putStrLn "Digite a matricula do aluno: "
    matricula <- getLine
    -- verificar aluno.txt
    putStrLn "Digite o tipo de treino: "
    tipo_treino <- getLine

    putStrLn "Insira o treino personalizado( ! :parar finalizar): "
    personalizado <- lerLinhas '!'
    let personalizadoArray= toArray personalizado
    treino<- cadastraTreino tipo_treino personalizadoArray
    associarTreinoAluno matricula treino 
    
    menuTreinoF     

{---Função para ler treino pela matricula
lerTreinoAluno :: IO()
lerTreinoAluno = do
    putStrLn "Digite a matricula do aluno para visualizar seu treino(s): "
    matricula <- getLine
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
                    deleteTreinoMatriculaComUmTreino matricula
                    menuTreinoF
                else do
                    putStrLn "Segue as opções abaixo, escolha apenas UMA opcão para excluir"
                    putStrLn "(OBS: A opção está na ordem em que a lista geral é apresentada)"
                    posicoes <- viewPosicoesTreinosMatricula matricula
                    exibirPosicoesMatricula posicoes
        
                    escolha <- readLn :: IO Int
                    posicaoValidada <- verificaPosicaoDeleteCorreta escolha posicoes
        
                    deleteTreinoMatriculaComVariosTreinos matricula posicaoValidada
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
            putStrLn "Digite o novo tipo de treino: (PR ou PS)"
            tipo_treino <- getLine
            tipoTreinoValidado <- tipoTreinoCorreto tipo_treino
            atualizarTreinoPelaMatricula (matricula) (Treino  matricula (map toUpper tipoTreinoValidado)  ""  "")
        "2" -> do
            putStrLn "Digite a nova descrição:"
            novoDescricao <- getLine
            atualizarTreinoPelaMatricula (matricula) (Treino  matricula ""  novoDescricao  "")
        "3" -> do
            putStrLn "Digite a nova data do treino:"
            novaData <- getLine
            dataValidada <- dataCorreta novaData
            atualizarTreinoPelaMatricula (matricula) (Treino  matricula ""  ""  novaData)
        _   -> putStrLn "Opção inválida."
    menuTreinoF

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
            matriculaCorreta novaMatricula-}

--Função auxiliar para ler múltiplas linhas, até encontrar o caracter de parada
lerLinhas :: Char -> IO String
lerLinhas stopChar = do
    linha <- getLine
    if stopChar `elem` linha
        then return ""
        else do
            restante <- lerLinhas stopChar
            return (linha ++"/"++ restante)
{-
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

tipoTreinoCorreto :: String -> IO String
tipoTreinoCorreto tipoTreino = do 
    let tipoTreinoUpper = map toUpper tipoTreino
    if  tipoTreinoUpper == "PR" || tipoTreinoUpper == "PS"
        then return tipoTreinoUpper
        else do
            putStrLn "Digite novamente o tipo de treino: (PR ou PS)"
            novoTipoTreino <- getLine
            tipoTreinoCorreto (map toUpper novoTipoTreino)-}