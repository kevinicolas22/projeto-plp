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
import MainAluno(limparTerminal,recuperaAlunoMatricula)
import AlunoController
import Control.Concurrent (threadDelay)
import Aluno
import Data.List.Split(splitOn)
import Data.List

-- menu voltado pra testes
-- Função principal
menuFuncionario :: IO()-> IO ()
menuFuncionario menuPrincipal= do
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║                Funcionário Codefit                    ║"
    putStrLn "║                                                       ║"
    putStrLn "║   [1] Cadastrar Aluno                                 ║"
    putStrLn "║   [2] Meus Dados                                      ║"
    putStrLn "║   [3] Listar todos os funcionarios                    ║"
    putStrLn "║   [4] Menu de Avaliação Física                        ║"
    putStrLn "║   [5] Menu de Treinos                                 ║"
    putStrLn "║   [6] Sair                                            ║"
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite a opção:                                   ║" 
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> do 
            criarAluno
            menuFuncionario menuPrincipal
        "2" -> lerFuncionarioOpcao menuPrincipal
        "3" -> lerTodosFuncionarios menuPrincipal
        "4" -> menuAvaliacaoFisica menuPrincipal
        "5" -> menuTreinoF menuPrincipal
        "6" -> do 
            putStrLn "Saindo..." 
            menuPrincipal
        _  -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionario menuPrincipal



-- Opção para adicionar um funcionário
adicionarFuncionarioOpcao ::IO()-> IO ()
adicionarFuncionarioOpcao menuPrincipal= do
    limparTerminal
    novoFuncionario <- criarFuncionario
    adicionarFuncionario novoFuncionario
    putStrLn "Funcionário adicionado com sucesso!"
    menuFuncionario menuPrincipal

-- Opção para ler todos funcionarios
lerTodosFuncionarios :: IO()->IO()
lerTodosFuncionarios menuPrincipal= do
    limparTerminal
    listarTodosFuncionarios >> menuFuncionario menuPrincipal

-- Opção para ler informações de um funcionário por id
lerFuncionarioOpcao :: IO() ->IO ()
lerFuncionarioOpcao menuPrincipal= do
    limparTerminal
    putStrLn "Digite o ID do funcionário que deseja buscar:"
    id <- getLine
    lerFuncionarioPorId (read id)
    menuFuncionario menuPrincipal

-- Opção para atualizar um funcionário
atualizarFuncionarioOpcao :: IO()-> IO ()
atualizarFuncionarioOpcao menuPrincipal= do
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
    menuFuncionario menuPrincipal

-- Opção para remover um funcionário
removerFuncionarioOpcao :: IO()->IO ()
removerFuncionarioOpcao menuPrincipal= do
    limparTerminal
    putStrLn "Digite o ID do funcionário que deseja remover:"
    id <- getLine
    removerFuncionarioPorId (read id)
    putStrLn "Funcionário removido com sucesso!"
    menuFuncionario menuPrincipal

-- Função para o menu de avaliação física
menuAvaliacaoFisica :: IO()->IO ()
menuAvaliacaoFisica menuPrincipal= do
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
            menuAvaliacaoFisica menuPrincipal
        "B" -> do 
            putStrLn "Digite o ID da avaliação física que deseja visualizar:"
            id <- readLn :: IO Int
            lerAvaliacaoFisicaPorId id
            menuAvaliacaoFisica menuPrincipal
        "C" -> do
            putStrLn "Listando todas as avaliações físicas..."
            listarTodasAvaliacoesFisicas
            menuAvaliacaoFisica menuPrincipal
        "D" -> atualizarAvaliacaoFisicaOpcao menuPrincipal
        "E" -> do
            putStrLn "Verificar IMC. Digite o ID da avaliação fisica aluno para verificar o IMC:"
            id <- readLn :: IO Int
            verificarIMC id
            menuAvaliacaoFisica menuPrincipal
        "F" -> do
            putStrLn "Digite o ID da avaliação física que deseja remover:"
            id <- readLn :: IO Int
            removerAvaliacaoFisicaPorId id
            menuAvaliacaoFisica menuPrincipal
        "G" -> do 
            putStrLn "Voltando ao Menu Principal..."
            menuFuncionario menuPrincipal
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAvaliacaoFisica menuPrincipal


atualizarAvaliacaoFisicaOpcao :: IO()->IO ()
atualizarAvaliacaoFisicaOpcao menuPrincipal= do
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
    menuAvaliacaoFisica menuPrincipal

--TREINO
menuTreinoF :: IO()->IO()
menuTreinoF menuPrincipal= do
    limparTerminal
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║                 TREINOS                    ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Solicitações                         ║"
    putStrLn "║   [2] Visualizar Treino(s)                 ║"
    putStrLn "║   [3] Visualizar Treinos da academia       ║"
    putStrLn "║   [4] Deletar Treino                       ║"
    putStrLn "║   [5] Alterar Treino                       ║"
    putStrLn "║   [6] Voltar para o menu principal         ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoTreinoF <- getLine
    let opcao = map toUpper opcaoTreinoF
    case opcao of
        "1" -> funcionarioCriaTreino menuPrincipal
      --  "B" -> lerTreinoAluno
      --  "C" -> lerTodosTreinosAcademia
      --  "D" -> excluirTreino
      --  "E" -> atualizarTreinoOpcao
        "6" -> menuFuncionario menuPrincipal
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuTreinoF menuPrincipal


--Função para o funcionario criar treino de um aluno

funcionarioCriaTreino :: IO () -> IO ()
funcionarioCriaTreino menuPrincipal = do
    limparTerminal
    putStrLn "   ==== Solicitações Pendentes ====\n"
    conexao <- openFile "haskell/solicitacoes.txt" ReadMode
    conteudo <- hGetContents conexao
    let conteudoMaiusculo = map toUpper conteudo
        linhas = lines conteudoMaiusculo
        solicitacoes = map (splitOn ",")  linhas
    if not(length solicitacoes == 0)
        then do
            exibeSolicitacoes linhas
            putStrLn "\n [0] Voltar       [1] Atribuir Treino"
            opçao<- getLine
            case opçao of 
                "0"-> menuTreinoF menuPrincipal
                "1"-> do
                    putStrLn "\nMatricula do aluno: "
                    matricula <- getLine
                    let matriculaCaps = map toUpper matricula
                    -- verificar aluno.txt
                    putStrLn "Tipo de Treino requisitado: "
                    tipo_treino <- getLine
                    let tipoTreinoCaps = map toUpper tipo_treino
                    solicitaçaoEncontrada<- existeSolicitacao tipoTreinoCaps matriculaCaps solicitacoes
                    if solicitaçaoEncontrada
                        then do
                            putStrLn "Insira o treino personalizado( ! :parar finalizar): "
                            personalizado <- lerLinhas '!'
                            let personalizadoArray = toArray personalizado
                            treino <- cadastraTreino tipoTreinoCaps personalizadoArray
                            associarTreinoAluno matriculaCaps treino
                            removerSolicitacao tipoTreinoCaps matricula
                            putStrLn " TREINO ATRIBUÍDO COM SUCESSO !"
                            threadDelay (2 * 1000000)
                            funcionarioCriaTreino menuPrincipal
                        else do
                            putStrLn " Solicitação não encontrada! Preencha os campos corretamente"
                            threadDelay (2 * 1000000)
                            funcionarioCriaTreino menuPrincipal
                _ -> funcionarioCriaTreino menuPrincipal
        else do
            putStrLn " Não há solicitações pendentes. \n\n [0] Voltar"
            opçao1<- getLine
            case opçao1 of
                "0"-> menuTreinoF menuPrincipal

removerSolicitacao :: String -> String -> IO ()
removerSolicitacao tipoTreino matricula = do
    handle <- openFile "haskell/solicitacoes.txt" ReadMode
    contents <- hGetContents handle
    (tempName, tempHandle) <- openTempFile "." "temp"
    let conteudoMaiusculo = map toUpper contents
        linhas = lines conteudoMaiusculo
        solicitacoes = map (splitOn ",") linhas
        novasSolicitacoes = filter (\[mat, tipo] -> mat /= matricula || tipo /= tipoTreino) solicitacoes
        novoConteudo = unlines $ map (intercalate "," ) novasSolicitacoes
    hPutStr tempHandle (novoConteudo)
    hClose handle
    hClose tempHandle
    removeFile "haskell/solicitacoes.txt"
    renameFile tempName "haskell/solicitacoes.txt"
    


existeSolicitacao :: String -> String -> [[String]] -> IO Bool
existeSolicitacao tipoTreino matricula linhas = do
    let existe = any (\[mat,tipo] -> mat == matricula && tipo == tipoTreino) linhas
    return existe




exibeSolicitacoes :: [String] -> IO ()
exibeSolicitacoes [] = putStrLn " "
exibeSolicitacoes detalhesSolicitacao = mapM_ exibeSolicitacao detalhesSolicitacao

exibeSolicitacao :: String -> IO ()
exibeSolicitacao string= do
    let partes = splitOn "," string
    alunoEncontrado<- recuperaAlunoMatricula (partes !! 0)
    putStrLn ("=> Aluno:"++ nomeAluno alunoEncontrado ++ 
            "\n   Matricula:"++ matricula alunoEncontrado++
            "\n   Tipo de treino requisitado:" ++ partes !! 1 ++"\n\n")
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