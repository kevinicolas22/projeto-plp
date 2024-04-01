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
import Aula
import MainAluno
import MainAluno(limparTerminal)
import AlunoController
import Control.Concurrent (threadDelay)
import Aluno
import Planos
import Data.List.Split(splitOn)
import Data.List
import Control.Monad (when)
import AulaService
import Data.Time.LocalTime
import Data.Time.Format
import System.Console.ANSI

-- menu voltado pra testes
-- Função principal
menuFuncionario :: IO()-> IO ()
menuFuncionario menuPrincipal= do
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║                Funcionário Codefit                    ║"
    putStrLn "║                                                       ║"
    putStrLn "║   [1] Cadastrar Aluno                                 ║"
    putStrLn "║   [2] Solicitações de Treino                          ║"
    putStrLn "║   [3] Lista de Alunos                                 ║"
    putStrLn "║   [4] Menu de Aulas                                   ║"
    putStrLn "║   [5] Liberar Acesso Aluno                            ║"
    putStrLn "║   [6] Menu de Avaliação Física                        ║"
    putStrLn "║   [7] Sair                                            ║"
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite a opção:                                   ║" 
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> do 
            criarAluno
            menuFuncionario menuPrincipal
        "2" -> funcionarioCriaTreino menuPrincipal
        "3" -> listarAlunos menuPrincipal
        "4" -> menuAulas menuPrincipal
        "5" -> liberarAcessoAluno menuPrincipal
        "6" -> menuAvaliacaoFisica menuPrincipal
        "7" -> do 
            putStrLn "Saindo..." 
            menuPrincipal
        _  -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionario menuPrincipal


listarAlunos:: IO()->IO()
listarAlunos menuPrincipal= do
    limparTerminal
    putStrLn "                     ALUNOS"
    conexao <- openFile "haskell/Aluno.txt" ReadMode
    conteudo<- hGetContents conexao
    seq conteudo $ return ()
    let linhas = lines conteudo
        matriculas = primeirosElementos linhas
    exibeAlunos matriculas
    putStrLn "\n [0] Voltar     [1] Excluir Aluno"
    opçao<- getLine
    case opçao of
        "0"-> menuFuncionario menuPrincipal
        "1"-> do
            putStrLn "> Matrícula para exclusão: "
            matriculaEx<- getLine
            deletarAluno matriculaEx
            threadDelay (2 * 1000000)
            listarAlunos menuPrincipal

        _ -> listarAlunos menuPrincipal


-- Função para o menu de avaliação física
menuAvaliacaoFisica :: IO()->IO ()
menuAvaliacaoFisica menuPrincipal= do
    limparTerminal
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
                "0"->  menuPrincipal
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
                "0"-> menuFuncionario menuPrincipal

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

--Aulas
--Função para criar uma aula

menuAulas ::IO() -> IO()
menuAulas menuPrincipal = do
    limparTerminal
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║                   AULAS                    ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Criar aula                           ║"
    putStrLn "║   [2] Ler todas as aula                    ║"
    putStrLn "║   [3] Deletar Aula                         ║"
    putStrLn "║   [4] Voltar para o menu principal         ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoTreinoF <- getLine
    let opcao = map toUpper opcaoTreinoF
    case opcao of
        "1" -> criarAula menuPrincipal
        "2" -> lerTodasAulas menuPrincipal
        "3" -> deletarAulas menuPrincipal
        "4" -> menuFuncionario menuPrincipal
        _   -> do 
            putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAulas menuPrincipal
            
        


lerTodasAulas :: IO() -> IO ()
lerTodasAulas menuPrincipal = do
    limparTerminal
    putStrLn "════════════════════AULAS DISPONÍVEIS════════════════════\n\n"
    viewAulas
    putStrLn "\n [0] Voltar"
    opçao <- getLine
    case opçao of
        "0"-> menuAulas menuPrincipal
        _ -> lerTodasAulas menuPrincipal
    

deletarAulas :: IO() -> IO()
deletarAulas menuPrincipal = do
    putStrLn "Digite o nome da aula que deseja deletar: "
    nome <- getLine
    let nomeUpper = map toUpper nome

    deletarAulaPeloNome nomeUpper
    putStrLn (nomeUpper++" deletada da lista de aulas !")
    threadDelay (2 * 1000000)
    menuAulas menuPrincipal

criarAula ::IO() -> IO()
criarAula menuPrincipal= do
    limparTerminal
    putStrLn "═════════════════CADASTRO DE AULA═══════════════════\n"
    putStrLn "> Nome da aula: "
    nomeAula <- getLine
    let nomeAulaCaps = map toUpper nomeAula
    putStrLn "> Horario da aula: "
    horario <- getLine
    planos <- escolhaDePlanos
    adcionaAula nomeAulaCaps horario planos
    menuAulas menuPrincipal

escolhaDePlanos :: IO [PlanoTipo]
escolhaDePlanos = do
    putStrLn "Escolha até 3 planos: "
    putStrLn "1 - Plano Light"
    putStrLn "2 - Plano Gold"
    putStrLn "3 - Plano Premium"
    loop []
    
--Função recursiva para let três
loop :: [PlanoTipo] -> IO [PlanoTipo]
loop lista = do
    opcao <- getOpcaoPlano lista
    case opcao of
        Just plano -> do
            
            if length lista < 2
                then loop (plano : lista)
                else return (plano : lista)
        Nothing -> do
            if null lista
                then do
                    putStrLn "Nenhum plano escolhido. Por favor, escolha pelo menos um plano."
                    loop lista
                else return lista

-- Function to associate a string with a plan
getOpcaoPlano :: [PlanoTipo] -> IO (Maybe PlanoTipo)
getOpcaoPlano lista = do
    putStrLn "Digite o número do plano ou 0 para parar (default: Light):"
    opcao <- getLine
    case opcao of
        "1" -> validarepeticao lista Light
        "2" -> validarepeticao lista Gold
        "3" -> validarepeticao lista Premium
        "0" -> return Nothing
        ""  -> validarepeticao lista Light
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha entre 1, 2, 3 ou 0."
            getOpcaoPlano lista

-- Função auxiliar para validar repetição de planos
validarepeticao :: [PlanoTipo] -> PlanoTipo -> IO (Maybe PlanoTipo)
validarepeticao lista plano =
    if plano `elem` lista
        then do
            putStrLn "Plano já escolhido. Por favor, escolha outro."
            getOpcaoPlano lista  -- Corrigido para passar a lista de planos escolhidos
        else return (Just plano)

--Função auxiliar para ler múltiplas linhas, até encontrar o caracter de parada
lerLinhas :: Char -> IO String
lerLinhas stopChar = do
    linha <- getLine
    if stopChar `elem` linha
        then return ""
        else do
            restante <- lerLinhas stopChar
            return (linha ++"/"++ restante)

--Função para liberar acesso aluno
liberarAcessoAluno :: IO() -> IO()
liberarAcessoAluno menuAulas= do
    putStrLn "Para liberar o acesso do ALUNO, informe a matrícula: "
    matriculaAluno <- getLine

    currentTime <- getZonedTime
    let horaAtual = localTimeOfDay $ zonedTimeToLocalTime currentTime
    putStrLn $ "Hora atual: " ++ formatHour horaAtual ++":00"

    resultado <- acessoLiberado matriculaAluno (parseHourToInt(formatHour horaAtual))

    printAcesso resultado
    putStrLn"\nPressione ENTER para voltar..."
    opçao<- getLine
    menuFuncionario menuAulas
--Função para formatar a hora
formatHour :: TimeOfDay -> String
formatHour time = formatTime defaultTimeLocale "%H" time

parseHourToInt :: String -> Int
parseHourToInt formattedHour = read (takeWhile (/= ':') formattedHour) :: Int

printAcesso :: Bool -> IO ()
printAcesso False = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Fora do horário permitido."
    setSGR [Reset]

printAcesso True = do
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "Acesso Autorizado"
    setSGR [Reset]