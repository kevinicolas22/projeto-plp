{-# LANGUAGE PackageImports #-}
module ManagerService where

import "directory" System.Directory
import Manager
import System.IO
import Data.List
import Data.List.Split
import Funcionario
import FuncionarioService
import Data.Function
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)
import Data.List (find)
import Text.Printf
import Data.List (null) 
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, maybeToList)
import Control.Exception
import System.Console.ANSI
import Control.Concurrent
import Control.Exception (catch, IOException)
import Control.Concurrent (threadDelay)



--- Funçao para criar Gestor e as delimitaçoes de cada dado do gestor 
criarGestor :: IO Manager
criarGestor = do
    putStrLn ">> Digite o seu ID de gestor: "
    id <- getLine

    -- Tente abrir o arquivo, criando-o se não existir
    handle <- openFile "manager.txt" ReadWriteMode `catch` \e -> do
        let _ = e :: IOError
        openFile "manager.txt" WriteMode

    -- Leia o conteúdo do arquivo e verifique se o ID já existe
    assunto <- hGetContents handle
    let linhas = lines assunto
        ids = primeirosElementosG linhas

    if id `elem` ids
        then do
            putStrLn "ID já em uso. Escolha um ID diferente."
            threadDelay (1 * 1000000)
            hClose handle  -- Feche o arquivo antes de chamar a função recursiva
            criarGestor
        else do
            hClose handle  -- Feche o arquivo antes de prosseguir
            putStrLn "\n>> Digite o seu CPF (formatação 000.000.000-00): "
            cpfG <- getLine
            case delimitarCpfG cpfG of
                Just cpfGDelimitado -> do
                    putStrLn "\n>> Digite o seu nome: "
                    nomeG <- getLine
                    putStrLn "\n>> Digite sua data de nascimento (formato dd/mm/aaaa): "
                    nascimento <- getLine
                    case delimitarNascimento nascimento of
                        Just nascimentoDelimitado -> do
                            putStrLn "\n>> Digite seu telefone (formato DDD000000000): "
                            telefoneG <- getLine
                            case delimitarTelefoneG telefoneG of
                                Just telefoneGDelimitado -> do
                                    putStrLn "\n>> Digite seu endereço: "
                                    enderecoG <- getLine
                                    return(Manager (read id) cpfGDelimitado nomeG nascimentoDelimitado telefoneGDelimitado enderecoG)
                                Nothing -> do
                                    putStrLn "Telefone inválido. Por favor, inicie novamente seu cadastro."
                                    threadDelay (1 * 1000000)
                                    limpar
                                    criarGestor
                        Nothing -> do
                            putStrLn "Data de nascimento inválida. Por favor, inicie novamente seu cadastro."
                            threadDelay (1 * 1000000)
                            limpar
                            criarGestor
                Nothing -> do
                    putStrLn "CPF inválido. Por favor, inicie novamente seu cadastro."
                    threadDelay (1 * 1000000)
                    limpar
                    criarGestor 

                     



--- Funçao para adicionar gestor ao arquivo txt(banco de dados)
adicionarGestor :: Manager -> IO ()
adicionarGestor novo_gestor = do
  conexao <- openFile "manager.txt" ReadMode
  assunto <- hGetContents conexao
  let linhas = lines assunto
      ids = primeirosElementosG linhas
      idNovo = managerId novo_gestor
  if verificandoIdG (show idNovo) ids
    then putStrLn "ID já em uso. Escolha um ID diferente."
    else appendFile "manager.txt" (toStringManager novo_gestor ++ "\n")
  hClose conexao


--- Funçao para  ler o Gestor por Id
lerGestorPorId :: Int -> IO ()
lerGestorPorId targetId = do
    conexao <- openFile "manager.txt" ReadMode
    assunto <- hGetContents conexao
    let linhas = lines assunto
        ids = primeirosElementosG linhas
    if not (verificandoIdG (show targetId) ids)
        then putStrLn "ID não encontrado." 
        else do 
            let dadosGestor = filtrarIdG targetId linhas 
            case dadosGestor of
                Nothing -> putStrLn "Gestor não encontrado."
                Just gestor -> do
                    putStrLn "Procurando..."
                    threadDelay (2 * 1000000)
                    putStrLn "\nGestor encontrado!\n"
                    threadDelay (2 * 1000000)
                    mostrarGestor gestor
    hClose conexao

--- Funçao para atualizar gestor por ID
atualizarGestorPorId :: Int -> Manager -> IO ()
atualizarGestorPorId targetId novoGestor = do
  handle <- openFile "manager.txt" ReadMode
  contents <- hGetContents handle
  let linhas = lines contents
      ids = primeirosElementosG linhas
  if not (verificandoIdG (show targetId) ids)
    then putStrLn "Gestor não encontrado."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      let linhasAtualizadas = map (\linha ->
                                      if verificandoIdG (show targetId) (primeirosElementosG [linha])
                                          then atualizarDadosGestor linha novoGestor
                                          else linha) linhas
      hPutStr tempHandle (unlines linhasAtualizadas)
      hClose handle
      hClose tempHandle
      removeFile "manager.txt"
      renameFile tempName "manager.txt"

--- Funçao para atualizar os dados de um gestor
atualizarDadosGestor :: String -> Manager -> String
atualizarDadosGestor linha (Manager id cpfG nomeG enderecoG telefoneG dataNascimento) =
  let dadosAntigos = splitOn "," linha
      novosDados = [show id, if null cpfG then dadosAntigos !! 1 else cpfG,
                    if null nomeG then dadosAntigos !! 2 else nomeG,
                    if null enderecoG then dadosAntigos !! 3 else enderecoG,
                    if null telefoneG then dadosAntigos !! 4 else telefoneG,
                    if null dataNascimento then dadosAntigos !! 5 else dataNascimento]
  in intercalate "," novosDados

--- Funçao para remover gestor por ID
removerGestorPorId :: Int -> IO ()
removerGestorPorId targetId = do
    handle <- openFile "manager.txt" ReadMode
    contents <- hGetContents handle
    let linhas = lines contents
        ids = primeirosElementosG linhas
    if not (verificandoIdG (show targetId) ids)
      then putStrLn "Gestor não encontrado."
      else do
        (tempName, tempHandle) <- openTempFile "." "temp"
        let linhasFiltradas = filter (\linha -> not $ verificandoIdG (show targetId) (primeirosElementosG [linha])) linhas
        hPutStr tempHandle (unlines linhasFiltradas)
        hClose handle
        hClose tempHandle
        removeFile "manager.txt"  
        renameFile tempName "manager.txt" 

-- Função para ler o arquivo e contar o número de linhas
contarGestores :: FilePath -> IO Int
contarGestores arquivo = do
    conteudo <- readFile arquivo
    let linhas = lines conteudo
    return (length linhas)

--- Função para listar todos os gestores do arquivo "manager.txt"
listarTodosGestores :: IO ()
listarTodosGestores = do
    handle <- openFile "manager.txt" ReadMode
    assunto <- hGetContents handle
    let linhas = lines assunto
        gestores = mapMaybe parseManager linhas
    mostrarGestores gestores
    hClose handle

--- Função para imprimir os dados de uma lista de gestores
mostrarGestores :: [Manager] -> IO ()
mostrarGestores gestores =
    if null gestores
        then putStrLn "\nNenhum gestor encontrado."
        else mapM_ mostrarGestor gestores

--- Função para imprimir os dados de um gestor
mostrarGestor :: Manager -> IO ()
mostrarGestor gestor = putStrLn $ unlines
    [ "Id: " ++ show (managerId gestor)
    , "Nome: " ++ nomeG gestor
    , "Cpf: " ++ cpfG gestor
    , "Endereço: " ++ enderecoG gestor
    , "Telefone: " ++ telefoneG gestor
    , "Data de Nascimento: " ++ dataNascimento gestor
    ]

--- Função para converter uma linha do arquivo em um Manager
parseManager :: String -> Maybe Manager
parseManager linha = case splitOn "," linha of
    [managerId, cpfG, nomeG, dataNascimento, telefoneG, enderecoG] ->
        case readMaybe managerId of
            Just mId -> Just (Manager mId cpfG nomeG dataNascimento telefoneG enderecoG)
            Nothing -> Nothing  -- Se a conversão falhar, retornamos Nothing
    _ -> Nothing

--- Funcao para consultar gestor 

    --- Funções auxiliares

verificandoIdG :: String -> [String] -> Bool
verificandoIdG str xs = str `elem` xs

toStringManager :: Manager -> String
toStringManager gestor = intercalate ", " [show (managerId gestor), show (cpfG gestor), nomeG gestor, dataNascimento gestor, show (telefoneG gestor), enderecoG gestor]

primeirosElementosG :: [String] -> [String]
primeirosElementosG linhas = map (\linha -> head(splitOn","linha))linhas


filtrarIdG :: Int -> [String] -> Maybe Manager
filtrarIdG id listaG = do
    let listaP = primeirosElementosG listaG
        posicao = posicaoIdListaG id listaP
    --sabendo que a posicao da listaP e a mesma da listaG, com os mesmos valores
    let gestorDados = splitOn "," (listaG !! posicao)
    case gestorDados of
        [managerId, cpfG, nomeG, dataNascimento, telefoneG, enderecoG] ->
            Just (Manager (read managerId) cpfG nomeG dataNascimento telefoneG enderecoG)
        _ -> Nothing


-- Função que retorna a posição na lista do ID fornecido.
posicaoIdListaG :: Int -> [String] -> Int
posicaoIdListaG id lista = do
    let posUltimo = (length (lista) - 1)
    if id == read(lista !! posUltimo)
        then posUltimo
        else posicaoIdListaG id (take posUltimo lista)


-- Função para calcular o benefício de 10%
calcularBeneficio :: Float -> Float
calcularBeneficio salario = salario * 0.1

-- Função para imprimir a folha de pagamento
imprimirFolhaPagamento :: Funcionario -> IO ()
imprimirFolhaPagamento funcionario = do
  let salarioBruto = salario funcionario
      beneficio = calcularBeneficio salarioBruto
      salarioTotal = salarioBruto + beneficio
  putStrLn $ "| ID: " ++ show (funcId funcionario)
  putStrLn $ "| Nome: " ++ nome funcionario
  putStrLn $ "| Data de Ingresso: " ++ data_ingresso funcionario
  putStrLn $ "| Benefício: " ++ show beneficio
  putStrLn $ "| Salário Bruto: R$ " ++ show salarioBruto
  putStrLn $ "| Salário com Benefício (10%): R$ " ++ show salarioTotal
  putStrLn "========================================"
            
filtrarIdF :: Int -> [String] -> [[String]]
filtrarIdF id listaG = do
    let listaP = primeirosElementos listaG
        posicao = posicaoIdLista id listaP
    --sabendo que a posicao da listaP é a mesma da listaG, com os mesmos valores
    return (splitOn "," (listaG !! posicao))

folhaPagamentoFuncionario :: Int -> IO ()
folhaPagamentoFuncionario targetId = do
  conexao <- openFile "haskell/funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      funcionarioInfos = filtrarId targetId linhas
  case listToMaybe funcionarioInfos of
    Nothing -> putStrLn "ID não encontrado."
    Just [] -> putStrLn "Funcionário não encontrado." 
    Just (idStr:nome:cpf:endereco:telefone:dataIngresso:salarioStr:_) -> do
      let funcionario = Funcionario { funcId = read idStr, nome = nome, cpf = cpf, endereco = endereco, telefone = telefone, data_ingresso = dataIngresso, salario = read salarioStr }
      putStrLn "\n========== FOLHA DE PAGAMENTO =========="
      imprimirFolhaPagamento funcionario
  hClose conexao

type Valor = Double
type Plano = String
type Salario = Double

-- Função para calcular o valor total recebido pela academia
calcularReceita :: [(Valor, Plano)] -> Double
calcularReceita = sum . map fst

-- Função para contar a quantidade de alunos em cada plano
contarAlunosPorPlano :: [(Valor, Plano)] -> [(Plano, Int)]
contarAlunosPorPlano = map (\xs -> (snd $ head xs, length xs)) . groupBy ((==) `on` snd) . sortOn snd

-- Função para calcular a média de pagamento por aluno
calcularMediaPagamento :: [(Valor, Plano)] -> Double
calcularMediaPagamento pagamentos = (calcularReceita pagamentos) / fromIntegral (length pagamentos)

-- Função para calcular o total de dinheiro pago aos funcionários
calcularSalarios :: [String] -> Double
calcularSalarios = sum . map (read . last . splitOn ",")

-- Função principal para gerar o relatório financeiro
gerarRelatorio :: IO ()
gerarRelatorio = do
    pagamentosTxt <- readFile "pagamentos.txt"
    funcionariosTxt <- readFile "haskell/funcionario.txt"
    let pagamentos = map ((\[v, p] -> (read v, p)) . splitOn ",") (lines pagamentosTxt)
    let funcionarios = lines funcionariosTxt
    let receitaAcademia = calcularReceita pagamentos
    let alunosPorPlano = contarAlunosPorPlano pagamentos
    let mediaPagamento = calcularMediaPagamento pagamentos
    let totalSalarios = calcularSalarios funcionarios
    putStrLn "========== Relatório Financeiro da Academia =========="
    putStrLn $ "| Alunos por Plano: " ++ show alunosPorPlano
    putStrLn $ "| Média de Pagamento por Aluno: " ++ show mediaPagamento
    putStrLn $ "| Total de Salários dos Funcionários: " ++ show totalSalarios
    putStrLn $ "| Receita Total: " ++ show receitaAcademia
    putStrLn "======================================================="
    

limpar :: IO ()
limpar = do
      clearScreen
      setCursorPosition 0 0
