{-# LANGUAGE PackageImports #-}
module ManagerService where

-- importações

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
import Data.Char (isSpace)


-- Função para criar um gestor com ID automático
criarGestor :: IO Manager
criarGestor = do
    -- Tente abrir o arquivo, criando-o se não existir
    handle <- openFile "manager.txt" ReadWriteMode `catch` \e -> do
        let _ = e :: IOError
        openFile "manager.txt" WriteMode

    -- Leia o conteúdo do arquivo e verifique se o ID já existe
    assunto <- hGetContents handle
    let linhas = lines assunto
        ids = primeirosElementosG linhas

    idGestor <- gerarIdUnico 1 ids

    -- Se o ID gerado já existe, gere um novo
    if idGestor `elem` ids
        then do
            putStrLn "ID já em uso. Escolha um ID diferente."
            threadDelay (1 * 1000000)
            hClose handle  -- Feche o arquivo antes de chamar a função recursiva
            criarGestor
        else do
            hClose handle  -- Feche o arquivo antes de prosseguir
            let idInt = read idGestor :: Int
            putStrLn "══════════════CADASTRO/GESTOR═══════════\n"
            putStrLn $ ">> Seu ID: " ++ show idInt
            putStrLn "\n>> Digite o seu CPF (formatação 000.000.000-00): "
            tentativaCpf <- cpfFormatado
            cpfG <- cpfCorreto tentativaCpf linhas
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
                                    return (Manager (idInt) cpfGDelimitado nomeG nascimentoDelimitado telefoneGDelimitado enderecoG)
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


-- Função para gerar um ID único
gerarIdUnico :: Int -> [String] -> IO String
gerarIdUnico count ids = do
    if show count `elem` ids
        then gerarIdUnico (count + 1) ids
        else return (show count)


--- Funçao para adicionar gestor ao arquivo txt de manager, adicionar o cpf e uma senha para
-- o txt de login, assim o sistema faz o cadastro de gestor

adicionarGestor :: Manager -> String -> String -> IO ()
adicionarGestor novo_gestor cpfGestor senha = do
  conexao <- openFile "manager.txt" ReadMode
  assunto <- hGetContents conexao
  let linhas = lines assunto
      ids = primeirosElementosG linhas
      idNovo = managerId novo_gestor
  if (verificandoIdG (show idNovo) ids) && (repeticaoCpf cpfGestor linhas)
    then putStrLn "CPF já em uso. Escolha outro "
    else do 
        appendFile"haskell/login.txt"(cpfGestor++","++senha++",2"++"\n")
        appendFile "manager.txt" (toStringManager novo_gestor ++ "\n")
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


-- Função para verificar o id de gestor
verificandoIdG :: String -> [String] -> Bool
verificandoIdG str xs = str `elem` xs

-- Função converte um objeto Manager em uma string 
-- formatada contendo todos os seus atributos separados por ","
toStringManager :: Manager -> String
toStringManager gestor = intercalate ", " [show (managerId gestor), show (cpfG gestor), nomeG gestor, dataNascimento gestor, show (telefoneG gestor), enderecoG gestor]

-- Pega uma lista de String e retorna outra lista de String,
-- contendo apenas o primeiro elemento de cada string
primeirosElementosG :: [String] -> [String]
primeirosElementosG linhas = map (\linha -> head(splitOn","linha))linhas

-- Função para filtrar um Id de Gestor
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
            
--Muda filtrar e folha
filtrarFuncionarioPorId :: Int -> [String] -> Maybe Funcionario
filtrarFuncionarioPorId targetId linhas = do
  linha <- find (\linha -> case splitOn ";" linha of { (idStr:_) -> readMaybe idStr == Just targetId; _ -> False }) linhas
  let [idStr, nome, cpf, endereco, telefone, dataIngresso, salarioStr] = splitOn ";" linha
  salario <- readMaybe salarioStr
  return (Funcionario { funcId = read idStr, nome = nome, cpf = cpf, endereco = endereco, telefone = telefone, data_ingresso = dataIngresso, salario = salario })

-- Função para imprimir a folha de pagamento de um funcionário por ID
folhaPagamentoFuncionario :: Int -> IO ()
folhaPagamentoFuncionario targetId = do
  conexao <- openFile "haskell/funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
  case filtrarFuncionarioPorId targetId linhas of
    Nothing -> putStrLn "Funcionário não encontrado."
    Just funcionario -> do
      putStrLn "\n========== FOLHA DE PAGAMENTO =========="
      imprimirFolhaPagamento funcionario
  hClose conexao

-- tipos auxiliares para usar na parte Financeira
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
calcularSalarios = sum . map (read . last . splitOn ";")

-- Função principal para gerar o relatório financeiro
gerarRelatorio :: IO ()
gerarRelatorio = do
    pagamentosTxt <- readFile "haskell/pagamentos.txt"
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
    putStrLn $ "| Receita Bruta: " ++ show receitaAcademia
    putStrLn $ "| Receita Líquida: " ++ show (receitaAcademia-totalSalarios)
    putStrLn "======================================================="
    

limpar :: IO ()
limpar = do
      clearScreen
      setCursorPosition 0 0

--Função

--Função recursiva para validar a repetição de cpf
cpfCorreto :: String -> [String] -> IO String
cpfCorreto cpf linhas= do
    if not (repeticaoCpf cpf linhas)
        then return cpf
        else do
            putStrLn "CPF já em uso. Tente outro"
            novaTentativa <- getLine
            cpfCorreto novaTentativa linhas

cpfFormatado :: IO String
cpfFormatado  = do
    entrada <- getLine
    case delimitarCpfG entrada of
        Just formato -> return formato
        Nothing -> do
            putStrLn $ "CPF inválido. Por favor, tente novamente."
            cpfFormatado

--Função para não aceitar repetição de cpf
repeticaoCpf :: String -> [String] -> Bool
repeticaoCpf cpf linhas = do
    let cpfs = segundoElementosG linhas
    cpf `elem` cpfs

--Funcao para pegar os cpfs do txt
-- Função para pegar os CPF
segundoElementosG :: [String] -> [String]
segundoElementosG linhas = map (removeEspacos . removeAspas . (!! 1) . splitOn ",") linhas
    where
        removeAspas = filter (/= '"')
        removeEspacos = dropWhile isSpace

senhaValida :: IO String
senhaValida  = do
    senha <- getLine
    if length senha >= 4
        then do
            return senha
        else do
            putStrLn "Senha tem que possuir no mínimo 4 caracters, tente novamente!"
            senhaValida  