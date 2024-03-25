{-# LANGUAGE PackageImports #-}
module ManagerService where

import "directory" System.Directory
import Manager
import System.IO
import Data.List
import Data.List.Split
import Funcionario
import FuncionarioService
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


--- Funçao para criar Gestor e as delimitaçoes de cada dado do gestor 
criarGestor :: IO Manager
criarGestor = do
    putStrLn "Digite o seu ID de gestor: "
    id <- getLine
    conexao <- openFile "manager.txt" ReadMode
    assunto <- hGetContents conexao
    let linhas = lines assunto
        ids = primeirosElementosG linhas
    if id `elem` ids
        then do
            putStrLn "ID já em uso. Escolha um ID diferente."
            hClose conexao
            criarGestor
        else do
            putStrLn "Digite o seu cpfG (formatação 000.000.000-00): "
            cpfG <- getLine
            case delimitarCpfG cpfG of
                Just cpfGDelimitado -> do
                    putStrLn "Digite o seu nomeG: "
                    nomeG <- getLine

                    putStrLn "Digite sua data de nascimento (formato dd/mm/aaaa): "
                    nascimento <- getLine
                    case delimitarNascimento nascimento of
                        Just nascimentoDelimitado -> do
                            putStrLn "Digite seu telefoneG (formato DDD000000000): "
                            telefoneG <- getLine
                            case delimitarTelefoneG telefoneG of
                                Just telefoneGDelimitado -> do
                                    putStrLn "Digite seu endereço: "
                                    enderecoG <- getLine
                                    return (Manager (read id) (show cpfG) nomeG nascimentoDelimitado (show telefoneGDelimitado) enderecoG)
                                Nothing -> do
                                    putStrLn "telefoneG inválido. Por favor, digite novamente."
                                    criarGestor
                        Nothing -> do
                            putStrLn "Data de nascimento inválida. Por favor, digite novamente."
                            criarGestor
                Nothing -> do
                    putStrLn "cpfG inválido. Por favor, digite novamente."
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
                    putStrLn "Gestor encontrado"
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

--- Função para listar todos os gestores do arquivo "manager.txt"
listarTodosGestores :: IO ()
listarTodosGestores = do
    handle <- openFile "manager.txt" ReadMode
    assunto <- hGetContents handle
    let linhas = lines assunto
        gestores = mapMaybe parseManager linhas
    if null gestores
        then putStrLn "\nNenhum gestor encontrado."
        else mostrarGestores gestores
    hClose handle

--- Função para imprimir os dados de uma lista de gestores
mostrarGestores :: [Manager] -> IO ()
mostrarGestores gestores = mapM_ mostrarGestor gestores

--- Função para imprimir os dados de um gestor
mostrarGestor :: Manager -> IO ()
mostrarGestor gestor = putStrLn $ unlines
    [ "Id: " ++ show (managerId gestor)
    , "nomeG: " ++ nomeG gestor
    , "cpfG: " ++ cpfG gestor
    , "Endereço: " ++ enderecoG gestor
    , "telefoneG: " ++ telefoneG gestor
    , "Data de Nascimento: " ++ dataNascimento gestor
    ]

--- Função para converter uma linha do arquivo em um Manager
parseManager :: String -> Maybe Manager
parseManager linha = case splitOn "," linha of
    [managerId, cpfG, nomeG, dataNascimento, telefoneG, enderecoG] ->
        Just (Manager (read managerId) cpfG nomeG dataNascimento telefoneG enderecoG)
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

filtrarIdF :: Int -> [String] -> [[String]]
filtrarIdF id listaG = do
    let listaP = primeirosElementos listaG
        posicao = posicaoIdLista id listaP
    --sabendo que a posicao da listaP é a mesma da listaG, com os mesmos valores
    return (splitOn "," (listaG !! posicao))

folhaPagamentoFuncionario :: Int -> IO ()
folhaPagamentoFuncionario targetId = do
  conexao <- openFile "funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      funcionarioInfos = filtrarId targetId linhas
  case listToMaybe funcionarioInfos of
    Nothing -> putStrLn "ID não encontrado."
    Just [] -> putStrLn "Funcionário não encontrado."
    Just (idStr:nome:cpf:endereco:telefone:dataIngresso:salarioStr:_) -> do
      let funcionario = Funcionario { funcId = read idStr, nome = nome, cpf = cpf, endereco = endereco, telefone = telefone, data_ingresso = dataIngresso, salario = read salarioStr }
      putStrLn "FOLHA DE PAGAMENTO:"
      imprimirFolhaPagamento funcionario
  hClose conexao


