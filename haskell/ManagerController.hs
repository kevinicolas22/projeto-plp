{-# LANGUAGE PackageImports #-}
module ManagerController where

import "directory" System.Directory
import Manager
import System.IO
import Data.List
import Data.List.Split
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
        ids = primeirosElementos linhas
    if read id `elem` ids
        then do
            putStrLn "ID já em uso. Escolha um ID diferente."
            hClose conexao
            criarGestor
        else do
            putStrLn "Digite o seu CPF (formatação 000.000.000-00): "
            cpf <- getLine
            case delimitarCpf cpf of
                Just cpfDelimitado -> do
                    putStrLn "Digite o seu nome: "
                    nome <- getLine

                    putStrLn "Digite sua data de nascimento (formato dd/mm/aaaa): "
                    nascimento <- getLine
                    case delimitarNascimento nascimento of
                        Just nascimentoDelimitado -> do
                            putStrLn "Digite seu telefone (formato DDD000000000): "
                            telefone <- getLine
                            case delimitarTelefone telefone of
                                Just telefoneDelimitado -> do
                                    putStrLn "Digite seu endereço: "
                                    endereco <- getLine
                                    return (Manager (read id) (read cpf) nome nascimentoDelimitado (read telefoneDelimitado) endereco)
                                Nothing -> do
                                    putStrLn "Telefone inválido. Por favor, digite novamente."
                                    criarGestor
                        Nothing -> do
                            putStrLn "Data de nascimento inválida. Por favor, digite novamente."
                            criarGestor
                Nothing -> do
                    putStrLn "CPF inválido. Por favor, digite novamente."
                    criarGestor



--- Funçao para adicionar gestor ao arquivo txt(banco de dados)
adicionarGestor :: Manager -> IO ()
adicionarGestor novo_gestor = do
  conexao <- openFile "manager.txt" ReadMode
  assunto <- hGetContents conexao
  let linhas = lines assunto
      ids = primeirosElementos linhas
      idNovo = managerId novo_gestor
  if verificandoId (show idNovo) ids
    then putStrLn "ID já em uso. Escolha um ID diferente."
    else appendFile "manager.txt" (toStringManager novo_gestor ++ "\n")
  hClose conexao


--- Funçao para  ler o Gestor por Id
lerGestorPorId :: Int -> IO ()
lerGestorPorId targetId = do
    conexao <- openFile "manager.txt" ReadMode
    assunto <- hGetContents conexao
    let linhas = lines assunto
        ids = primeirosElementos linhas
    if not (verificandoId (show targetId) ids)
        then putStrLn "ID não encontrado."
        else do 
            let dadosGestor = filtrarId targetId linhas 
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
      ids = primeirosElementos linhas
  if not (verificandoId (show targetId) ids)
    then putStrLn "Gestor não encontrado."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      let linhasAtualizadas = map (\linha ->
                                      if verificandoId (show targetId) (primeirosElementos [linha])
                                          then atualizarDadosGestor linha novoGestor
                                          else linha) linhas
      hPutStr tempHandle (unlines linhasAtualizadas)
      hClose handle
      hClose tempHandle
      removeFile "manager.txt"
      renameFile tempName "manager.txt"

--- Funçao para atualizar os dados de um gestor
atualizarDadosGestor :: String -> Manager -> String
atualizarDadosGestor linha (Manager id cpf nome endereco telefone dataNascimento) =
  let dadosAntigos = splitOn "," linha
      novosDados = [show id, if null cpf then dadosAntigos !! 2 else cpf,
                    if null nome then dadosAntigos !! 1 else nome,
                    if null endereco then dadosAntigos !! 5 else endereco,
                    if null telefone then dadosAntigos !! 4 else telefone,
                    if null dataNascimento then "" else dataNascimento]
  in intercalate "," novosDados

--- Funçao para remover gestor por ID
removerGestorPorId :: Int -> IO ()
removerGestorPorId targetId = do
    handle <- openFile "manager.txt" ReadMode
    contents <- hGetContents handle
    let linhas = lines contents
        ids = primeirosElementos linhas
    if not (verificandoId (show targetId) ids)
      then putStrLn "Gestor não encontrado."
      else do
        (tempName, tempHandle) <- openTempFile "." "temp"
        let linhasFiltradas = filter (\linha -> not $ verificandoId (show targetId) (primeirosElementos [linha])) linhas
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
    , "Nome: " ++ nome gestor
    , "CPF: " ++ cpf gestor
    , "Endereço: " ++ endereco gestor
    , "Telefone: " ++ telefone gestor
    , "Data de Nascimento: " ++ dataNascimento gestor
    ]

--- Função para converter uma linha do arquivo em um Manager
parseManager :: String -> Maybe Manager
parseManager linha = case splitOn "," linha of
    [managerId, cpf, nome, dataNascimento, telefone, endereco] ->
        Just (Manager (read managerId) cpf nome dataNascimento telefone endereco)
    _ -> Nothing



    --- Funções auxiliares

verificandoId :: String -> [String] -> Bool
verificandoId str xs = str `elem` xs

toStringManager :: Manager -> String
toStringManager gestor = intercalate ", " [show (managerId gestor), show (cpf gestor), nome gestor, dataNascimento gestor, show (telefone gestor), endereco gestor]

primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head(splitOn","linha))linhas


filtrarId :: Int -> [String] -> Maybe Manager
filtrarId id listaG = do
    let listaP = primeirosElementos listaG
        posicao = posicaoIdLista id listaP
    --sabendo que a posicao da listaP e a mesma da listaG, com os mesmos valores
    let gestorDados = splitOn "," (listaG !! posicao)
    case gestorDados of
        [managerId, cpf, nome, dataNascimento, telefone, endereco] ->
            Just (Manager (read managerId) cpf nome dataNascimento telefone endereco)
        _ -> Nothing

-- Função que retorna a posição na lista do ID fornecido.
posicaoIdLista :: Int -> [String] -> Int
posicaoIdLista id lista = do
    let posUltimo = (length (lista) - 1)
    if id == read(lista !! posUltimo)
        then posUltimo
        else posicaoIdLista id (take posUltimo lista)
