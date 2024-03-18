module FuncionarioController where

import Funcionario
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO
import Data.List.Split (splitOn)

--  Função que extrai os primeiros elementos de uma lista de strings.
primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head (words (replace ',' ' ' linha))) linhas
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c : cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs

-- Função que verifica se uma string está presente em uma lista de strings.
verificandoId :: String -> [String] -> Bool
verificandoId str xs = str `elem` xs

-- Função para adicionar um funcionário ao arquivo
adicionarFuncionario :: Funcionario -> IO ()
adicionarFuncionario novo_funcionario = do
  conexao <- openFile "funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas
      idNovo = funcId novo_funcionario
  if verificandoId (show idNovo) ids
    then putStrLn "ID já em uso. Escolha um ID diferente."
    else appendFile "funcionario.txt" (toStringFuncionario novo_funcionario ++ "\n")
  hClose conexao

-- Função para criar um novo funcionário
criarFuncionario :: IO Funcionario
criarFuncionario = do
  putStrLn "Digite o seu ID: "
  id <- getLine
  conexao <- openFile "funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas

  if id `elem` ids
    then do
      putStrLn "ID já em uso. Escolha um ID diferente."
      hClose conexao
      criarFuncionario
    else do
      putStrLn "Digite o seu nome: "
      nome <- getLine
        
      putStrLn "Digite seu CPF: "
      cpf <- getLine

      putStrLn "Digite seu endereço: "
      endereco <- getLine

      putStrLn "Digite seu telefone: "
      telefone <- getLine

      putStrLn "Digite sua data de ingresso: "
      data_ingresso <- getLine

      return (Funcionario (read id) nome cpf endereco telefone data_ingresso)


-- Função para ler um funcionário pelo ID e imprimir seus dados
lerFuncionarioPorId :: Int -> IO ()
lerFuncionarioPorId targetId = do
  conexao <- openFile "funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas
  if not (verificandoId (show targetId) ids)
    then putStrLn "ID não encontrado."
    else do
        let dadosFuncionario = filtrarId targetId linhas
        putStrLn "Funcionario encontrado"
        imprimindoFuncionario dadosFuncionario
  hClose conexao  

--- Função para listar todos os gestores do arquivo "funcionario.txt"
listarTodosFuncionarios :: IO () 
listarTodosFuncionarios = do
    handle <- openFile "funcionario.txt" ReadMode
    assunto <- hGetContents handle
    let linhas = lines assunto
        funcionarios = map (splitOn ",") linhas
        --funcionarios = mapMaybe parseFuncionario linhas
    if (length linhas) == 0
        then putStrLn "\nNenhum funcionario encontrado."
        else imprimindoFuncionario funcionarios
    hClose handle

-- Função que atualiza os dados de um funcionário no arquivo "funcionario.txt" com base no ID fornecido.
atualizarFuncionarioPorId :: Int -> Funcionario -> IO ()
atualizarFuncionarioPorId targetId novoFuncionario = do
  handle <- openFile "funcionario.txt" ReadMode
  contents <- hGetContents handle
  let linhas = lines contents
      ids = primeirosElementos linhas
  if not (verificandoId (show targetId) ids)
    then putStrLn "Funcionário não encontrado."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      let linhasAtualizadas = map (\linha ->
                                      if verificandoId (show targetId) (primeirosElementos [linha])
                                          then atualizarDadosFuncionario linha novoFuncionario
                                          else linha) linhas
      hPutStr tempHandle (unlines linhasAtualizadas)
      hClose handle
      hClose tempHandle
      removeFile "funcionario.txt"
      renameFile tempName "funcionario.txt"

-- Função para atualizar os dados de um funcionário em uma linha específica
atualizarDadosFuncionario :: String -> Funcionario -> String
atualizarDadosFuncionario linha (Funcionario id nome cpf endereco telefone dataIngresso) =
  let dadosAntigos = splitOn "," linha
      novosDados = [show id, if null nome then dadosAntigos !! 1 else nome,
                    if null cpf then dadosAntigos !! 2 else cpf,
                    if null endereco then dadosAntigos !! 3 else endereco,
                    if null telefone then dadosAntigos !! 4 else telefone,
                    if null dataIngresso then dadosAntigos !! 5 else dataIngresso]
  in intercalate "," novosDados

-- Função para remover um funcionário do arquivo pelo ID.
removerFuncionarioPorId :: Int -> IO ()
removerFuncionarioPorId targetId = do
  handle <- openFile "funcionario.txt" ReadMode
  contents <- hGetContents handle
  let linhas = lines contents
      ids = primeirosElementos linhas
  if not (verificandoId (show targetId) ids)
    then putStrLn "Funcionário não encontrado."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      let linhasFiltradas = filter (\linha -> not $ verificandoId (show targetId) (primeirosElementos [linha])) linhas
      hPutStr tempHandle (unlines linhasFiltradas)
      hClose handle
      hClose tempHandle
      removeFile "funcionario.txt"
      renameFile tempName "funcionario.txt"

-- Função auxiliar para converter uma lista de strings em um Funcionario
parseFuncionario :: [String] -> Maybe Funcionario
parseFuncionario [id, nome, cpf, endereco, telefone, dataIngresso] =
  Just (Funcionario (read id) nome cpf endereco telefone dataIngresso)
parseFuncionario _ = Nothing

-- Função para converter um funcionário em uma string no formato esperado
toStringFuncionario :: Funcionario -> String
toStringFuncionario (Funcionario id nome cpf endereco telefone data_ingresso) =
  intercalate "," [show id, nome, cpf, endereco, telefone, data_ingresso]

-- Função para filtrar os dados de um funcionário pelo ID
filtrarId :: Int -> [String] -> [[String]]
filtrarId id listaG = do
    let listaP = primeirosElementos listaG
        posicao = posicaoIdLista id listaP
    --sabendo que a posicao da listaP e a mesma da listaG, com os mesmos valores
    return (splitOn "," (listaG !! posicao))

-- Função que retorna a posição na lista do ID fornecido.
posicaoIdLista :: Int -> [String] -> Int
posicaoIdLista id lista = do
    let posUltimo = (length (lista) - 1)
    if id == read(lista !! posUltimo)
        then posUltimo
        else posicaoIdLista id (take posUltimo lista)

-- Função para imprimir os dados de um funcionário representados por uma lista de listas de strings.
imprimindoFuncionario :: [[String]] -> IO()
imprimindoFuncionario [] = return ()
imprimindoFuncionario (x:xs) = do
    if length x >= 6 then
        putStrLn ("\nId: " ++ (x !! 0) ++
                "\nNome: " ++ (x !! 1) ++
                "\nCpf: " ++ (x !! 2) ++
                "\nEndereço: " ++ (x !! 3) ++
                "\nTelefones: " ++ (x !! 4) ++
                "\nData Ingresso: " ++ (x !! 5))
    else
        putStrLn "A lista não contém dados suficientes para um funcionário."
    imprimindoFuncionario xs