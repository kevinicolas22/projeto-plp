module Funcionario where

import DB
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO

-- Definição de tipos de dados
type Id = Int

type Nome = String

type Cpf = String

type Endereco = String

type Telefone = String

type Data_Ingresso = String

-- Definição de um funcionário
data Funcionario = Funcionario
  { funcId :: Id,
    nome :: Nome,
    cpf :: Cpf,
    endereco :: Endereco,
    telefone :: Telefone,
    data_ingresso :: Data_Ingresso
  }
  deriving (Show)

primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head (words (replace ',' ' ' linha))) linhas
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c : cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs

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
lerFuncionarioPorId :: Id -> IO ()
lerFuncionarioPorId targetId = do
  conteudo <- readFile "funcionario.txt"
  let linhas = lines conteudo
      funcionarios = mapMaybe (parseFuncionario . words) linhas
  case concatMap (\func -> maybeToList $ if funcId func == targetId then Just func else Nothing) funcionarios of
    [funcionario] -> putStrLn $ show funcionario
    _ -> putStrLn "Funcionário não encontrado"

-- Função para remover um funcionário pelo ID
removerFuncionarioPorId :: Id -> IO ()
removerFuncionarioPorId targetId = do
  dirAtual <- getCurrentDirectory
  let arquivo = dirAtual ++ "/funcionario.txt"
  conteudo <- withFile arquivo ReadMode hGetContents
  let linhas = lines conteudo
      funcionarios = mapMaybe (parseFuncionario . words) linhas
      funcionariosFiltrados = filter (\func -> funcId func /= targetId) funcionarios
      novoConteudo = unlines (map toStringFuncionario funcionariosFiltrados)
  writeFile arquivo novoConteudo
  putStrLn "Funcionário removido com sucesso!"

-- Função auxiliar para converter uma lista de strings em um Funcionario
parseFuncionario :: [String] -> Maybe Funcionario
parseFuncionario [id, nome, cpf, endereco, telefone, dataIngresso] =
  Just (Funcionario (read id) nome cpf endereco telefone dataIngresso)
parseFuncionario _ = Nothing

-- Função para converter um funcionário em uma string no formato esperado
toStringFuncionario :: Funcionario -> String
toStringFuncionario (Funcionario id nome cpf endereco telefone data_ingresso) =
  intercalate "," [show id, nome, cpf, endereco, telefone, data_ingresso]
