module MaquinaService where

import Maquina
import Data.List (intercalate)
import System.IO
import Data.List (sortOn)
import Text.Read (readMaybe)
import Data.List (find)
import Data.List.Split
import Data.Maybe (mapMaybe, maybeToList)
import Data.List.Split (splitOn)
import Control.Exception (catch, IOException)


--  Função que extrai os primeiros elementos de uma lista de strings.
primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head(splitOn","linha))linhas


-- Função que verifica se uma string está presente em uma lista de strings.
verificandoIdG :: String -> [String] -> Bool
verificandoIdG str xs = str `elem` xs

-- Função para adicionar uma maquina arquivo
adicionarMaquina :: Maquina -> IO ()
adicionarMaquina nova_maquina = do
  conexao <- openFile "maquina.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas
      idNovo = codigoMaquina nova_maquina  
  if verificandoIdG (show idNovo) ids
    then putStrLn "ID já em uso. Escolha um ID diferente."
    else appendFile "maquina.txt" (toStringMaquina nova_maquina ++ "\n")
  hClose conexao

-- Função para criar uma nova maquina
criarMaquina :: IO Maquina
criarMaquina = do
  putStrLn "Digite o seu ID: "
  targetId <- getLine 
  let id = read targetId :: Int
  conexao <- openFile "maquina.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas

  if verificandoIdM (show id) ids
    then do
      putStrLn "ID já em uso. Escolha um ID diferente."
      hClose conexao
      criarMaquina
    else do
      putStrLn "Digite o nome da máquina: "
      nome <- getLine

      putStrLn "Digite a data de manutenção (formato: ddmmaaaa): "
      dataManutencao <- obterInformacaoM "data de manutenção" delimitarManutencao

      return (Maquina id nome dataManutencao)

verificandoIdM :: String -> [String] -> Bool
verificandoIdM str xs = str `elem` xs

parseMaquina :: String -> Maybe Maquina
parseMaquina linha = case splitOn "," linha of
    [codigo, nome, dataManutencao] ->
        Just (Maquina (read codigo) nome (read dataManutencao))
    _ -> Nothing

obterInformacaoM :: String -> (String -> Maybe String) -> IO String
obterInformacaoM tipo validador = do
    entrada <- getLine
    case validador entrada of
        Just formato -> return formato
        Nothing -> do
            putStrLn $ tipo ++ " inválido. Por favor, tente novamente."
            obterInformacaoM tipo validador

-- Função para converter uma maquina em uma string no formato esperado
toStringMaquina :: Maquina -> String
toStringMaquina (Maquina id nome dataManutencao) =  
  intercalate "," [show id, nome, show dataManutencao]  

-- Função para ler o arquivo e contar o número de linhas
contarMaquinas :: FilePath -> IO Int
contarMaquinas arquivo = do
    conteudo <- readFile arquivo
    let linhas = lines conteudo
    return (length linhas)

adicionarMaquinaReparo :: String -> IO ()
adicionarMaquinaReparo idStr = do
    let id = read idStr :: Int  -- Convertendo o ID fornecido para um número inteiro
    -- Abre o arquivo "maquinas.txt" em modo de leitura
    handleMaquinas <- openFile "haskell/maquina.txt" ReadMode
    -- Lê o conteúdo do arquivo "maquinas.txt"
    conteudoMaquinas <- hGetContents handleMaquinas
    -- Encontra a linha correspondente ao ID fornecido
    let maquina = find (\m -> codigoMaquina m == id) (parseMaquinas conteudoMaquinas)
    case maquina of
        Just m -> do
            -- Abre o arquivo "maquina_reparo.txt" em modo de anexar
            handleReparo <- openFile "haskell/maquina_reparo.txt" AppendMode
            -- Adiciona o ID e o nome da máquina ao arquivo "maquina_reparo.txt"
            hPutStrLn handleReparo (show (codigoMaquina m) ++ ", " ++ nomeMaquina m)
            -- Fecha o arquivo "maquina_reparo.txt"
            hClose handleReparo
            -- Fecha o arquivo "maquinas.txt"
            hClose handleMaquinas
            putStrLn "Máquina adicionada com sucesso!"
        Nothing -> do
            hClose handleMaquinas
            putStrLn "Máquina não encontrada com o ID fornecido."



-- Função para analisar o conteúdo do arquivo "maquinas.txt"
parseMaquinas :: String -> [Maquina]
parseMaquinas conteudo = mapMaybe parseMaquina (lines conteudo)




-- Função para ler o arquivo e imprimir todas as máquinas cadastradas
imprimirMaquinasReparo :: FilePath -> IO ()
imprimirMaquinasReparo arquivo = do
    conteudo <- readFile arquivo
    let linhas = lines conteudo
    putStrLn ">> Máquinas com necessidade de reparo <<"
    mapM_ mostrarMaquinas linhas


-- ler maquinas
lerMaquinas :: FilePath -> IO()
lerMaquinas arquivo = do
  conteudo <- readFile arquivo
  let linhas = lines conteudo
  putStrLn ">> Máquinas Cadatradas <<\n"
  mapM_ mostrarMaquinas linhas  

-- funcao auxiliar de ler maquinas
mostrarMaquinas :: String -> IO ()
mostrarMaquinas linha = 
  let [idStr, nome, _] = splitOn "," linha
      id = read idStr :: Codigo
  in putStrLn $ "| Nome: " ++ nome

-- funcao auxiliar de ler maquinas
mostrarMaquinasM :: String -> IO ()
mostrarMaquinasM linha = 
  let [idStr, nome, _] = splitOn "," linha
      id = read idStr :: Codigo
  in putStrLn $ "ID: " ++ idStr ++ " | Nome: " ++ nome


-- Função auxiliar para ler uma máquina do arquivo
lerMaquinaR :: String -> Maybe Maquina
lerMaquinaR linha =
  case splitOn "," linha of
    [idStr, nome, dataManutencaoStr] ->
      case readMaybe idStr of
        Just id -> Just (Maquina id nome dataManutencaoStr)
        Nothing -> Nothing
    _ -> Nothing

-- Função para mostrar o nome e a data de manutenção de uma máquina
mostrarMaquina :: Maquina -> IO ()
mostrarMaquina (Maquina _ nome dataManutencao) =
  putStrLn $ "Nome: " ++ nome ++ " | Data de Manutenção: " ++ dataManutencao

-- Função para listar as máquinas em ordem alfabética
listarMaquinasOrdemAlfabetica :: IO ()
listarMaquinasOrdemAlfabetica = do
  conteudo <- readFile "haskell/maquina_reparo.txt"
  let linhas = lines conteudo
      maquinas = mapMaybe lerMaquinaR linhas
      maquinasOrdenadas = sortOn nomeMaquina maquinas
  mapM_ mostrarMaquina maquinasOrdenadas
