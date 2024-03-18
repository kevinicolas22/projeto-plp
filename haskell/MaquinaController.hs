module MaquinaController where

import Maquina
import Data.List (intercalate)
import System.IO
import Data.List (sortOn)
import Data.List.Split
import Data.Maybe (mapMaybe, maybeToList)
import Data.List (sortOn)

--  Função que extrai os primeiros elementos de uma lista de strings.
primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head(splitOn","linha))linhas


-- Função que verifica se uma string está presente em uma lista de strings.
verificandoId :: String -> [String] -> Bool
verificandoId str xs = str `elem` xs

-- Função para adicionar uma maquina arquivo
adicionarMaquina :: Maquina -> IO ()
adicionarMaquina nova_maquina = do
  conexao <- openFile "maquina.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas
      idNovo = codigoMaquina nova_maquina  
  if verificandoId (show idNovo) ids
    then putStrLn "ID já em uso. Escolha um ID diferente."
    else appendFile "maquina.txt" (toStringMaquina nova_maquina ++ "\n")
  hClose conexao

-- Função para criar uma nova maquina
criarMaquina :: IO Maquina
criarMaquina = do
    putStrLn "Digite o ID da máquina: "
    id <- getLine
    conexao <- openFile "maquina.txt" ReadMode
    conteudo <- hGetContents conexao
    let linhas = lines conteudo
        ids = primeirosElementos linhas

    if id `elem` ids
        then do
            putStrLn "ID já em uso. Escolha um ID diferente."
            hClose conexao
            criarMaquina
        else do
            putStrLn "Digite o nome da máquina: "
            nome <- getLine
            putStrLn "Digite a data de manutenção da máquina (formato: dd/mm/aaaa): "
            dataStr <- getLine
            case delimitarManutencao dataStr of
                Just manutencaoDelimitada -> do
                    let dataManut = read manutencaoDelimitada :: DataManutencao
                    return (Maquina id nome dataManut)
                Nothing -> do
                    putStrLn "Data de manutenção inválida. Por favor, digite novamente."
                    criarMaquina



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

-- Função para adicionar uma maquina arquivo
adicionarMaquinaReparo :: Maquina -> IO ()
adicionarMaquinaReparo reparo_maquina = do
  conexao <- openFile "maquina_reparo.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas
      idNovo = codigoMaquina reparo_maquina
  if verificandoId (show idNovo) ids
    then putStrLn "ID inexistente. Escolha um ID diferente."
    else appendFile "maquina_reparo.txt" (toStringMaquina reparo_maquina ++ "\n")
  hClose conexao

-- Função para ler o arquivo e imprimir todas as máquinas cadastradas
imprimirMaquinasReparo :: FilePath -> IO ()
imprimirMaquinasReparo arquivo = do
    conteudo <- readFile arquivo
    let linhas = lines conteudo
    mapM_ putStrLn linhas


-- ler maquinas
lerMaquinas :: FilePath -> IO()
lerMaquinas arquivo = do
  conteudo <- readFile arquivo
  let linhas = lines conteudo
  putStrLn "Máquinas Cadatradas"
  mapM_ mostrarMaquinas linhas  

-- funcao auxiliar de ler maquinas
mostrarMaquinas :: String -> IO ()
mostrarMaquinas linha = 
  let [idStr, nome, _] = splitOn "," linha
      id = read idStr :: Codigo
  in putStrLn $ "ID: " ++ idStr ++ " | Nome: " ++ nome


  -- listar maquinas em ordem de manutenção
listarMaquinaPorManutencao :: [Maquina] -> [Maquina]
listarMaquinaPorManutencao maquinas = sortOn dataManutencao maquinas