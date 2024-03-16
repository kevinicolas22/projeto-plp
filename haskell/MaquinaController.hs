import Maquina

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

-- Função para adicionar uma maquina arquivo
adicionarMaquina :: Maquina -> IO ()
adicionarMaquina nova_maquina = do
  conexao <- openFile "maquina.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas
      idNovo = maquinaId nova_maquina
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
      putStrLn "Digite a data de manutenção da máquina: "
      dataStr <- getLine
      let dataManut = read dataStr :: Int
      return (Maquina id nome dataManut)


-- Função para converter uma maquina em uma string no formato esperado
toStringMaquina :: Maquina -> String
toStringMaquina (Maquina id nome) =
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
      idNovo = maquinaId reparo_maquina
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