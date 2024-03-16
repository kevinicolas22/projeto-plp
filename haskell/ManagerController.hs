module ManagerController where

import Manager
import System.IO
import System.Directory
import Data.List
import Data.List.Split


----- ---------criar gestor------------------

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
            putStrLn "Digite o seu CPF: "
            cpf <- getLine
            -- adicionar propriedade

            putStrLn "Digite o seu nome: "
            nome <- getLine

            putStrLn "Digite sua data de nascimento: "
            nascimento <- getLine
            -- adicionar propriedade

            putStrLn "Digite seu telefone: "
            telefone <- getLine
            -- adicionar propriedade

            putStrLn "Digite seu endereço: "
            endereco <- getLine

            return (Manager (read id) (read cpf) nome nascimento (read telefone) endereco)

------- adicionar gestor ao txt ----------

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



---- ler gestor por ID --------

lerGestorPorId :: Int -> IO()
lerGestorPorId targetId = do
    conexao <- openFile "manager.txt" ReadMode
    assunto <- hGetContents conexao
    let linhas = lines assunto
        ids = primeirosElementos linhas
    if not ( verificandoId(show targetId) ids)
        then putStrLn "ID não encontrado."
        else do 
            let dadosGestor = filtrarId targetId linhas 
            putStrLn "Gestor encontrado"
            mostrarGestor dadosGestor
    hClose conexao

    --- Atualizar gestor ---
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

-- Função para atualizar os dados de um gestor
atualizarDadosGestor :: String -> Manager -> String
atualizarDadosGestor linha (Manager id cpf nome endereco telefone dataNascimento) =
  let dadosAntigos = splitOn "," linha
      novosDados = [show id, if null cpf then dadosAntigos !! 2 else cpf,
                    if null nome then dadosAntigos !! 1 else nome,
                    if null endereco then dadosAntigos !! 5 else endereco,
                    if null telefone then dadosAntigos !! 4 else telefone,
                    if null birth then dadosAntigos !! 3 else dataNascimento]
  in intercalate "," novosDados

  --- remover gestor ---
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

    -- Funções auxiliares

verificandoId :: String -> [String] -> Bool
verificandoId str xs = str `elem` xs

toStringManager :: Manager -> String
toStringManager gestor = intercalate ", " [show (managerId gestor), show (cpf gestor), name gestor, birth gestor, show (telephone gestor), address gestor]

primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head (words (replace ',' ' ' linha))) linhas
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c : cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs


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

-- Função para imprimir os dados de um gestor representados por uma lista de listas de strings.
mostrarGestor :: [[String]] -> IO()
mostrarGestor [] = return ()
mostrarGestor (a:as) = do
    if length a >= 6 then
        putStrLn ("\nId: " ++ (a !! 0) ++
                "\nNome: " ++ (a !! 1) ++
                "\nCpf: " ++ (a !! 2) ++
                "\nEndereço: " ++ (a !! 3) ++
                "\nTelefones: " ++ (a !! 4) ++
                "\nData Ingresso: " ++ (a !! 5))
                ---- alterar os dados
    else
        putStrLn "A lista não contém dados suficientes para um gestor."
    mostrarGestor as