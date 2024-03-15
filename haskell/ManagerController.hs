import Manager

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


primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head (words (replace ',' ' ' linha))) linhas
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c : cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs

----- verificar o ID --------
verificandoId :: String -> [String] -> Bool
verificandoId str xs = str `elem` xs

toStringManager :: Manager -> String
toStringManager gestor = intercalate ", " [show (managerId gestor), show (cpf gestor), name gestor, birth gestor, show (telephone gestor), address gestor]

---- ler gestor por ID --------

lerGestorPorId :: Int -> IO()
lerFuncionarioPorId targetId = do
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
