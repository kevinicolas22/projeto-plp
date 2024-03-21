module FuncionarioService where

import Funcionario
import Treino
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (elemIndices)
import AvaliacaoFisica



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
  id <- readLn :: IO Int
  conexao <- openFile "funcionario.txt" ReadMode
  conteudo <- hGetContents conexao
  let linhas = lines conteudo
      ids = primeirosElementos linhas

  if verificandoId (show id) ids
    then do
      putStrLn "ID já em uso. Escolha um ID diferente."
      hClose conexao
      criarFuncionario
    else do
      putStrLn "Digite o seu nome: "
      nome <- getLine

      putStrLn "Digite o seu CPF 11 dígitos ex: 12345678900: "
      cpf <- obterInformacao "CPF" delimitarCpf

      putStrLn "Digite seu endereço: "
      endereco <- getLine

      putStrLn "Digite o seu telefone 11 dígitos ex: 08391234567"
      telefone <- obterInformacao "telefone" delimitarTelefone

      putStrLn "Digite a data de ingresso 8 dígitos ex: DDMMAAAA"
      data_ingresso <- obterInformacao "data de ingresso" delimitarIngresso

      putStrLn "Digite seu salário: "
      salario <- readLn :: IO Float

      return (Funcionario id nome cpf endereco telefone data_ingresso salario)


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
atualizarDadosFuncionario linha (Funcionario id nome cpf endereco telefone dataIngresso salario) =
  let dadosAntigos = splitOn "," linha
      novosDados = [show id, if null nome then dadosAntigos !! 1 else nome,
                    if null cpf then dadosAntigos !! 2 else cpf,
                    if null endereco then dadosAntigos !! 3 else endereco,
                    if null telefone then dadosAntigos !! 4 else telefone,
                    if null dataIngresso then dadosAntigos !! 5 else dataIngresso,
                    if salario == 0 then dadosAntigos !! 6 else show salario]
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
parseFuncionario [id, nome, cpf, endereco, telefone, dataIngresso, salario] =
  Just (Funcionario (read id) nome cpf endereco telefone dataIngresso (read salario))
parseFuncionario _ = Nothing

-- Função para converter um funcionário em uma string no formato esperado
toStringFuncionario :: Funcionario -> String
toStringFuncionario (Funcionario id nome cpf endereco telefone data_ingresso salario) =
  intercalate "," [show id, nome, cpf, endereco, telefone, data_ingresso, show salario]

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
                "\nData Ingresso: " ++ (x !! 5) ++
                "\nSalário: " ++ (x !! 6))
    else
        putStrLn "A lista não contém dados suficientes para um funcionário."
    imprimindoFuncionario xs


--- Funçao para delimitar o telefone
delimitarTelefone :: String -> Maybe String
delimitarTelefone telefone
    | length numeros == 11 = Just telefone
    | otherwise = Nothing
    where
        numeros = filter isDigit telefone

--- Função data de ingresso delimitar em 00/00/0000
delimitarIngresso :: String -> Maybe String
delimitarIngresso data_ingresso
    | length numeros == 8 = Just dataFormatada
    | otherwise = Nothing
    where
        numeros = filter isDigit data_ingresso
        dia = take 2 numeros
        mes = take 2(drop 2 numeros)
        ano = drop 4 numeros
        dataFormatada = intercalate "/"[dia,mes,ano]

--- Função delimitar CPF 11 numeros 000.000.000-00
delimitarCpf :: String -> Maybe String
delimitarCpf cpf
    | length numeros == 11 = Just cpfFormatado
    | otherwise = Nothing
    where
        numeros = filter isDigit cpf
        cpfFormatado = intercalate "."[chunk 0 3, chunk 3 6, chunk 6 9] ++ "-" ++ take 2(drop 9 numeros)
        chunk start end = take(end - start)(drop start numeros)

        
obterInformacao :: String -> (String -> Maybe String) -> IO String
obterInformacao tipo validador = do
    entrada <- getLine
    case validador entrada of
        Just formato -> return formato
        Nothing -> do
            putStrLn $ tipo ++ " inválido. Por favor, tente novamente."
            obterInformacao tipo validador


-- Definição do tipo AvaliacaoFisica
-- Função para criar uma nova avaliação física
criarAvaliacaoFisica :: IO AvaliacaoFisica.AvaliacaoFisica
criarAvaliacaoFisica = do
    putStrLn "Digite a data da avaliação (DD/MM/AAAA):"
    dataAvaliacao <- obterInformacao "Data da avaliação" delimitarData
    putStrLn "Digite seu peso:"
    peso <- readLn :: IO Float
    putStrLn "Digite a sua altura (1.80):"
    altura <- readLn :: IO Float
    putStrLn "Digite sua idade:"
    idade <- readLn :: IO Int
    putStrLn "Digite seu objetivo:"
    objetivo <- getLine
    return (AvaliacaoFisica.AvaliacaoFisica dataAvaliacao (Atributos peso altura idade objetivo))


-- Função auxiliar para delimitar a data no formato "DD/MM/AAAA"
delimitarData :: String -> Maybe String
delimitarData dataAvaliacao
    | length numeros == 8 = Just dataFormatada
    | otherwise = Nothing
    where
        numeros = filter isDigit dataAvaliacao
        dia = take 2 numeros
        mes = take 2 (drop 2 numeros)
        ano = drop 4 numeros
        dataFormatada = intercalate "/" [dia, mes, ano]

-- Função para adicionar uma avaliação física ao arquivo
adicionarAvaliacaoFisica :: IO ()
adicionarAvaliacaoFisica = do
    nova_avaliacao <- criarAvaliacaoFisica
    appendFile "avaliacoes_fisicas.txt" (toStringAvaliacaoFisica nova_avaliacao ++ "\n")


-- Função auxiliar para converter uma lista de strings em uma AvaliacaoFisica
parseAvaliacaoFisica :: String -> Maybe AvaliacaoFisica.AvaliacaoFisica
parseAvaliacaoFisica linha = case words linha of
    [dataAvaliacao, peso, altura, idade, objetivo] ->
        Just (AvaliacaoFisica.AvaliacaoFisica dataAvaliacao (Atributos (read peso) (read altura) (read idade) (read objetivo)))
    _ -> Nothing

-- Função para converter uma AvaliacaoFisica em uma string no formato esperado
toStringAvaliacaoFisica :: AvaliacaoFisica.AvaliacaoFisica -> String
toStringAvaliacaoFisica (AvaliacaoFisica.AvaliacaoFisica dataAvaliacao atributos) =
    intercalate " "
        [show dataAvaliacao, show (peso atributos), show (altura atributos), show (idade atributos), show (objetivo atributos)]


-- Função para calcular o idade e determinar a faixa
calcularidade :: Float -> Float -> String
calcularidade peso altura
    | altura <= 0 = "Altura inválida"
    | peso <= 0 = "Peso inválido"
    | otherwise = faixaidade (peso / (altura * altura))

-- Função para determinar a faixa do idade
faixaidade :: Float -> String
faixaidade imc
    | imc < 16.0 = "Baixo peso Grau III"
    | imc < 16.9 = "Baixo peso Grau II"
    | imc < 18.4 = "Baixo peso Grau I"
    | imc < 24.9 = "Peso normal"
    | imc < 29.9 = "Sobrepeso"
    | imc < 34.9 = "Obesidade Grau I"
    | imc < 39.9 = "Obesidade Grau II"
    | otherwise = "Obesidade Grau III"

--TREINO

--Função para cadastrar TREINO (primeiro paramêtro por enquanto é uma String)
cadastraTreino :: Int -> String -> String -> IO()
cadastraTreino matricula tipo_treino descricao = do
  let novoTreino = Treino matricula tipo_treino descricao
  appendFile "treino.txt" (toString novoTreino ++ "\n")