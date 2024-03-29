module FuncionarioService where

import Funcionario
import Treino
import Planos
import Aula
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (elemIndices)
import AvaliacaoFisica
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import MainAluno
import Aluno



--  Função que extrai os primeiros elementos de uma lista de strings.


-- Função que verifica se uma string está presente em uma lista de strings.
verificandoId :: String -> [String] -> Bool
verificandoId str xs = str `elem` xs


-- Função para adicionar um funcionário ao arquivo
adicionarFuncionario :: Funcionario -> IO ()
adicionarFuncionario novo_funcionario = do
  conexao <- openFile "haskell//funcionario.txt" ReadMode
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
  conexao <- openFile "haskell//funcionario.txt" ReadMode
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

-- AVALIAÇÃO FISICA 

-- Função para criar uma nova avaliação física
criarAvaliacaoFisica :: IO AvaliacaoFisica
criarAvaliacaoFisica = do
    conexaoAluno <- openFile "haskell/aluno.txt" ReadMode
    conteudoAluno<- hGetContents conexaoAluno
    putStrLn "Digite o ID da avaliação:"
    avaliacaoId <- readLn :: IO Int
    putStrLn "Digite a matricula do aluno:"
    matriculaAvaliacao <- getLine
    let matriculaExistente= existeMatricula matriculaAvaliacao conteudoAluno
    if not(matriculaExistente)
        then do
            putStrLn " > Matrícula não encontrada !"
            hClose conexaoAluno
            criarAvaliacaoFisica 
        else do
            conexao <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
            conteudo <- hGetContents conexao
            let linhas = lines conteudo
                ids = primeirosElementos linhas
            if verificandoId (show avaliacaoId) ids
            then do
                putStrLn "ID já em uso. Escolha um ID diferente."
                hClose conexaoAluno
                hClose conexao
                criarAvaliacaoFisica
            else do
                putStrLn "Digite a data da avaliação (DD/MM/AAAA):"
                dataAvaliacao <- obterInformacao "Data da avaliação" delimitarData
                putStrLn "Digite o peso:"
                peso <- readLn :: IO Float
                putStrLn "Digite a altura (1.80):"
                altura <- readLn :: IO Float
                putStrLn "Digite a idade:"
                idade <- readLn :: IO Int
                putStrLn "Digite seu objetivo:"
                objetivo <- getLine
                hClose conexaoAluno
                return (AvaliacaoFisica avaliacaoId dataAvaliacao peso altura idade objetivo matriculaAvaliacao)

lerAvaliacaoFisicaPorId :: Int -> IO ()
lerAvaliacaoFisicaPorId targetId = do
    conexao <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
    conteudo <- hGetContents conexao
    let linhas = lines conteudo
        ids = primeirosElementos linhas
    if not (verificandoId (show targetId) ids)
        then putStrLn "ID não encontrado."
        else do
            let dadosAvaliacao = filtrarId targetId linhas
            putStrLn "Avaliação física encontrada:"
            imprimindoAvaliacaoFisica dadosAvaliacao
    hClose conexao  


-- Função para listar todas as avaliações físicas do arquivo "avaliacoes_fisicas.txt"
listarTodasAvaliacoesFisicas :: IO ()
listarTodasAvaliacoesFisicas = do
    handle <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
    conteudo <- hGetContents handle
    let linhas = lines conteudo
        avaliacoes = map (splitOn ",") linhas
    if null linhas
        then putStrLn "\nNenhuma avaliação física encontrada."
        else imprimindoAvaliacaoFisica avaliacoes
    hClose handle

-- Função para atualizar os dados de uma avaliação física no arquivo "avaliacoes_fisicas.txt" com base no ID fornecido.
atualizarAvaliacaoFisicaPorId :: Int -> AvaliacaoFisica -> IO ()
atualizarAvaliacaoFisicaPorId targetId novaAvaliacao = do
    handle <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
    contents <- hGetContents handle
    let linhas = lines contents
        ids = primeirosElementos linhas
    if not (verificandoId (show targetId) ids)
        then putStrLn "Avaliação física não encontrada."
        else do
            (tempName, tempHandle) <- openTempFile "." "temp"
            let linhasAtualizadas = map (\linha ->
                    if verificandoId (show targetId) (primeirosElementos [linha])
                        then atualizarDadosAvaliacaoFisica linha novaAvaliacao
                        else linha) linhas
            hPutStr tempHandle (unlines linhasAtualizadas)
            hClose handle
            hClose tempHandle
            removeFile "avaliacoes_fisicas.txt"
            renameFile tempName "avaliacoes_fisicas.txt"

-- Função para atualizar os dados de uma avaliação física em uma linha específica
atualizarDadosAvaliacaoFisica :: String -> AvaliacaoFisica -> String
atualizarDadosAvaliacaoFisica linha (AvaliacaoFisica id dataAvaliacao novoPeso novaAltura novaIdade novoObjetivo matriculaAlunoAv) =
    let dadosAntigos = splitOn "," linha
        idAntigo = dadosAntigos !! 0
        dataAntiga = dadosAntigos !! 1
        pesoAntigo = dadosAntigos !! 2
        alturaAntiga = dadosAntigos !! 3
        idadeAntiga = dadosAntigos !! 4
        objetivoAntigo = dadosAntigos !! 5
        novosDados = [idAntigo,
                      if null dataAvaliacao then dataAntiga else dataAvaliacao,
                      if novoPeso == 0 then pesoAntigo else show novoPeso,
                      if novaAltura == 0 then alturaAntiga else show novaAltura,
                      if novaIdade == 0 then idadeAntiga else show novaIdade,
                      if null novoObjetivo then objetivoAntigo else novoObjetivo]
    in intercalate "," novosDados


-- Função para remover uma avaliação física do arquivo pelo ID.
removerAvaliacaoFisicaPorId :: Int -> IO ()
removerAvaliacaoFisicaPorId targetId = do
    handle <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
    contents <- hGetContents handle
    let linhas = lines contents
        ids = primeirosElementos linhas
    if not (verificandoId (show targetId) ids)
        then putStrLn "Avaliação física não encontrada."
        else do
            (tempName, tempHandle) <- openTempFile "." "temp"
            let linhasFiltradas = filter (\linha -> not $ verificandoId (show targetId) (primeirosElementos [linha])) linhas
            hPutStr tempHandle (unlines linhasFiltradas)
            hClose handle
            hClose tempHandle
            removeFile "haskell/avaliacoes_fisicas.txt"
            renameFile tempName "haskell/avaliacoes_fisicas.txt"
            putStrLn "Avaliação física removida com sucesso."

-- Função para imprimir os dados de uma avaliação física representada por uma lista de listas de strings.
imprimindoAvaliacaoFisica :: [[String]] -> IO ()
imprimindoAvaliacaoFisica [] = return ()
imprimindoAvaliacaoFisica (x:xs) = do
    if length x >= 1 then
        putStrLn $ "\nID da avaliação: " ++  (x !! 0) ++
                  "\nData da avaliação: " ++  (x !! 1) ++
                  "\nPeso: " ++ (x !! 2) ++
                  "\nAltura: " ++  (x !! 3) ++
                  "\nIdade: " ++(x !! 4) ++
                  "\nObjetivo: " ++ (x !! 5) ++
                  "\nMatricula Aluno: "++ (x!!6)
    else
        putStrLn "A lista não contém dados suficientes para uma avaliação física."
    imprimindoAvaliacaoFisica xs

-- Função para adicionar uma avaliação física ao arquivo
adicionarAvaliacaoFisica :: AvaliacaoFisica -> IO ()
adicionarAvaliacaoFisica nova_avaliacao = do
    conexao <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
    conteudo <- hGetContents conexao
    let linhas = lines conteudo
        ids = primeirosElementos linhas
        idNovo = avaliacaoId nova_avaliacao
    if verificandoId (show idNovo) ids
      then putStrLn "ID já em uso. Escolha um ID diferente."
      else appendFile "haskell/avaliacoes_fisicas.txt" (toStringAvaliacaoFisica nova_avaliacao ++ "\n")
    hClose conexao

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

-- Função auxiliar para converter uma lista de strings em uma AvaliacaoFisica
parseAvaliacaoFisica :: String -> Maybe AvaliacaoFisica
parseAvaliacaoFisica linha = case words linha of
    [avaliacaoId, dataAvaliacao, peso, altura, idade, objetivo, matriculaAlunoAv] ->
        Just (AvaliacaoFisica (read avaliacaoId) dataAvaliacao (read peso) (read altura) (read idade) objetivo matriculaAlunoAv)
    _ -> Nothing

toStringAvaliacaoFisica :: AvaliacaoFisica -> String
toStringAvaliacaoFisica avaliacao =
    intercalate "," [show (avaliacaoId avaliacao), dataAvaliacao avaliacao, show (peso avaliacao), show (altura avaliacao), show (idade avaliacao), objetivo avaliacao, matriculaAlunoAv avaliacao]

verificarIMC :: Int -> IO ()
verificarIMC alunoId = do
    handle <- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
    contents <- hGetContents handle
    let linhas = lines contents
        ids = primeirosElementos linhas
    if not (verificandoId (show alunoId) ids)
        then putStrLn "ID do aluno não encontrado."
        else do
            let avaliacaoAluno = filtrarId alunoId linhas
            if null avaliacaoAluno
                then putStrLn "Avaliação física do aluno não encontrada."
                else do
                    let [_, _, pesoStr, alturaStr, _, _] = head avaliacaoAluno
                        pesoAluno = read pesoStr :: Float
                        alturaAluno = read alturaStr :: Float
                        imcAluno = calcularIMC pesoAluno alturaAluno
                        faixaIMC = calcularFaixaIMC imcAluno
                        recomendacao = obterRecomendacaoIMC faixaIMC
                    putStrLn $ "IMC do aluno: " ++ show imcAluno ++ " - " ++ faixaIMC
                    putStrLn $ "Recomendação: " ++ recomendacao
    hClose handle

obterRecomendacaoIMC :: String -> String
obterRecomendacaoIMC faixaIMC
    | faixaIMC == "Baixo peso Grau III" = "Recomenda-se procurar um médico imediatamente."
    | faixaIMC == "Baixo peso Grau II" = "Recomenda-se aumentar a ingestão calórica e buscar orientação médica."
    | faixaIMC == "Baixo peso Grau I" = "Recomenda-se aumentar a ingestão calórica e praticar exercícios físicos regularmente."
    | faixaIMC == "Peso normal" = "Parabéns! Seu peso está dentro da faixa saudável. Continue mantendo hábitos saudáveis."
    | faixaIMC == "Sobrepeso" = "Recomenda-se controlar a dieta e aumentar a prática de exercícios físicos."
    | faixaIMC == "Obesidade Grau I" = "Recomenda-se uma dieta balanceada e acompanhamento médico regular."
    | faixaIMC == "Obesidade Grau II" = "Recomenda-se buscar orientação médica para iniciar um programa de perda de peso."
    | otherwise = "Recomenda-se procurar um médico para avaliação detalhada."



-- Função para calcular o IMC
calcularIMC :: Float -> Float -> Float
calcularIMC peso altura
    | altura <= 0 = 0
    | otherwise = peso / (altura * altura)

-- Função para determinar a faixa de IMC
calcularFaixaIMC :: Float -> String
calcularFaixaIMC imc
    | imc < 16.0 = "Baixo peso Grau III"
    | imc < 16.9 = "Baixo peso Grau II"
    | imc < 18.4 = "Baixo peso Grau I"
    | imc < 24.9 = "Peso normal"
    | imc < 29.9 = "Sobrepeso"
    | imc < 34.9 = "Obesidade Grau I"
    | imc < 39.9 = "Obesidade Grau II"
    | otherwise = "Obesidade Grau III"



--TREINO

--Função para cadastrar TREINO (primeiro paramêtro por enquanto é a matricula)
cadastraTreino :: String -> [String]  -> IO Treino
cadastraTreino tipo_treino exercicios= do
  let novoTreino = Treino tipo_treino exercicios
  appendFile "haskell//treino.txt" (show novoTreino ++ "\n")
  return novoTreino

associarTreinoAluno:: String-> Treino -> IO()
associarTreinoAluno matricula treino= do
    alunoEncontrado <- recuperaAlunoMatricula matricula
    let novoAluno = alunoEncontrado { treinos = treino : treinos alunoEncontrado }
    substituirAlunoTxt novoAluno matricula

toArray :: String -> [String]
toArray exercicios = splitOn "/" exercicios
