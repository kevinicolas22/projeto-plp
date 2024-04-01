module LoginService where

import Login
import System.IO
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (elemIndices, intercalate, elemIndex)


--Funçãp para verificar se cadastro já existe
existeCadastro :: String ->String-> IO Bool
existeCadastro cpf conteudo= do
    let linhas = lines conteudo
        cpfs = primeirosElementos linhas
    seq cpfs $ return()
    let existe =cpf `elem` cpfs
   
    return existe

--Função para verificar se o tipo de usuário e a senha estão corretos, comparando com os passados no input
cadastroCondizComTipoESenha :: String -> Int -> String ->IO Bool
cadastroCondizComTipoESenha cpf tipoUsuario senha = do
    conexao<- openFile "haskell/login.txt" ReadMode
    conteudo<- hGetContents conexao
    seq conteudo $ return()
    let linhas = lines conteudo
        cpfs = primeirosElementos linhas
        posicao = cpf `elemIndices` cpfs
    if null posicao
        then do
            hClose conexao
            return False 
        else do
            let conjuntoDados = linhas !! head posicao
                listaP = splitOn "," conjuntoDados
                resultado = (listaP !! 2) == show tipoUsuario && (listaP !! 1) == senha
            seq resultado $ return()
            hClose conexao
            return resultado

--Função para retornar o tipo de menu a ser acessado
tipoMenu :: String ->IO String
tipoMenu cpf = do
    conexao<- openFile "haskell/login.txt" ReadMode
    conteudo<- hGetContents conexao
    seq conteudo $ return()
    let linhas = lines conteudo
        cpfs = primeirosElementos linhas
        --posicao = elemIndex cpf cpfs
        posicao = pegarPosicao cpf cpfs
        --posicao = cpf `elemIndices` cpfs
        conjuntoDados = linhas !! posicao
        listaP = splitOn "," conjuntoDados
    seq listaP $ return()
    hClose conexao    
    return (listaP !! 2)

--Função responsável por retornar as posição de ocorrência do cpf numa lista
pegarPosicao :: String -> [String] -> Int
pegarPosicao cpf cpfs = do
    case elemIndex cpf cpfs of
        Just x -> x
        Nothing -> -1

--Função para validar as entradas para um formato de cpf 
delimitarCpf :: String -> String
delimitarCpf cpfA
    | length numeros == 11  = cpfFormatado
    where
        numeros = filter isDigit cpfA
        cpfFormatado = intercalate "."[chunk 0 3, chunk 3 6, chunk 6 9] ++ "-" ++ take 2(drop 9 numeros)
        chunk start end = take(end - start)(drop start numeros)

--Função que retorna os primeiros elementos de uma lista, no caso a ser usado vai retornar os CPF's cadastrados
primeirosElementos :: [String] -> [String]
primeirosElementos linhas = map (\linha -> head (words (replace ',' ' ' linha))) linhas
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c : cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs