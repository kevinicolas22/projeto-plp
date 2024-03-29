module AulaService where

import Planos
import Aula
import MainAluno
import Control.Concurrent (threadDelay)
import Data.Char (toUpper)
import System.IO
import System.Directory (removeFile, renameFile)
import Data.List (elemIndices)


--Aulas
--Função para criar uma aula
adcionaAula :: String ->  String -> [PlanoTipo] -> IO()
adcionaAula nomeAula horarioAula planosPermitidos = do
    let novaAula = Aula {
        nomeAula = nomeAula,
        horarioAula = horarioAula,
        planosPermitidos = planosPermitidos
    }

    existe <- verificaAulaExiste nomeAula

    if null existe
        then do
            appendFile "haskell/aulas.txt" (nomeAula ++";"++ horarioAula ++ ";" ++ (showPlanos planosPermitidos) ++ "\n")
            putStrLn "Aula criada"
        else do
            putStrLn "Aula já existe com esse nome"
            threadDelay (2 * 1000000)

viewAulas :: IO ()
viewAulas = do
    conteudo <- readFile "haskell/aulas.txt"
    let aulas= recuperarAulas conteudo
    exibeAulas aulas
    

-- Função para deletar uma aula pelo nome
deletarAulaPeloNome :: String -> IO ()
deletarAulaPeloNome nome = do
    handle <- openFile "haskell/aulas.txt" ReadMode
    conteudo <- hGetContents handle
    let aulas = recuperarAulas conteudo
        nomes = nomesDasAulas aulas
        posicao = nome `elemIndices` nomes

    existe <- verificaAulaExiste nome

    if null existe
        then do
            putStrLn "Aula não existe"
            threadDelay (2 * 1000000)
        else do
            let novaListaAulas = filter (\aula -> nomeAula aula /= nome) aulas
            (tempName, tempHandle) <- openTempFile "." "temp"
            hPutStr tempHandle (aulasToString novaListaAulas)
            hClose handle
            hClose tempHandle
            removeFile "haskell/aulas.txt"
            renameFile tempName "haskell/aulas.txt"
            putStrLn "Aula Deletada"

aulasToString :: [Aula] -> String
aulasToString aulas = unlines $ map show aulas

nomesDasAulas :: [Aula] -> [String]
nomesDasAulas aulas = map nomeAula aulas

-- Função para ler uma aula de uma lista de strings
readAula :: String -> Aula
readAula str =
    let (nome, horario, planosStr) = read str
    in Aula nome horario (map read planosStr)

    
verificaAulaExiste :: String -> IO [Int]
verificaAulaExiste nome = do
    conteudo <- readFile "haskell/aulas.txt"
    let nomeUpper = map toUpper nome
        aulas = recuperarAulas conteudo
        nomes = nomesDasAulas aulas
        posicao = nomeUpper `elemIndices` nomes
    return (posicao)
    
        
    
pegarNomesDasAulas :: [Aula] -> [String]
pegarNomesDasAulas aulas = map nomeAula (aulas)

