module AulaService where

import Planos
import Aula
import MainAluno
import Control.Concurrent (threadDelay)

--Aulas
--Função para criar uma aula
adcionaAula :: String ->  String -> [PlanoTipo] -> IO()
adcionaAula nomeAula horarioAula planosPermitidos = do
    let novaAula = Aula {
        nomeAula = nomeAula,
        horarioAula = horarioAula,
        planosPermitidos = planosPermitidos
    }
    appendFile "haskell/aulas.txt" (nomeAula ++";"++ horarioAula ++ ";" ++ (showPlanos planosPermitidos) ++ "\n")

viewAulas :: IO ()
viewAulas = do
    conteudo <- readFile "haskell/aulas.txt"
    let aulas= recuperarAulas conteudo
    exibeAulas aulas
    threadDelay (3 * 1000000)

-- Função para ler uma aula de uma lista de strings
readAula :: String -> Aula
readAula str =
    let (nome, horario, planosStr) = read str
    in Aula nome horario (map read planosStr)