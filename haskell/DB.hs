module DB where

import System.IO

-- Função para adicionar conteúdo a um arquivo
add :: FilePath -> String -> IO ()
add arquivo conteudo = appendFile arquivo (conteudo ++ "\n")
