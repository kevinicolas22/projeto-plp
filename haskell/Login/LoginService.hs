module LoginService where

import Login
import Data.IORef
import Control.Monad.IO.Class (liftIO)

--Define um tipo de dados para representar o contador de IDs
newtype IDCounter = IDCounter { unIDCounter :: IORef Int }

-- Função para criar um novo contador de IDs
newIDCounter :: IO IDCounter
newIDCounter = IDCounter <$> newIORef 0

-- Função para gerar um novo ID incremental
generateID :: IDCounter -> IO Int
generateID (IDCounter ref) = atomicModifyIORef' ref (\n -> (n + 1, n + 1))

--Função para cadastrar login no login.txt e retornar sua matricula
cadastraLogin :: String -> Int -> Int -> String -> Int
cadastraLogin tipoUsuario senha confirmacaoSenha palavraChave = do
    conexao <- openFile "login.txt" ReadMode
    conteudo <- hGetContents conexao
    