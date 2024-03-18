module Maquina where

import DB
import System.Directory
import System.Environment
import System.IO
import Data.List (intercalate)
import Data.Char (isDigit)

type Codigo = String
type Nome = String 
type DataManutencao = Int

data Maquina = Maquina{
    codigoMaquina :: Codigo,
    nomeMaquina :: Nome,
    dataManutencao :: DataManutencao
} deriving (Show)

--- Função data de manutencao delimitar em 00/00/0000
delimitarManutencao :: String -> Maybe String
delimitarManutencao manutencao
    | length numeros == 8 = Just dataFormatada
    | otherwise = Nothing
    where
        numeros = filter isDigit manutencao
        dia = take 2 numeros
        mes = take 2(drop 2 numeros)
        ano = drop 4 numeros
        dataFormatada = intercalate "/"[dia,mes,ano]