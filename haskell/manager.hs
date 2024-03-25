{-# LANGUAGE PackageImports #-}
module Manager where

import System.Directory
import DB
import System.Environment
import System.IO
import Data.List.Split
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (chunksOf)


type IdG = Int
type CpfG = String 
type NomeG = String
type DataNascimento = String
type Email = String
type TelefoneG = String
type EnderecoG = String

data Manager = Manager {
    managerId :: IdG,
    cpfG :: CpfG,
    nomeG :: NomeG,
    dataNascimento :: DataNascimento,
    telefoneG :: TelefoneG,
    enderecoG :: EnderecoG
} deriving (Show)

--- Funçao para delimitar o telefoneG
delimitarTelefoneG :: String -> Maybe String
delimitarTelefoneG telefoneG
    | length numeros == 11 = Just telefoneG
    | otherwise = Nothing
    where
        numeros = filter isDigit telefoneG

--- Função data de nascimento delimitar em 00/00/0000
delimitarNascimento :: String -> Maybe String
delimitarNascimento nascimento
    | length numeros == 8 = Just dataFormatada
    | otherwise = Nothing
    where
        numeros = filter isDigit nascimento
        dia = take 2 numeros
        mes = take 2(drop 2 numeros)
        ano = drop 4 numeros
        dataFormatada = intercalate "/"[dia,mes,ano]

--- Função delimitar cpfG 11 numeros 000.000.000-00
delimitarCpfG :: String -> Maybe String
delimitarCpfG cpfG
    | length numeros == 11 = Just cpfGFormatado
    | otherwise = Nothing
    where
        numeros = filter isDigit cpfG
        cpfGFormatado = intercalate "."[chunk 0 3, chunk 3 6, chunk 6 9] ++ "-" ++ take 2(drop 9 numeros)
        chunk start end = take(end - start)(drop start numeros)