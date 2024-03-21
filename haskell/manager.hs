{-# LANGUAGE PackageImports #-}
module Gestor where

import System.Directory
import DB
import System.Environment
import System.IO
import Data.List.Split
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (chunksOf)


type Id = Int
type Cpf = String 
type Nome = String
type DataNascimento = String
type Email = String
type Telefone = String
type Endereco = String

data Manager = Manager {
    managerId :: Id,
    cpf :: Cpf,
    nome :: Nome,
    dataNascimento :: DataNascimento,
    telefone :: Telefone,
    endereco :: Endereco
} deriving (Show)

--- Funçao para delimitar o telefone
delimitarTelefone :: String -> Maybe String
delimitarTelefone telefone
    | length numeros == 11 = Just telefone
    | otherwise = Nothing
    where
        numeros = filter isDigit telefone

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

--- Função delimitar CPF 11 numeros 000.000.000-00
delimitarCpf :: String -> Maybe String
delimitarCpf cpf
    | length numeros == 11 = Just cpfFormatado
    | otherwise = Nothing
    where
        numeros = filter isDigit cpf
        cpfFormatado = intercalate "."[chunk 0 3, chunk 3 6, chunk 6 9] ++ "-" ++ take 2(drop 9 numeros)
        chunk start end = take(end - start)(drop start numeros)