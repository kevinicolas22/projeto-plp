module Funcionario where

import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO
import Data.Char (isDigit)
import Data.List.Split (splitOn)

-- Definição de tipos de dados
type Id = Int

type Nome = String

type Cpf = String

type Endereco = String

type Telefone = String

type Data_Ingresso = String

-- Definição de um funcionário
data Funcionario = Funcionario
  { funcId :: Id,
    nome :: Nome,
    cpf :: Cpf,
    endereco :: Endereco,
    telefone :: Telefone,
    data_ingresso :: Data_Ingresso
  }
  deriving (Show)

--- Funçao para delimitar o telefone
delimitarTelefone :: String -> Maybe String
delimitarTelefone telefone
    | length numeros == 11 = Just telefone
    | otherwise = Nothing
    where
        numeros = filter isDigit telefone

--- Função data de nascimento delimitar em 00/00/0000
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