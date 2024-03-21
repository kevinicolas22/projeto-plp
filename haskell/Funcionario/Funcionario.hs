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

type Salario = Float

-- Definição de um funcionário
data Funcionario = Funcionario
  { funcId :: Id,
    nome :: Nome,
    cpf :: Cpf,
    endereco :: Endereco,
    telefone :: Telefone,
    data_ingresso :: Data_Ingresso,
    salario :: Salario
  }
  deriving (Show)
