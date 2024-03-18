module Maquina where

import DB
import System.Directory
import System.Environment
import System.IO

type Codigo = String
type Nome = String 
type DataManutencao = Int

data Maquina = Maquina{
    codigoMaquina :: Codigo,
    nomeMaquina :: Nome,
    dataManutencao :: DataManutencao
} deriving (Show)

