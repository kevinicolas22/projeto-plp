module Manager where

import DB
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO

type ManagerId = Int
type Cpf = String 
type Name = String
type Birth = String
type Email = String
type Telephone = String
type Address = String

data Manager = Manager {
    managerId :: ManagerId,
    cpf :: Cpf,
    name :: Name,
    birth :: Birth,
    telephone :: Telephone,
    address :: Address
} deriving (Show)




