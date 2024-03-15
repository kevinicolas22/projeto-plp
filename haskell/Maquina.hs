import DB
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory
import System.Environment
import System.IO

type Codigo = Int
type Nome = String 
type DataManutencao = Int

data Maquina = Maquina{
    codigoMaquina :: Codigo
    nomeMaquina :: Nome
    dataManutencao :: DataManutencao
} deriving (Show)

