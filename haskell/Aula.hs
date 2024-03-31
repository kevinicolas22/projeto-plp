module Aula where
import Planos
import Data.List (intercalate)
import Data.Text (Text)
import Data.Char (isDigit)

type NomeA = String

type Horario = String

type Planos= [PlanoTipo]

data Aula = Aula {
    nomeAula :: NomeA,
    horarioAula :: Horario,
    planosPermitidos :: Planos
} 


showAula :: Aula-> String
showAula aula="=> " ++ nomeAula aula ++ "\n" ++
                "   Horário: " ++ horarioAula aula ++ "\n" ++
                "   Válida para os planos: " ++ intercalate ", " (map show (planosPermitidos aula))
showAulaTxt:: Aula-> String
showAulaTxt aula= nomeAula aula ++ ";" ++ horarioAula aula ++ ";" ++ showPlanos (planosPermitidos aula)
    where showPlanos planos = "[" ++ intercalate ", " (map (\p -> "\"" ++ show p ++ "\"") planos) ++ "]"


instance Show Aula where
    show aula = "("++ show (nomeAula aula)++","++ show (horarioAula aula)++","++ showPlanos(planosPermitidos aula)++")"
        where showPlanos planos = "[" ++ intercalate ", " (map (\p -> "\"" ++ show p ++ "\"") planos) ++ "]"

instance Read Aula where
    readsPrec _ str = [(Aula nomeAula "" [], "")]
        where nomeAula = takeWhile (/= ',') str

