module Planos where
import Data.Foldable

import System.Environment
import System.IO
import Data.List (intercalate)
import Data.Maybe ( mapMaybe, maybeToList )

type ValorMensal= Float
type ValorSemestre = Float
type ValorAnual = Float
type HoraEntradaMaxima= Int
type HoraEntradaMinima= Int

data PlanoTipo = Light | Gold | Premium
    deriving (Show, Eq, Ord, Enum)

data Plano = Plano {
    tipo:: PlanoTipo,
    valorMensal:: ValorMensal,
    valorSemestre:: ValorSemestre,
    valorAnual:: ValorAnual,
    horaEntradaMaxima:: HoraEntradaMaxima,
    horaEntradaMinima:: HoraEntradaMinima 
}
    

-- Valores predefinidos para cada tipo de plano
planoLight :: Plano
planoLight = Plano {
    tipo = Light,
    valorMensal = 50.0,
    valorSemestre = 280.0,
    valorAnual = 480.0,
    horaEntradaMaxima = 19,
    horaEntradaMinima = 10
}

instance Show Plano where
    show plano = show (tipo plano)

planoGold :: Plano
planoGold = Plano {
    tipo = Gold,
    valorMensal = 70.0,
    valorSemestre = 330.0,
    valorAnual = 600.0,
    horaEntradaMaxima = 20,
    horaEntradaMinima = 8
}


planoPremium :: Plano
planoPremium = Plano {
    tipo = Premium,
    valorMensal = 100.0,
    valorSemestre = 510.0,
    valorAnual = 960.0,
    horaEntradaMaxima = 23,
    horaEntradaMinima = 5
} 

instance Read PlanoTipo where
  readsPrec _ str = case str of
    "Light" -> [(Light, "")]
    "Gold"  -> [(Gold, "")]
    "Premium" -> [(Premium, "")]
    _       -> []


showPlanos :: [PlanoTipo] -> String
showPlanos planos = "[" ++ intercalate ", " (map (\p -> "\"" ++ show p ++ "\"") planos) ++ "]"

detalhesPlano :: PlanoTipo -> String
detalhesPlano Light = detalhesPlano'' planoLight
detalhesPlano Gold = detalhesPlano'' planoGold
detalhesPlano Premium = detalhesPlano'' planoPremium

detalhesPlano'' :: Plano -> String
detalhesPlano'' plano = "    ======= " ++ show (tipo plano) ++ " =======" ++
                        "\nHorario Maximo de entrada: " ++ show (horaEntradaMaxima plano) ++ " hrs"++
                        "\nHorario Minimo de entrada: " ++ show (horaEntradaMinima plano) ++ " hrs"++
                        "\nValor mensal: R$" ++ show (valorMensal plano)