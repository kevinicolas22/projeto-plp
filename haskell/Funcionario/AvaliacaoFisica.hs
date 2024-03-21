module AvaliacaoFisica where

type Data = String
type Medida = Float

data ComposicaoCorporal = ComposicaoCorporal {
    percentualGordura :: Medida,
    massaMagra :: Medida,
    massaGorda :: Medida,
    imc :: Medida
} deriving (Show)

data Atributos = Atributos {
    peso :: Medida,
    altura :: Medida,
    idade :: Int,
    objetivo :: String
} deriving (Show)

data AvaliacaoFisica = AvaliacaoFisica {
    dataAvaliacao :: Data,
    atributos :: Atributos 
} deriving (Show)