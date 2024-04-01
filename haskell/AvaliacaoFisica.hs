module AvaliacaoFisica where

type Data = String
type Id = Int

data AvaliacaoFisica = AvaliacaoFisica {
    avaliacaoId :: Id,
    dataAvaliacao :: Data,
    peso :: Float,
    altura :: Float,
    idade :: Int,
    objetivo :: String,
    matriculaAlunoAv:: String
} deriving (Show)