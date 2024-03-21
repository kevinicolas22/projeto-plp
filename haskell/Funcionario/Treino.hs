module Treino where

type TipoTreino = String

type Matricula = Int

type Descricao = String

--Definição de um treino
data Treino = Treino
    {   
        matricula :: Matricula,
        tipoTreino :: TipoTreino,
        descricao :: Descricao
    }deriving (Show)

class Stringify a where
    toString :: a -> String
    
instance Stringify Treino where
    toString (Treino matricula tipoTreino descricao) =  show matricula ++ "," ++
                                                        tipoTreino ++ "," ++
                                                        descricao