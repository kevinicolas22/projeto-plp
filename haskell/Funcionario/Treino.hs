module Treino where

type TipoTreino = String

type Matricula = Int

type Descricao = String

type DataTreino = String

--Definição de um treino
data Treino = Treino
    {   
        matricula :: Matricula,
        tipoTreino :: TipoTreino,
        descricao :: Descricao,
        dataTreino :: DataTreino
    }

class Stringify a where
    toString :: a -> String
    
instance Stringify Treino where
    toString (Treino matricula tipoTreino descricao dataTreino) =  show matricula ++ "," ++
                                                        tipoTreino ++ "," ++
                                                        descricao ++ "," ++
                                                        dataTreino
instance Show Treino where
    show (Treino matricula tipoTreino descricao dataTreino) = "Matricula: " ++ show matricula ++
                                                            "Tipo Treino: " ++ tipoTreino ++ 
                                                            "Descricao: " ++ descricao ++
                                                            "Data Treino: " ++ dataTreino