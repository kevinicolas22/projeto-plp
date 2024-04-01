module Treino where
import Data.List.Split(splitOn)
import Data.Aeson
type TipoTreino = String

type Exercicios = [String]


--Definição de um treino
data Treino = Treino
    {   
        tipoTreino :: TipoTreino,
        exercicios :: Exercicios
    }

class Stringify a where
    toString :: a -> String
    
showTreino:: Treino->String
showTreino treino = "\x1b[32m" ++"=> TIPO DE TREINO: \x1b[0m" ++ tipoTreino treino++  "\n" ++"   EXERCÍCIOS: " ++ alignExercicios (exercicios treino)


instance Show Treino where
    show treino = "("++show(tipoTreino treino)++","++ show (exercicios treino)++")"

alignExercicios :: [String] -> String
alignExercicios [] = ""
alignExercicios (x:xs) = x ++ "\n" ++ unlines (map ("               " ++) xs)

instance Read Treino where
    readsPrec _ str = [(Treino tipoTreino exercicios, "")]
     where
        partes = splitOn "," str
        tipoTreino = partes !! 0
        exercicios = read (partes !! 1) :: Exercicios

        

