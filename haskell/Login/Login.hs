module Login where

type TipoUsuario = String

type Matricula = Int

type Senha = Int

type PalavraChave = String

--Definição de um login

data Login = Login
    {
        tipoUsuario :: TipoUsuario,
        matricula :: Matricula,
        senha :: Senha,
        palavraChave :: PalavraChave
    }deriving (Show)