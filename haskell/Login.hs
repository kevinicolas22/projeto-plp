module Login where

type Cpf = String

type Senha = String

type TipoUsuario = Int

--Definição de um login

data Login = Login
    {
        cpf :: Cpf,
        senha :: Senha,
        tipoUsuario :: TipoUsuario
    }deriving (Show)

class Stringify a where
    toString :: a -> String
    
--Função para usar como padrão para salvar nos arquivos  
instance Stringify Login where
    toString (Login cpf senha tipoUsuario) =  cpf ++ "," ++
                                                senha  ++ "," ++
                                                show tipoUsuario