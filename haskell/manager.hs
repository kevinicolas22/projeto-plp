module Manager where

import TypeClasses
import System.IO
import Data.List.Split

type ManagerId = Int
type Cpf = Int 
type Name = String
type Birth = String
type Email = String
type Telephone = Int
type Address = String

data Manager = Manager {
    managerId :: ManagerId,
    cpf :: Cpf,
    name :: Name,
    birth :: Birth,
    telephone :: Telephone,
    address :: Address
} deriving (Show)

instance Person Customer where
  personCpf manager = cpf manager

instance Entity Manager where
    entityId manager = managerId manager

instance Stringfy Manager where
    toString manager = show (managerId manager) ++ "," ++
                       show (cpf manager) ++ "," ++
                       name manager ++ "," ++
                       birth manager ++ "," ++
                       show (telephone manager) ++ "," ++
                       address manager

-- divide a string de entrada por , usando o split/
-- converte os campos para os tipos corretos e cria o valor manager
-- retorno é uma tupla com os dados
instance Read Manager where
    readsPrec _ str = do
        let l = splitOn "," str
        let id = read (l !! 0) :: ManagerId
        let cpf = read (l !! 1) :: Cpf
        let name = l !! 2
        let birth = l !! 3
        let telephone = read (l !! 4) :: Telephone
        let address = l !! 5
        [(Manager id cpf name birth telephone address, "")]

-- criar gestor
criarGestor :: IO gestor
criarGestor = do
    putStrLn "Digite o seu ID de gestor: "
    id <- getLine
    assunto <- readFile "gestor.txt"  -- Assumindo que as informações estão em um arquivo chamado gestor.txt
    let linhas = lines assunto
        gestores = mapMaybe (parseFuncionario . words) linhas
        ids = map managerId gestores
    if read id `elem` ids
        then do
            putStrLn "ID já em uso. Escolha um ID diferente."
            criarGestor
        else do
            putStrLn "Digite o seu CPF: "
            cpf <- getLine
            -- adicionar propriedade

            putStrLn "Digite o seu nome: "
            nome <- getLine

            putStrLn "Digite sua data de nascimento: "
            nascimento <- getLine
            -- adicionar propriedade

            putStrLn "Digite seu telefone: "
            telefone <- getLine
            -- adicionar propriedade

            putStrLn "Digite seu endereço: "
            endereco <- getLine

            return (Manager (read id) (read cpf) nome nascimento (read telefone) endereco)



