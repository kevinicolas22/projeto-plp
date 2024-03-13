module Manager where

import TypeClasses

type ManagerCpf = Int 
type Ssn = String
type Name = String
type Birth = String
type Email = String
type Telephone = Int
type Address = String

data Manager = Manager {
    cpf :: ManagerCpf,
    ssn :: Ssn,
    name :: Name,
    birth :: Birth,
    telephone :: Telephone,
    address :: Address
}

instance Person Manager where
    personSSN manager = Manager.ssn manager

instance Entity Manager where
    entityCpf manager = Manager.cpf manager

instance Show Manager where
    show(Meneger cpf name birth telephone address) = "\n¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨\n" ++
                                                            "CPF: " ++ (show cpf) ++ "\n" ++
                                                            "Nome: " ++ name ++ "\n" ++
                                                            "Aniversário: " ++ (show birth) ++ "\n" ++
                                                            "Telefone: " ++ (show telephone) ++
                                                            "Endereço: " ++ address ++
                                                      "\n¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨\n"

instance Stringfy Manager where
    toString(Meneger cpf ssn name birth telephone address) = show cpf ++ "," ++
                                                                ssn ++ "," ++
                                                                name ++ "," ++
                                                                show birth ++ "," ++
                                                                show telephone ++ "," ++
                                                                address 

-- divide a string de entrada por , usando o split/
-- converte os campos para os tipos corretos e cria o valor manager
-- retorno é uma tupla com os dados
instance Read Meneger where                                                                
     readsPrec _ str = do
    let l = splitOn "," str
    let cpf = read (l !! 0) :: ManagerCpf
    let ssn = l !! 1
    let name = l !! 2
    let birth = l !! 3
    let telephone = read (l !! 4) :: Telephone
    let address = l !! 5
    [(Manager cpf ssn name birth telephone address, "")]



