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



