module Funcionario where

type Id = Int
type Nome = String
type Cpf = String
type Endereco = String
type Telefone = String
type Data_Ingresso = String
type Data_Saida = String

data Funcionario = Funcionario {
    , id :: Id
    , nome :: Nome
    , cpf :: Cpf
    , endereco :: Endereco
    , telefone :: Telefone
    , data_ingresso :: Data_Ingresso
    , data_saida :: Data_Sainda

}

instance Show Funcionario where
    show(Funcionario id nome cpf endereco telefone data_ingresso data_saida) = "\n--------------------\n" ++ 
                                                                                "ID: " ++ show id ++ "\n" ++
                                                                                "Nome: " ++ nome ++ "\n" ++ 
                                                                                "CPF: " ++ cpf ++ "\n" ++
                                                                                "Endereço: " ++ endereco ++ "\n" ++
                                                                                "Telefone: " ++ telefone ++ "\n" ++
                                                                                "Data Ingresso: " ++ data_ingresso + "\n" ++
                                                                                "Data Saida: " ++ data_saida + "\n" ++
                                                                                "\n_____________________\n"

instance Stringfy Funcionario where
  toString (Funcionario id nome cpf endereco telefone data_ingresso data_saida) = show id ++ "," ++
                                              nome ++ "," ++
                                              cpf ++ "," ++
                                              endereco ++ "," ++
                                              telefone ++ "," ++
                                              data_ingresso ++ "," ++ 
                                              data_saida
