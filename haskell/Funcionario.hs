module Funcionario where

import DB
import System.Environment

type Id = Int
type Nome = String
type Cpf = String
type Endereco = String
type Telefone = String
type Data_Ingresso = String
type Data_Saida = String

data Funcionario = Funcionario {
    id :: Id,
    nome :: Nome,
    cpf :: Cpf,
    endereco :: Endereco,
    telefone :: Telefone,
    data_ingresso :: Data_Ingresso
    
}

adcionarFuncionario::Funcionario -> IO()
adcionarFuncionario novo_funcionario = do
    appendFile "funcionario.txt" (toStringFuncionario novo_funcionario)

criarFuncionario :: IO Funcionario
criarFuncionario = do
    putStrLn "Digite o seu ID: "
    id <- getLine

    putStrLn "Digite o seu nome: "
    nome <- getLine

    putStrLn "Digite seu CPF: "
    cpf <- getLine

    putStrLn "Digite seu endereço: "
    endereco <- getLine

    putStrLn "Digite seu telefone: "
    telefone <- getLine

    putStrLn "Digite sua data de ingresso: "
    data_ingresso <- getLine

    return (Funcionario (read id) nome cpf endereco telefone data_ingresso)

instance Show Funcionario where
    show(Funcionario id nome cpf endereco telefone data_ingresso ) = "\n--------------------\n" ++ 
                                                                                "ID: " ++ show id ++ "\n" ++
                                                                                "Nome: " ++ nome ++ "\n" ++ 
                                                                                "CPF: " ++ cpf ++ "\n" ++
                                                                                "Endereço: " ++ endereco ++ "\n" ++
                                                                                "Telefone: " ++ telefone ++ "\n" ++
                                                                                "Data Ingresso: " ++ data_ingresso ++ "\n" ++
                                                                                "\n_____________________\n"


toStringFuncionario:: Funcionario -> String
toStringFuncionario (Funcionario id nome cpf endereco telefone data_ingresso) = show id ++ "," ++
                                              nome ++ "," ++
                                              cpf ++ "," ++
                                              endereco ++ "," ++
                                              telefone ++ "," ++
                                              data_ingresso 
                                              
