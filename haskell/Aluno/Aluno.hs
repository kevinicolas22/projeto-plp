module Aluno where

import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)

import Planos

import System.Environment
import System.IO
import Planos
-- Definição dos Tipos de dados do aluno
type Id = Int
type Nome = String
type Cpf = String
type Endereço = String
type Contato = String
type PlanoAluno = Planos.PlanoTipo
type Treinos = [String]
type EmDia = Bool
type Matricula= String
type Senha= String
type Email= String

-- Definição de um Aluno
data Aluno = Aluno
  { alunoId :: Id,
    nome :: Nome,
    cpf :: Cpf,
    endereço :: Endereço,
    contato :: Contato,
    planoAluno :: PlanoAluno,
    treinos :: Treinos,
    emDia :: EmDia,
    matricula:: Matricula,
    senha:: Senha,
    email:: Email
  }

 

primeirosElementos :: [String] -> [String]
primeirosElementos = map (head . words . replace ',' ' ')
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c : cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs


instance Show Aluno where
  show (Aluno alunoId nome cpf endereco contato plano treinos emDia matricula senha email) =
    
    if emDia
      then 
        " CPF: " ++ show cpf
        ++ "\n Endereço: " ++  endereco
        ++ "\n Contato: " ++  contato
        ++ "\n Plano: " ++ show plano
        ++ "\n Treinos: " ++ show treinos
        ++ "\n Mensalidade: "++ "\x1b[32mEm dia\x1b[0m"
        ++ "\n Matrícula: " ++  matricula
        ++ "\n Senha: " ++ senha
        ++ "\n Email: " ++ email
    else
      do
        " CPF: " ++ show cpf
        ++ "\n Endereço: " ++  endereco
        ++ "\n Contato: " ++  contato
        ++ "\n Plano: " ++ show plano
        ++ "\n Treinos: " ++ show treinos
        ++ "\n Mensalidade: "++ "\x1b[31mPendente\x1b[0m"
        ++ "\n Matrícula: " ++  matricula
        ++ "\n Senha: " ++ senha
        ++ "\n Email: " ++ email
     
            


