module Aluno where

import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Aula
import Planos
import Treino
import System.Environment
import System.IO
import Planos
-- Definição dos Tipos de dados do aluno
type Id = Int
type Nomew = String
type Cpf = String
type Endereço = String
type Contato = String
type PlanoAluno = Planos.PlanoTipo
type Treinos = [Treino]
type EmDia = Bool
type Matricula= String
type Senha= String
type Email= String
type Aulas= [Aula]
-- Definição de um Aluno
data Aluno = Aluno
  { matricula:: Matricula,
    alunoId :: Id,
    nomeAluno :: Nomew,
    cpfAluno :: Cpf,
    endereçoAluno :: Endereço,
    contatoAluno :: Contato,
    planoAluno :: PlanoAluno,
    treinos :: Treinos,
    emDia :: EmDia,
    senhaAluno:: Senha,
    emailAluno:: Email,
    aulas:: Aulas,
    saldo:: Float
  }

-- Recupera apenas as matriculas dos alunos do aquivo Aluno.txt
primeirosElementos :: [String] -> [String]
primeirosElementos = map (getFirstElement . replace ';' ' ')
  where
    replace :: Char -> Char -> String -> String
    replace _ _ [] = []
    replace from to (c:cs)
      | c == from = to : replace from to cs
      | otherwise = c : replace from to cs
      
    getFirstElement :: String -> String
    getFirstElement line =
      case words line of
        [] -> "" -- Tratar caso de linha vazia
        (firstWord:_) -> firstWord

showAlunoLista:: Aluno -> String
showAlunoLista aluno= 
  "\n             ══════════════════════"++
  "\n             Nome: "++ nomeAluno aluno++
  "\n             Contato: "++ contatoAluno aluno++
  "\n             Matrícula: "++ matricula aluno
  
instance Show Aluno where
  show (Aluno matricula alunoId nomeAluno cpfAluno endereçoAluno contatoAluno plano treinos emDia senhaAluno emailAluno aulas saldo) =
    
    if emDia
      then 
        " CPF: " ++ cpfAluno
        ++ "\n Endereço: " ++  endereçoAluno
        ++ "\n Contato: " ++  contatoAluno
        ++ "\n Plano: " ++ show plano
        ++ "\n Mensalidade: "++ "\x1b[32mEm dia\x1b[0m"
        ++ "\n Matrícula: " ++  matricula
        ++ "\n Senha: " ++ senhaAluno
        ++ "\n Email: " ++ emailAluno
        ++ "\n Saldo Atual: R$ "++ show(saldo)
    else
      do
        " CPF: " ++ cpfAluno
        ++ "\n Endereço: " ++  endereçoAluno
        ++ "\n Contato: " ++  contatoAluno
        ++ "\n Plano: " ++ show plano
        ++ "\n Mensalidade: "++ "\x1b[31mPendente\x1b[0m"
        ++ "\n Matrícula: " ++  matricula
        ++ "\n Senha: " ++ senhaAluno
        ++ "\n Email: " ++ emailAluno
        ++ "\n SaldoAtual: R$ "++ show(saldo)
     
            


