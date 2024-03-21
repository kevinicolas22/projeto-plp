module AlunoController where

import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Control.Concurrent (threadDelay)
import System.Console.ANSI

import System.Environment
import System.IO
import Data.Char (isDigit)

import Aluno
import Planos

criarAluno :: IO Aluno
criarAluno = do
  hFlush stdout
  putStr "Nome do aluno: "
  hFlush stdout
  nomeAluno <- getLine
  putStr "\nCPF do aluno: "
  hFlush stdout
  cpfAluno <- getLine
  putStr "\nEmail do aluno: "
  hFlush stdout
  emailAluno <- getLine
  putStr "\nEndereço do aluno: "
  hFlush stdout
  endereçoAluno <- getLine
  putStr "\nContato do aluno: "
  hFlush stdout
  contatoAluno <- getLine
  putStrLn "\nEscolha o Plano: \n[A] LIGHT   [B] GOLD     [C] PREMIUM"
  hFlush stdout
  opção <- getLine
  let planoEscolhido = case opção of
        "a" -> Planos.Light
        "A" -> Planos.Light
        "b" -> Planos.Gold
        "B" -> Planos.Gold
        "c" -> Planos.Premium
        "C" -> Planos.Premium
        _ -> error "Opção inválida"
  putStr "\nMatricula para o aluno: "
  hFlush stdout
  matriculaAluno <- getLine
  if not(verificarString matriculaAluno)
    then do
      putStrLn("Formato inválido! ")
      criarAluno
      
  else do
    putStr "\nNova senha de acesso do aluno: "
    hFlush stdout
    senhaAluno <- getLine
    if not(verificarStringSenha senhaAluno)
    then do
      putStrLn("Formato inválido! ")
      criarAluno
    
    else do
      putStrLn "\nConfirmar Cadastro... Pressione ENTER"
      confirma <- getChar
      when (confirma /= '\n') $ void getChar -- Aguarda o Enter
      let alunoCriado = Aluno { alunoId = 0
                              , nome = nomeAluno
                              , cpf = cpfAluno
                              , endereço = endereçoAluno
                              , contato = contatoAluno
                              , planoAluno = planoEscolhido
                              , treinos = []
                              , emDia = False
                              , matricula= matriculaAluno
                              , senha= senhaAluno
                              , email = emailAluno
                              }
      return alunoCriado    


exibirAluno:: Aluno->String
exibirAluno aluno= show aluno

verificarString :: String -> Bool
verificarString str = length str >= 3 && all isDigit str

verificarStringSenha:: String-> Bool
verificarStringSenha str= length str>= 4