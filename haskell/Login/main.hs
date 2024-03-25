module Main where

import Login
import LoginService
import Data.Char (toUpper)
import Aluno.MenuAluno

--Interface sistema
main :: IO()
main = do
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "║                                                       ║"
    putStrLn "║   Escolha a forma de entrada:                         ║"
    putStrLn "║                                                       ║"
    putStrLn "║   a. Aluno                                            ║"
    putStrLn "║   b. Gestor                                           ║"
    putStrLn "║   c. Funcionario                                      ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    putStrLn "Digite uma opção: "
    opcao <- getLine
    let opcaoUpper = map toUpper opcao
    case opcaoUpper of
        "A" -> menuAluno
        --"B" -> --menuGestor
        --"C" -> --menuFuncionario
        --"D" -> menuLogin
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main 

--Função para criar um login
{-criarLogin :: IO()
criarLogin = do
    putStrLn "Digite o tipo de usuario (ALUNO, GESTOR, FUNCIONARIO): "
    tipoUsuario <- getLine
    putStrLn "Digite sua nova senha: "
    senha <- readLn :: IO Int
    putStrLn "Para confirmar digite novamente sua senha: "
    confirmacaoSenha <- readLn :: IO Int
    putStrLn "Digite a palavra chave, em caso de recuperação de senha: "
    palavraChave <- getLine

    let matricula = cadastraLogin tipoUsuario senha confirmacaoSenha palavraChave
    -}