{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where


import Data.Char (toUpper)
import MainAluno
import MainFuncionario as MF
import Control.Concurrent (threadDelay)
import System.Console.ANSI
--Interface sistema
main :: IO()
main = do
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "║                                                       ║"
    putStrLn "║   Escolha a forma de entrada:                         ║"
    putStrLn "║                                                       ║"
    putStrLn "║   [1] Aluno                                           ║"
    putStrLn "║   [2] Gestor                                          ║"
    putStrLn "║   [3] Funcionario                                     ║"
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite uma opção:                                 ║" 
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    
    opcao <- getLine
    let opcaoUpper = map toUpper opcao
    case opcaoUpper of
        "1" -> loginAluno main
        --"B" -> --menuGestor
        "3" -> menuFuncionario main
        --"D" -> menuLogin
        _ -> do 
            putStrLn "! Opção inválida !"
            threadDelay (2 * 1000000)
            main
       

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