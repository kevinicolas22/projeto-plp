{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where


import Data.Char (toUpper)
import MainAluno
import MainFuncionario as MF
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
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite uma opção:                                 ║" 
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    
    opcao <- getLine
    let opcaoUpper = map toUpper opcao
    case opcaoUpper of
        "A" -> loginAluno
        --"B" -> --menuGestor
        "C" -> MF.main
        --"D" -> menuLogin
       

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