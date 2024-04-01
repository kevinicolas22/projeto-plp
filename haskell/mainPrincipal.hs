{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Login
import LoginService
import MainAluno
import MainGestor
import MainFuncionario as MF
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.Exit (exitSuccess)
import Data.Char (toUpper)
import System.IO
import AlunoController
--Interface sistema
loginMembro:: Int->IO()
loginMembro tipoFuncionarioValidado = do
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"   
    putStrLn "══════════════════════════LOGIN══════════════════════════"   
    putStrLn "\n> Digite o seu cpf ('!' para sair): "
    cpf <- getLine
    if cpf=="!"
        then do
            main
        else return()
    cpfCorreto cpf tipoFuncionarioValidado
    
    let cpfDelimitado = delimitarCpf cpf
    putStrLn "> Digite sua senha: "
    senha <- getLine
    senhaCorreta senha tipoFuncionarioValidado

    putStrLn "\n Carregando..."
    threadDelay (2 * 1000000)
    conexao<- openFile "haskell/login.txt" ReadMode
    conteudo<- hGetContents conexao
    existeCadastroR <- existeCadastro cpfDelimitado conteudo
    if existeCadastroR
        then do
            hClose conexao
            tipoEsenhaCorreto <- cadastroCondizComTipoESenha cpfDelimitado tipoFuncionarioValidado senha
            if tipoEsenhaCorreto
                then do
                    tipo <- tipoMenu cpfDelimitado
                    case (tipo) of
                        "2" -> menuGestor main
                        "3" -> menuFuncionario main
                else do
                    putStrLn "Senha inválida ou tipo de usuario inválido, tente novamente!"
                    threadDelay (2 * 1000000)
                    loginMembro tipoFuncionarioValidado
        else do
            hClose conexao
            putStrLn " > Cadastro não encontrado !"
            threadDelay (2 * 1000000)
            loginMembro tipoFuncionarioValidado
main :: IO()
main = do 
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    putStrLn "|\n|   1. ALUNO"
    putStrLn "|   2. GESTOR"
    putStrLn "|   3. FUNCIONÁRIO"
    putStr "|\n| > Tipo de usuario ('!' Para Sair):"
    hFlush stdout
    tipoFuncionario <- getLine
    if tipoFuncionario == "!"
        then do
            putStrLn "Encerrando o programa!"
            threadDelay (2 * 1000000)
            exitSuccess
        else return()
    let tipoFuncInt = read tipoFuncionario :: Int
    tipoFuncionarioValidado <- tipoUsuarioCorreto tipoFuncInt
    if tipoFuncionarioValidado == 1
        then do loginAluno main
    else return()
    loginMembro tipoFuncionarioValidado

cpfCorreto :: String ->Int-> IO ()
cpfCorreto cpf tipoFuncionarioValidado= do
    if not(length cpf==11)
        then do
            putStrLn "CPF não está no formato 000.000.000-00 ! "
            threadDelay (2 * 1000000)
            loginMembro tipoFuncionarioValidado
        else do
            putStrLn " "    

tipoUsuarioCorreto :: Int ->IO Int
tipoUsuarioCorreto x = if verificarIntTipoFuncionario x
                            then return x
                            else do
                                putStrLn "Opção inválida, tente novamente!"
                                novaOpcao <- readLn :: IO Int
                                tipoUsuarioCorreto novaOpcao

senhaCorreta :: String ->Int-> IO ()
senhaCorreta senha tipoFuncionarioValidado = do
    if length senha >= 4
        then do
            putStrLn " "
        else do
            putStrLn "Senha tem que possuir no mínimo 4 caracters, tente novamente!"
            threadDelay (2 * 1000000)
            loginMembro tipoFuncionarioValidado
            

padraoCorreto :: String -> IO String
padraoCorreto opcao = do
    if opcao == "C" || opcao == "E"
        then return opcao
        else do
            putStrLn "Opção Inválida, tente novamente!"
            novaOpcao <- getLine
            let opcaoUpper = map toUpper opcao
            padraoCorreto opcaoUpper

verificarIntTipoFuncionario :: Int -> Bool
verificarIntTipoFuncionario x = x >= 1 && x <= 3