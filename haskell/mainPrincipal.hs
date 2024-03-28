{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Login
import LoginService
import MainAluno
import MainFuncionario as MF
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.Exit (exitSuccess)
import Data.Char (toUpper)
import System.IO
--Interface sistema

main :: IO()
main = do 
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    putStrLn "\n > Tipo de usuario:"
    putStrLn "\n   1. ALUNO"
    putStrLn "   2. GESTOR"
    putStrLn "   3. FUNCIONÁRIO\n"
    putStr " >"
    hFlush stdout
    tipoFuncionario <- readLn :: IO Int
    tipoFuncionarioValidado <- tipoUsuarioCorreto tipoFuncionario
    if tipoFuncionarioValidado == 1
        then do loginAluno main
    else return()
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"   
    putStrLn "══════════════════════════LOGIN══════════════════════════"   
    putStrLn "\n> Digite o seu cpf: "
    cpf <- getLine
    cpfValido <- cpfCorreto cpf

    putStrLn "> Digite sua senha: "
    senha <- getLine
    senhaValida <- senhaCorreta senha 


    putStrLn "Digite: (C - Confirmar o Login; E - Exit)"
    opcao <- getLine
    let opcaoUpper = map toUpper opcao
    opcaoValida <- padraoCorreto opcaoUpper

    if opcaoValida == "E"
        then do 
            putStrLn "Encerrando o programa!"
            exitSuccess
        else return()


    existeCadastroR <- existeCadastro cpfValido

    tipoEsenhaCorreto <- cadastroCondizComTipoESenha cpfValido tipoFuncionarioValidado senhaValida
    if existeCadastroR
        then do
            if tipoEsenhaCorreto
                then do
                    tipo <- tipoMenu cpfValido
                    case (tipo) of
                        --"2" -> putStrLn "Sou gestor" -- chama menu de gestor
                        "3" -> menuFuncionario main
                else do
                    putStrLn "Senha inválida ou tipo de usuario inválido, tente novamente!"
                    threadDelay (2 * 1000000)
                    main
        else do
            cadastraLogin cpfValido senhaValida tipoFuncionarioValidado
            putStrLn "--Seu primeiro acesso foi cadastrado, faça o login novamente"
            threadDelay (2 * 1000000)
            main


cpfCorreto :: String -> IO String
cpfCorreto cpf = do
    case delimitarCpf cpf of
        Just x -> return x
        Nothing -> do
            putStrLn "CPF não está no formato 000.000.000-00, digite novamente: "
            novocpf <- getLine
            cpfCorreto novocpf

tipoUsuarioCorreto :: Int ->IO Int
tipoUsuarioCorreto x = if verificarIntTipoFuncionario x
                            then return x
                            else do
                                putStrLn "Opção inválida, tente novamente!"
                                novaOpcao <- readLn :: IO Int
                                tipoUsuarioCorreto novaOpcao

senhaCorreta :: String -> IO String
senhaCorreta senha = do
    if length senha >= 6
        then return senha
        else do
            putStrLn "Senha tem que possuir no mínimo 6 caracters, tente novamente!"
            novaSenha <- getLine
            senhaCorreta novaSenha 

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