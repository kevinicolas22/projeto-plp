{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (mapMaybe)
import System.Exit
import System.IO
import Text.Read (readMaybe)
import "directory" System.Directory
import MenuFuncionario

main :: IO()
main = do
    putStrLn "╔═══════════════════════════════╗"
    putStrLn "║   Bem-vindo à CodeFit!        ║"
    putStrLn "║   Escolha como deseja entrar: ║"
    putStrLn "║   1. Gestor                   ║"
    putStrLn "║   2. Funcionário              ║"
    putStrLn "║   3. Aluno                    ║"
    putStrLn "║   4. Sistema da Academia      ║"
    putStrLn "║   5. Sair                     ║"
    putStrLn "╚═══════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuGestor
        "2" -> menuFuncionario
        "3" -> menuAluno
        "4" -> menuAcademia
        "5" -> sair 
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main


































  