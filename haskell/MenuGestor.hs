{-# LANGUAGE PackageImports #-}

module MenuGestor where

import Data.Maybe (mapMaybe)
import Manager
import ManagerService
import Maquina
import Text.Read (readMaybe)
import MaquinaService
import System.Exit
import System.IO
import "directory" System.Directory


menuGestor :: IO()
menuGestor = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   de Gestor:                               ║"
    putStrLn "║                                            ║"
    putStrLn "║   1. Gestor                                ║"
    putStrLn "║   2. Funcionário                           ║"
    putStrLn "║   3. Máquina                               ║"
    putStrLn "║   4. Aluno                                 ║"
    putStrLn "║   5. Financeiro                            ║"
    putStrLn "║   6. Sair                                  ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuGestorG
        "2" -> menuFuncionarioG
        "3" -> menuMaquinaG
        "4" -> menuAlunoG
        "5" -> menuFinanceiroG
        "6" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestor


menuGestorG :: IO()
menuGestorG = do 
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Gestor:             ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Consultar gestor                      ║"
    putStrLn "║   b. Listar gestores                       ║"
    putStrLn "║   c. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoGestorG <- getLine
    case opcaoGestorG of
        "a" -> consultarGestor
        "b" -> listarGestores
        "c" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestor

consultarGestor :: IO ()
consultarGestor = do
  putStrLn "Digite o ID do gestor que deseja buscar:"
  id <- getLine
  case readMaybe id of
    Just gestorId -> lerGestorPorId gestorId
    Nothing -> putStrLn "ID inválido. Por favor, digite um número válido." >> main
  main

listarGestores :: IO ()
listarGestores = do
  putStrLn "Lista de Todos os Gestores:"
  listarTodosGestores
  menuGestor   


menuFuncionarioG :: IO()
menuFuncionarioG = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Funcionario:        ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Criar funcionario                     ║"
    putStrLn "║   b. Atualizar funcionario                 ║"
    putStrLn "║   c. Listar funcionario                    ║"
    putStrLn "║   d. Consultar funcionario                 ║"
    putStrLn "║   e. Remover funcionario                   ║" 
    putStrLn "║   f. Consultar funcionario                 ║"
    putStrLn "║   g. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoFuncionarioG <- getLine
    case opcaoFuncionarioG of
        "a" -> -- nome das funcoes
        "f" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionarioG


menuMaquinaG :: IO ()
menuMaquinaG = do
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Máquina:                       ║"
    putStrLn "║                                                       ║"
    putStrLn "║   a. Criar máquina                                    ║"
    putStrLn "║   b. Verificar datas de manutenção dos equipamentos   ║"
    putStrLn "║   c. Listar equipamentos cadastrados                  ║"
    putStrLn "║   d. Adicionar máquina com necessidade de reparo      ║"
    putStrLn "║   e. Listar máquinas com necessidades de reparo       ║"
    putStrLn "║   f. Verificar quantidade de máquinas cadastradas     ║"
    putStrLn "║   g. Voltar ao menu principal                         ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    opcaoMaquinaG <- getLine
    case opcaoMaquinaG of 
      "a" -> criarMaquinas
      "b" -> listarMaquinasOrdemAlfabetica 
      "c" -> listarEquipamentos
      "d" -> do
        putStrLn "Informe o ID: "
        id <- getLine
        putStrLn "Informe o nomeG: "
        nomeG <- getLine
        putStrLn "Informe a data de manutenção: "
        dataMan <- getLine
        adicionarMaquinaReparo (Maquina (show id) nomeG dataMan) >> menuMaquina
      "e" -> imprimirMaquinasReparo "maquina_reparo.txt" >> menuMaquina
      "f" -> do
        numeroMaquinas <- contarMaquinas "maquina.txt"
        putStrLn $ "Número de máquinas registradas: " ++ show numeroMaquinas
        menuMaquina
      "g" -> main
      _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuMaquinaG

criarMaquinas :: IO ()
criarMaquinas = do
  novaMaquina <- criarMaquina
  adicionarMaquina novaMaquina
  putStrLn "Maquina adicionada com sucesso!"
  menuMaquinaG

listarEquipamentos :: IO ()
listarEquipamentos = do
  lerMaquinas "maquina.txt" >> menuMaquina


menuFinanceiroG :: IO()
menuFinanceiroG = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Financeiro:         ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Folha de Pagamento do funcionário     ║"
    putStrLn "║   b. Renda e Gastos Mensais                ║"
    putStrLn "║   c. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAlunoG <- getLine
    case opcaoAlunoG of
        "a" -> folhaDePagamento
        "b" -> ----
        "c" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFinanceiroG


folhaDePagamento :: IO()
folhaDePagamento = do
    putStrLn "Digite o ID do funcionário para calcular a folha de pagamento:"
    targetId <- getLine 
    let numero = read targetId :: Int
    folhaPagamentoFuncionario numero
    menuFinanceiroG

sair :: IO ()
sair = do
  putStrLn "Saindo..."
  exitSuccess
