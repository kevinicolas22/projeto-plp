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
    putStrLn "║   [1]. Gestor                              ║"
    putStrLn "║   [2]. Funcionário                         ║"
    putStrLn "║   [3]. Máquina                             ║"
    putStrLn "║   [4]. Aluno                               ║"
    putStrLn "║   [5]. Financeiro                          ║"
    putStrLn "║   [6]. Sair                                ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
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
    putStrLn "║   [1]. Consultar gestor                    ║"
    putStrLn "║   [2]. Listar gestores                     ║"
    putStrLn "║   [3]. Voltar para o menu                  ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoGestorG <- getLine
    case opcaoGestorG of
        "1" -> consultarGestor
        "2" -> listarGestores
        "3" -> main
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
    putStrLn "║   [1]. Criar funcionario                   ║"
    putStrLn "║   [2]. Atualizar funcionario               ║"
    putStrLn "║   [3]. Listar funcionario                  ║"
    putStrLn "║   [4]. Consultar funcionario               ║"
    putStrLn "║   [5]. Remover funcionario                 ║" 
    putStrLn "║   [6]. Consultar funcionario               ║"
    putStrLn "║   [7]. Voltar para o menu                  ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoFuncionarioG <- getLine
    case opcaoFuncionarioG of
        "1" -> -- nome das funcoes
        "7" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionarioG


menuMaquinaG :: IO ()
menuMaquinaG = do
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Máquina:                       ║"
    putStrLn "║                                                       ║"
    putStrLn "║   [1]. Criar máquina                                  ║"
    putStrLn "║   [2]. Verificar datas de manutenção dos equipamentos ║"
    putStrLn "║   [3]. Listar equipamentos cadastrados                ║"
    putStrLn "║   [4]. Adicionar máquina com necessidade de reparo    ║"
    putStrLn "║   [5]. Listar máquinas com necessidades de reparo     ║"
    putStrLn "║   [6]. Verificar quantidade de máquinas cadastradas   ║"
    putStrLn "║   [7]. Voltar ao menu principal                       ║"
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite a opção:                                   ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    opcaoMaquinaG <- getLine
    case opcaoMaquinaG of 
      "1" -> criarMaquinas
      "2" -> listarMaquinasOrdemAlfabetica 
      "3" -> listarEquipamentos
      "4" -> do
        putStrLn "Informe o ID: "
        id <- getLine
        putStrLn "Informe o nomeG: "
        nomeG <- getLine
        putStrLn "Informe a data de manutenção: "
        dataMan <- getLine
        adicionarMaquinaReparo (Maquina (show id) nomeG dataMan) >> menuMaquina
      "5" -> imprimirMaquinasReparo "maquina_reparo.txt" >> menuMaquina
      "6" -> do
        numeroMaquinas <- contarMaquinas "maquina.txt"
        putStrLn $ "Número de máquinas registradas: " ++ show numeroMaquinas
        menuMaquina
      "7" -> main
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
    putStrLn "║   [1]. Folha de Pagamento do funcionário   ║"
    putStrLn "║   [2]. Renda e Gastos Mensais              ║"
    putStrLn "║   [3]. Voltar para o menu                  ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAlunoG <- getLine
    case opcaoAlunoG of
        "1" -> folhaDePagamento
        "2" -> ----
        "3" -> main
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
