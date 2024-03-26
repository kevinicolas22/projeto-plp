{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (mapMaybe)
import Manager
import ManagerService
import Maquina
import Text.Read (readMaybe)
import MaquinaService
import System.Exit
import System.IO
import "directory" System.Directory
import FuncionarioService
import System.Console.ANSI -- Importação do módulo System.Console.ANSI para usar as funções de controle de terminal


main :: IO()
main = do
  menuGestor

menuGestor :: IO()
menuGestor = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   de Gestor:                               ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Gestor                               ║"
    putStrLn "║   [2] Funcionário                          ║"
    putStrLn "║   [3] Máquina                              ║"
    putStrLn "║   [4] Financeiro                           ║"
    putStrLn "║   [5] Sair                                 ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuGestorG
        "2" -> menuFuncionarioG
        "3" -> menuMaquinaG
        "4" -> menuFinanceiroG
        "5" -> menuGestor -- 
        _   -> (putStrLn "Opção inválida. Por favor, escolha novamente.") >> menuGestor

menuGestorG :: IO()
menuGestorG = do 
    limparTerminal
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Gestor:             ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Criar novo gestor                    ║"
    putStrLn "║   [2] Atualizar gestor                     ║"
    putStrLn "║   [3] Listar gestores                      ║"
    putStrLn "║   [4] Consultar gestor                     ║"
    putStrLn "║   [5] Remover gestores                     ║"
    putStrLn "║   [6] Voltar para o menu                   ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoGestorG <- getLine
    case opcaoGestorG of
        "1" -> criarNovoGestor
        "2" -> atualizarGestor
        "3" -> listarGestores
        "4" -> consultarGestor
        "5" -> removerGestor
        "6" -> menuGestor -- 
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestorG

consultarGestor :: IO ()
consultarGestor = do
  putStrLn "Digite o ID do gestor que deseja buscar:"
  id <- getLine
  case readMaybe id of
    Just gestorId -> lerGestorPorId gestorId
    Nothing -> putStrLn "ID inválido. Por favor, digite um número válido." >> menuGestor -- 
  menuGestor -- 
  
listarGestores :: IO ()
listarGestores = do
    putStrLn "Lista de Todos os Gestores:"
    listarTodosGestores
    menuGestor
 
criarNovoGestor :: IO ()
criarNovoGestor = do
  novoGestor <- criarGestor
  adicionarGestor novoGestor
  putStrLn "Gestor adicionado com sucesso!"
  menuGestor


atualizarGestor :: IO ()
atualizarGestor = do
    limparTerminal
    putStrLn "Digite o ID do gestor que deseja atualizar:"
    id <- getLine
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║       Escolha o dado do gestor a ser       ║"
    putStrLn "║              atualizado:                   ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] CPF                                  ║"
    putStrLn "║   [2] Nome                                 ║"
    putStrLn "║   [3] Endereço                             ║"
    putStrLn "║   [4] Telefone                             ║"
    putStrLn "║   [5] Data de Nascimento                   ║"
    putStrLn "╚════════════════════════════════════════════╝"
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite o novo CPF: "
            novoCPF <- getLine
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = novoCPF, nomeG = "", dataNascimento = "", telefoneG = "", enderecoG = ""})
        "2" -> do
            putStrLn "Digite o novo nome: "
            novoNome <- getLine
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = novoNome, dataNascimento = "", telefoneG = "", enderecoG = ""})
        "3" -> do
            putStrLn "Digite o novo endereço: "
            novoEndereco <- getLine
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = "", telefoneG = "", enderecoG = novoEndereco})
        "4" -> do
            putStrLn "Digite o novo telefone: "
            novoTelefone <- getLine
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = "", telefoneG = novoTelefone, enderecoG = ""})
        "5" -> do
            putStrLn "Digite a nova data de nascimento: "
            novaData <- getLine
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = novaData, telefoneG = "", enderecoG = ""})
        _ -> putStrLn "Opção inválida."
    menuGestor

removerGestor :: IO ()
removerGestor = do
  numGestores <- contarGestores "manager.txt"-- Obtém o número de gestores
  if numGestores > 2 then
    do
      putStrLn "Digite o ID do gestor que deseja remover:"
      id <- getLine
      removerGestorPorId (read id)
      putStrLn "Gestor removido com sucesso!"
  else do
    putStrLn "Impossível remover gestor no momento..."
  menuGestor

menuFuncionarioG :: IO()
menuFuncionarioG = do
    limparTerminal
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Funcionario:        ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Criar funcionario                    ║"
    putStrLn "║   [2] Atualizar funcionario                ║"
    putStrLn "║   [3] Listar funcionario                   ║"
    putStrLn "║   [4] Consultar funcionario                ║"
    putStrLn "║   [5] Remover funcionario                  ║" 
    putStrLn "║   [6] Voltar para o menu                   ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoFuncionarioG <- getLine
    case opcaoFuncionarioG of
        "1" -> criarNovoFuncionario
        "6" -> menuGestor -- 
        _   -> (putStrLn "Opção inválida. Por favor, escolha novamente.") >> menuFuncionarioG

criarNovoFuncionario :: IO ()
criarNovoFuncionario = do
  novoFuncionario <- criarFuncionario
  adicionarFuncionario novoFuncionario
  putStrLn "Funcionario criado com sucesso!"
  --menuFuncionarioG



menuMaquinaG :: IO ()
menuMaquinaG = do
    limparTerminal
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Máquina:                       ║"
    putStrLn "║                                                       ║"
    putStrLn "║   [1] Criar máquina                                   ║"
    putStrLn "║   [2] Adicionar máquina com necessidade de reparo     ║"
    putStrLn "║   [3] Listar equipamentos cadastrados                 ║"
    putStrLn "║   [4] Listar máquinas com necessidades de reparo      ║"
    putStrLn "║   [5] Verificar datas de manutenção dos equipamentos  ║"
    putStrLn "║   [6] Verificar quantidade de máquinas cadastradas    ║"
    putStrLn "║   [7] Voltar ao menu principal                        ║"
    putStrLn "║                                                       ║"
    putStrLn "║   > Digite a opção:                                   ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    opcaoMaquinaG <- getLine
    case opcaoMaquinaG of 
      "1" -> criarMaquinas
      "2" -> do
        putStrLn "Informe o ID: "
        id <- getLine 
        putStrLn "Informe o nome: "
        nomeG <- getLine
        putStrLn "Informe a data de manutenção: "
        dataMan <- getLine
        adicionarMaquinaReparo id -- maquina adiconada -- case para voltar ou manter, tirar o voltar automatico 
      "3" -> listarEquipamentos
      "4" -> imprimirMaquinasReparo "haskell/maquina_reparo.txt" -- case para voltar ou manter, tirar o voltar automatico 
      "5" -> listarMaquinasOrdemAlfabetica 
      "6" -> do
        numeroMaquinas <- contarMaquinas "maquina.txt"
        putStrLn $ "Número de máquinas registradas: " ++ show numeroMaquinas
        -- case para voltar ou manter, tirar o voltar automatico 
         
      "7" -> menuGestor -- 
      _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuMaquinaG
    

criarMaquinas :: IO ()
criarMaquinas = do
  novaMaquina <- criarMaquina
  adicionarMaquina novaMaquina
  putStrLn "Maquina adicionada com sucesso!"
  


listarEquipamentos :: IO ()
listarEquipamentos = do
  lerMaquinas "maquina.txt" 
  -- case para voltar ou manter, tirar o voltar automatico 
   


menuFinanceiroG :: IO ()
menuFinanceiroG = do
    limparTerminal
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Financeiro:         ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Folha de Pagamento do funcionário    ║"
    putStrLn "║   [2] Renda e Gastos Mensais               ║"
    putStrLn "║   [3] Voltar para o menu                   ║"
    putStrLn "║                                            ║"
    putStrLn "║   > Digite a opção:                        ║"
    putStrLn "╚════════════════════════════════════════════╝"

    opcaoFinanceiroG <- getLine
    case opcaoFinanceiroG of
        "1" -> folhaDePagamento
        "3" -> menuGestor -- 
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFinanceiroG



folhaDePagamento :: IO()
folhaDePagamento = do
    putStrLn "Digite o ID do funcionário para calcular a folha de pagamento:"
    targetId <- getLine 
    let numero = read targetId :: Int
    folhaPagamentoFuncionario numero
    -- case para voltar ou manter, tirar o voltar automatico 

sair :: IO ()
sair = do
  putStrLn "Saindo..."
  exitSuccess


limparTerminal :: IO ()
limparTerminal = do
      clearScreen
      setCursorPosition 0 0
