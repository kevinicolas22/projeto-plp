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
import Funcionario
import Control.Concurrent
import System.Console.ANSI -- Importação do módulo System.Console.ANSI para usar as funções de controle de terminal


main :: IO()
main = do
  menuGestor

menuGestor :: IO()
menuGestor = do
    limparTerminal
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
        "6" -> menuGestor 
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestorG

consultarGestor :: IO ()
consultarGestor = do
  limparTerminal
  putStrLn ">> Digite o ID do gestor que deseja buscar:"
  id <- getLine
  case readMaybe id of
    Just gestorId -> lerGestorPorId gestorId
    Nothing -> putStrLn "ID inválido. Por favor, digite um número válido." 
  putStrLn "\n\n [0] Voltar"
  op <- getLine
  case op of
    "0" -> menuGestorG
  
listarGestores :: IO ()
listarGestores = do
    limparTerminal
    putStrLn ">> Lista de Todos os Gestores << \n"
    listarTodosGestores
    putStrLn "\n\n [0] Voltar"
    op <- getLine
    case op of
      "0" -> menuGestorG
    
 
criarNovoGestor :: IO ()
criarNovoGestor = do
  novoGestor <- criarGestor
  adicionarGestor novoGestor
  putStrLn "\n\nGestor adicionado com sucesso!"
  threadDelay (2 * 1000000)
  limparTerminal
  menuGestorG


atualizarGestor :: IO ()
atualizarGestor = do
    limparTerminal
    putStrLn ">> Digite o ID do gestor que deseja atualizar:"
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
            putStrLn ">> Digite o novo CPF: "
            novoCPF <- getLine
            putStrLn"\nGestor Atualizado!"
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = novoCPF, nomeG = "", dataNascimento = "", telefoneG = "", enderecoG = ""})
            
        "2" -> do
            putStrLn "\n>> Digite o novo nome: "
            novoNome <- getLine
            putStrLn"\nGestor Atualizado!"
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = novoNome, dataNascimento = "", telefoneG = "", enderecoG = ""})
            
        "3" -> do
            putStrLn "\n>> Digite o novo endereço: "
            novoEndereco <- getLine
            putStrLn"\nGestor Atualizado!"
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = "", telefoneG = "", enderecoG = novoEndereco})
            
        "4" -> do
            putStrLn "\n>> Digite o novo telefone: "
            novoTelefone <- getLine
            putStrLn"\nGestor Atualizado!"
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = "", telefoneG = novoTelefone, enderecoG = ""})
            
        "5" -> do
            putStrLn "\n>> Digite a nova data de nascimento: "
            novaData <- getLine
            putStrLn"\nGestor Atualizado!"
            atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = novaData, telefoneG = "", enderecoG = ""})
            
        _ -> putStrLn "Opção inválida!"
    limparTerminal
    menuGestorG

removerGestor :: IO ()
removerGestor = do
  limparTerminal
  numGestores <- contarGestores "manager.txt"-- Obtém o número de gestores
  if numGestores > 2 then
    do
      putStrLn ">> Digite o ID do gestor que deseja remover:"
      id <- getLine
      removerGestorPorId (read id)
      putStrLn"Removendo..."
      threadDelay (2 * 1000000)
      putStrLn "Gestor removido com sucesso!"
  else do
    putStrLn "Não foi possível remover gestor no momento..."
    threadDelay (2 * 1000000)  
  menuGestorG

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
        "2" -> atualizarFuncionarioOpcao
        "3" -> lerTodosFuncionarios
        "4" -> lerFuncionarioOpcao
        "5" -> removerFuncionarioOpcao
        "6" -> menuGestor 
        _   -> (putStrLn "Opção inválida. Por favor, escolha novamente.") >> menuFuncionarioG

criarNovoFuncionario :: IO ()
criarNovoFuncionario = do
  novoFuncionario <- criarFuncionario
  adicionarFuncionario novoFuncionario
  putStrLn "\n Funcionario criado com sucesso!"
  threadDelay (2 * 1000000)
  limparTerminal
  menuFuncionarioG


lerTodosFuncionarios :: IO()
lerTodosFuncionarios = do
    limparTerminal
    listarTodosFuncionarios
    putStrLn "\n\n [0] Voltar"
    op <- getLine
    case op of
      "0" -> menuFuncionarioG

-- Opção para ler informações de um funcionário por id
lerFuncionarioOpcao :: IO ()
lerFuncionarioOpcao = do
    limparTerminal
    putStrLn ">> Digite o ID do funcionário que deseja buscar:"
    id <- getLine
    putStrLn"Procurando...\n"
    threadDelay (2 * 1000000)
    lerFuncionarioPorId (read id)
    putStrLn "\n\n [0] Voltar"
    op <- getLine
    case op of
      "0" -> menuFuncionarioG

-- Opção para atualizar um funcionário
atualizarFuncionarioOpcao :: IO ()
atualizarFuncionarioOpcao = do
    limparTerminal
    putStrLn ">> Digite o ID do funcionário que deseja atualizar:"
    id <- getLine
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║     Escolha o dado do funcionario a ser    ║"
    putStrLn "║              atualizado:                   ║"
    putStrLn "║                                            ║"
    putStrLn "║   [1] Nome                                 ║"
    putStrLn "║   [2] CPF                                  ║"
    putStrLn "║   [3] Endereço                             ║"
    putStrLn "║   [4] Telefone                             ║"
    putStrLn "║   [5] Data de Ingresso                     ║"
    putStrLn "║   [6] Salário                              ║"
    putStrLn "╚════════════════════════════════════════════╝"
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Digite o novo nome:"
            novoNome <- getLine
            putStrLn"Funcionário atualizado!"
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = novoNome, cpf = "", endereco = "", telefone = "", data_ingresso = "", salario = 0.0})
            
        "2" -> do
            putStrLn "Digite o novo CPF:"
            novoCPF <- getLine
            putStrLn"Funcionário atualizado!"
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = novoCPF, endereco = "", telefone = "", data_ingresso = "", salario = 0.0})
            
        "3" -> do
            putStrLn "Digite o novo endereço:"
            novoEndereco <- getLine
            putStrLn"Funcionário atualizado!"
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = novoEndereco, telefone = "", data_ingresso = "", salario = 0.0})
            
        "4" -> do
            putStrLn "Digite o novo telefone:"
            novoTelefone <- getLine
            putStrLn"Funcionário atualizado!"
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = novoTelefone, data_ingresso = "", salario = 0.0})
            
        "5" -> do
            putStrLn "Digite a nova data de ingresso:"
            novaData <- getLine
            putStrLn"Funcionário atualizado!"
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = "", data_ingresso = novaData, salario = 0.0})
            
        "6" -> do
            putStrLn "Digite o novo salário:"
            novoSalario <- readLn :: IO Float
            putStrLn"Funcionário atualizado!"
            atualizarFuncionarioPorId (read id) (Funcionario {funcId = read id, nome = "", cpf = "", endereco = "", telefone = "", data_ingresso = "", salario = novoSalario})
          
        _   -> putStrLn "Opção inválida."
    limparTerminal
    menuFuncionarioG

-- Opção para remover um funcionário
removerFuncionarioOpcao :: IO ()
removerFuncionarioOpcao = do
    putStrLn ">> Digite o ID do funcionário que deseja remover:"
    id <- getLine
    removerFuncionarioPorId (read id)
    putStrLn"Removendo..."
    threadDelay (2 * 1000000)
    putStrLn "Funcionário removido com sucesso!"
    threadDelay (2 * 1000000)
    menuFuncionarioG





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
        limparTerminal
        putStrLn ">> Informe o ID: "
        id <- getLine 
        putStrLn "\n>> Informe o nome: "
        nomeG <- getLine
        putStrLn "\n>> Informe a data de manutenção: "
        dataMan <- getLine
        putStrLn"Adicionando..."
        threadDelay (2 * 1000000)
        adicionarMaquinaReparo id
        putStrLn "\n\n [0] Voltar"
        op <- getLine
        case op of
          "0" -> menuMaquinaG
      "3" -> listarEquipamentos 
      "4" -> imprimirMaquinasReparo  "haskell/maquina_reparo.txt" >> menuMaquinaG
      "5" -> listarMaquinasOrdemAlfabetica 
      "6" -> do
        numeroMaquinas <- contarMaquinas "haskell/maquina.txt"
        limparTerminal
        putStrLn $ ">> Número de máquinas registradas: " ++ show numeroMaquinas
        putStrLn "\n\n [0] Voltar"
        op <- getLine
        case op of
          "0" -> menuMaquinaG
         
      "7" -> menuGestor -- 
      _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuMaquinaG
    

criarMaquinas :: IO ()
criarMaquinas = do
  novaMaquina <- criarMaquina
  adicionarMaquina novaMaquina
  putStrLn "Maquina adicionada com sucesso!"
  threadDelay (2 * 1000000)
  menuMaquinaG

  


listarEquipamentos :: IO ()
listarEquipamentos = do
  limparTerminal
  lerMaquinas "haskell/maquina.txt" 
  putStrLn "\n\n [0] Voltar"
  op <- getLine
  case op of
    "0" -> menuMaquinaG
   


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
        "3" -> menuGestor 
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFinanceiroG



folhaDePagamento :: IO()
folhaDePagamento = do
    limparTerminal
    putStrLn ">> Digite o ID do funcionário para calcular a folha de pagamento:"
    targetId <- getLine 
    let numero = read targetId :: Int
    folhaPagamentoFuncionario numero
    putStrLn "\n\n [0] Voltar"
    op <- getLine
    case op of
      "0" -> menuFinanceiroG

sair :: IO ()
sair = do
  putStrLn "Saindo..."
  exitSuccess


limparTerminal :: IO ()
limparTerminal = do
      clearScreen
      setCursorPosition 0 0
