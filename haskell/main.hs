{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (mapMaybe)
import Manager
import ManagerController
import Maquina
import MaquinaController
import System.Exit
import System.IO
import "directory" System.Directory

-- Função principal
main :: IO ()
main = do
  putStrLn "Escolha o assunto da funcionalidade de Gestor:"
  putStrLn "1. Gestor"
  putStrLn "2. Funcionário"
  putStrLn "3. Máquina"
  putStrLn "4. Aluno"
  putStrLn "5. Sair"
  opcao <- getLine
  case opcao of
    "1" -> menuGestor
    "2" -> menuFuncionario
    "3" -> menuMaquina
    "4" -> menuAluno
    "5" -> sair
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

menuGestor :: IO ()
menuGestor = do
  putStrLn "Opções sobre Gestor:"
  putStrLn "a. Consultar gestor"
  putStrLn "b. Criar gestor"
  putStrLn "c. Listar gestores"
  putStrLn "d. Atualizar gestor"
  putStrLn "f. Remover gestor"
  opcaoGestor <- getLine
  case opcaoGestor of
    "a" -> consultarGestor
    "b" -> criarNovoGestor
    "d" -> atualizarGestor
    "f" -> removerGestor
    "e" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestor

consultarGestor :: IO ()
consultarGestor = do
  putStrLn "Digite o ID do gestor que deseja buscar:"
  id <- getLine
  lerGestorPorId (read id)
  main

criarNovoGestor :: IO ()
criarNovoGestor = do
  novoGestor <- criarGestor
  adicionarGestor novoGestor
  putStrLn "Gestor adicionado com sucesso!"
  main

-- Opção para atualizar um gestor
atualizarGestor :: IO ()
atualizarGestor = do
  putStrLn "Digite o ID do gestor que deseja atualizar:"
  id <- getLine
  putStrLn "Escolha o dado do gestor a ser atualizado:"
  putStrLn "1. CPF"
  putStrLn "2. Nome"
  putStrLn "3. Endereço"
  putStrLn "4. Telefone"
  putStrLn "5. Data de Nascimento"
  escolha <- getLine
  case escolha of
    "1" -> do
        putStrLn "Digite o novo CPF:"
        novoCPF <- getLine
        atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = novoCPF, name = "", birth = "", telephone = "", address = ""})
    "2" -> do
        putStrLn "Digite o novo nome:"
        novoNome <- getLine
        atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", name = novoNome, birth = "", telephone = "", address = ""})
    "3" -> do
      putStrLn "Digite o novo endereço:"
      novoEndereco <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", name = "", birth = "", telephone = "", address = novoEndereco})
    "4" -> do
      putStrLn "Digite o novo telefone:"
      novoTelefone <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", name = "", birth = "", telephone = novoTelefone, address = ""})
    "5" -> do
      putStrLn "Digite a nova data de nascimento:"
      novaData <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", name = "", birth = novaData, telephone = "", address = ""})
    _ -> putStrLn "Opção inválida."
  main

removerGestor :: IO ()
removerGestor = do
  putStrLn "Digite o ID do gestor que deseja remover:"
  id <- getLine
  removerGestorPorId (read id)
  putStrLn "Gestor removido com sucesso!"
  main

menuFuncionario :: IO ()
menuFuncionario = do
  putStrLn "Opções sobre Funcionário:"
  putStrLn "a. Excluir funcionario"
  opcaoFuncionario <- getLine
  case opcaoFuncionario of
    -- "a" ->
    -- "c" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionario

menuMaquina :: IO ()
menuMaquina = do
  putStrLn "Opções sobre Máquina:"
  putStrLn "1. Criar máquina"
  putStrLn "a. Controle de datas de manutenção dos equipamentos"
  putStrLn "b. Listar equipamentos cadastrados"
  putStrLn "c. Adicionar máquina com necessidade de reparo"
  putStrLn "d. Listar máquinas com quebradas/necessidades de reparo"
  putStrLn "e. Verificar quantidade de máquinas cadastradas"
  putStrLn "f. Voltar ao menu principal"
  opcaoMaquina <- getLine
  case opcaoMaquina of
    "1" -> criarMaquinas
    -- "a" -> controleStatusEqp
    -- "b" -> listarEquipamentos
    -- "c" -> main
    "c" -> do
      putStrLn "Informe o ID: "
      id <- getLine
      putStrLn "Informe o nome: "
      nome <- getLine
      putStrLn "Informe a data de manutenção: "
      dataMan <- getLine
      adicionarMaquinaReparo (Maquina id nome (read dataMan :: Int)) >> menuMaquina
    "d" -> imprimirMaquinasReparo "maquina_reparo.txt" >> menuMaquina
    "e" -> do
      numeroMaquinas <- contarMaquinas "maquina.txt"
      putStrLn $ "Número de máquinas registradas: " ++ show numeroMaquinas 
      menuMaquina
    "f" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuMaquina

criarMaquinas :: IO ()
criarMaquinas = do
  novaMaquina <- criarMaquina
  adicionarMaquina novaMaquina
  putStrLn "Maquina adicionada com sucesso!"
  main

menuAluno :: IO ()
menuAluno = do
  putStrLn "Opções sobre Aluno:"
  putStrLn "a. Verificar quantidade de Aluno"
  opcaoAluno <- getLine
  case opcaoAluno of {}

-- "a" -> verificarAlunos

sair :: IO ()
sair = do
  putStrLn "Saindo..."
  exitSuccess