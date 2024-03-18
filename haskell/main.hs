{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (mapMaybe)
import Manager
import ManagerController
import Maquina
import Text.Read (readMaybe)
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
  putStrLn "e. Voltar para o menu"
  opcaoGestor <- getLine
  case opcaoGestor of
    "a" -> consultarGestor
    "b" -> criarNovoGestor
    "c" -> listarGestores
    "d" -> atualizarGestor
    "f" -> removerGestor
    "e" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestor

consultarGestor :: IO ()
consultarGestor = do
  putStrLn "Digite o ID do gestor que deseja buscar:"
  id <- getLine
  case readMaybe id of
    Just gestorId -> lerGestorPorId gestorId
    Nothing       -> putStrLn "ID inválido. Por favor, digite um número válido." >> main
  main

criarNovoGestor :: IO ()
criarNovoGestor = do
  novoGestor <- criarGestor
  adicionarGestor novoGestor
  putStrLn "Gestor adicionado com sucesso!"
  main

listarGestores :: IO ()
listarGestores = do
    putStrLn "Lista de Todos os Gestores:"
    listarTodosGestores
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
        atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = novoCPF, nome = "", dataNascimento = "", telefone = "", endereco = ""})
    "2" -> do
        putStrLn "Digite o novo nome:"
        novoNome <- getLine
        atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = novoNome, dataNascimento = "", telefone = "", endereco = ""})
    "3" -> do
      putStrLn "Digite o novo endereço:"
      novoEndereco <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = "", dataNascimento = "", telefone = "", endereco = novoEndereco})
    "4" -> do
      putStrLn "Digite o novo telefone:"
      novoTelefone <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = "", dataNascimento = "", telefone = novoTelefone, endereco = ""})
    "5" -> do
      putStrLn "Digite a nova data de nascimento:"
      novaData <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = "", dataNascimento = novaData, telefone = "", endereco = ""})
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
  putStrLn "a. Criar máquina"
  putStrLn "b. Verificar datas de manutenção dos equipamentos"
  putStrLn "c. Listar equipamentos cadastrados"
  putStrLn "d. Adicionar máquina com necessidade de reparo"
  putStrLn "e. Listar máquinas com necessidades de reparo"
  putStrLn "f. Verificar quantidade de máquinas cadastradas"
  putStrLn "g. Voltar ao menu principal"
  opcaoMaquina <- getLine
  case opcaoMaquina of
    "a" -> criarMaquinas
    --"b" -> verificarDatasManutencao
    "c" -> listarEquipamentos
    "d" ->  do
      putStrLn "Informe o ID: "
      id <- getLine
      putStrLn "Informe o nome: "
      nome <- getLine
      putStrLn "Informe a data de manutenção: "
      dataMan <- getLine
      adicionarMaquinaReparo (Maquina (show id) nome dataMan) >> menuMaquina

    "e" -> imprimirMaquinasReparo "maquina_reparo.txt" >> menuMaquina

    "f" -> do
      numeroMaquinas <- contarMaquinas "maquina.txt"
      putStrLn $ "Número de máquinas registradas: " ++ show numeroMaquinas 
      menuMaquina

    "g" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuMaquina

criarMaquinas :: IO ()
criarMaquinas = do
  novaMaquina <- criarMaquina
  adicionarMaquina novaMaquina
  putStrLn "Maquina adicionada com sucesso!"
  main

listarEquipamentos :: IO()
listarEquipamentos = do
  lerMaquinas "maquina.txt" >> menuMaquina 

{-verificarDatasManutencao :: IO ()
verificarDatasManutencao = do
    -- as máquinas em reparo do arquivo
    maquinasReparo <- lerMaquinas "maquina_reparo.txt"
    -- ordem de manutenção
    let maquinasOrdenadas = listarMaquinaPorManutencao maquinasReparo
    -- máquinas ordenadas
    mapM_ mostrarMaquinas maquinasOrdenadas
    menuMaquina-}


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