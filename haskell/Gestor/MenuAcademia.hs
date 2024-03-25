{-# LANGUAGE PackageImports #-}

module MenuAcademia where


import Data.Maybe (mapMaybe)
import Manager
import ManagerService
import Maquina
import Text.Read (readMaybe)
import MaquinaService
import System.Exit
import System.IO
import "directory" System.Directory


menuAcademia :: IO()
menuAcademia = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   do Sistema CodeFit:                      ║"
    putStrLn "║                                            ║"
    putStrLn "║   1. Gestor                                ║"
    putStrLn "║   2. Espaços Oferecidos                    ║"
    putStrLn "║   3. Aulas Coletivas                       ║"
    putStrLn "║   4. Planos                                ║"
    putStrLn "║   5. Sair                                  ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuGestorAcad
        "2" -> menuEspacoAcad
        "3" -> menuAulasAcad
        "4" -> menuPlanosAcad
        "5" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAcademia

menuGestorAcad :: IO()
menuGestorAcad = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Gestor:             ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Criar Gestor                          ║"
    putStrLn "║   b. Atualizar Gestor                      ║"
    putStrLn "║   c. Remover Gestor                        ║"
    putStrLn "║   d. Listar Gestor                         ║"
    putStrLn "║   e. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoGestorAcad <- getLine
    case opcaoGestorAcad of
        "a" -> criarNovoGestor
        "b" -> atualizarGestor
        "c" -> removerGestor
        "d" -> listarGestor
        "e" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestorAcad


criarNovoGestor :: IO ()
criarNovoGestor = do
  novoGestor <- criarGestor
  adicionarGestor novoGestor
  putStrLn "Gestor adicionado com sucesso!"
  menuGestorAcad    

atualizarGestor :: IO ()
atualizarGestor = do
  putStrLn "Digite o ID do gestor que deseja atualizar:"
  id <- getLine
  putStrLn "╔════════════════════════════════════════════╗"
  putStrLn "║       Escolha o dado do gestor a ser       ║"
  putStrLn "║              atualizado:                   ║"
  putStrLn "║                                            ║"
  putStrLn "║   1. CPF                                   ║"
  putStrLn "║   2. Nome                                  ║"
  putStrLn "║   3. Endereço                              ║"
  putStrLn "║   4. Telefone                              ║"
  putStrLn "║   5. Data de Nascimento                    ║"
  putStrLn "╚════════════════════════════════════════════╝"
  escolha <- getLine
  case escolha of
    "1" -> do
        putStrLn "Digite o novo CPF: "
        novoCPF <- getLine
        atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = novoCPF, nome = "", dataNascimento = "", telefone = "", endereco = ""})
    "2" -> do
        putStrLn "Digite o novo nome: "
        novoNome <- getLine
        atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = novoNome, dataNascimento = "", telefone = "", endereco = ""})
    "3" -> do
      putStrLn "Digite o novo endereço: "
      novoEndereco <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = "", dataNascimento = "", telefone = "", endereco = novoEndereco})
    "4" -> do
      putStrLn "Digite o novo telefone: "
      novoTelefone <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = "", dataNascimento = "", telefone = novoTelefone, endereco = ""})
    "5" -> do
      putStrLn "Digite a nova data de nascimento: "
      novaData <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpf = "", nome = "", dataNascimento = novaData, telefone = "", endereco = ""})
    _ -> putStrLn "Opção inválida."
  menuGestorAcad


removerGestor :: IO ()
removerGestor = do
  putStrLn "Digite o ID do gestor que deseja remover:"
  id <- getLine
  removerGestorPorId (read id)
  putStrLn "Gestor removido com sucesso!"
  menuGestorAcad

listarGestor :: IO ()
listarGestor = do
    putStrLn "Lista de Todos os Gestores:"
    listarTodosGestores
    menuGestorAcad


menuEspacoAcad :: IO()
menuEspacoAcad = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Espaços da CodeFit:              ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoEspacoAcad <- getLine
    case opcaoEspacoAcad of
        "a" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuEspacoAcad


menuAulasAcad :: IO()
menuAulasAcad = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║       Aulas Coletivas da CodeFit:          ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAulasAcad <- getLine
    case opcaoAulasAcad of
        "a" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAulasAcad



menuPlanosAcad :: IO()
menuPlanosAcad = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║      Planos Imperdíveis da CodeFit:        ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoPlanosAcad <- getLine
    case opcaoPlanosAcad of
        "a" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuPlanosAcad