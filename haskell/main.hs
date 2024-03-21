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
import Funcionario
import FuncionarioService





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
        "1" -> gestor
        "2" -> funcionario
        "3" -> aluno
        "4" -> academia
        "5" -> sair 
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

gestor :: IO()
gestor = do
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
        "6" -> sair
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

  menuGestorG :: IO()
  menuGestorG = do 
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Gestor:             ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Consultar gestor                      ║"
    putStrLn "║   c. Listar gestores                       ║"
    putStrLn "║   e. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoGestorG <- getLine
    case opcaoGestorG of
        "a" -> consultarGestor
        "b" -> listarGestores
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

listarGestores :: IO ()
listarGestores = do
    putStrLn "Lista de Todos os Gestores:"
    listarTodosGestores
    main   

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
    putStrLn "║   f. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoFuncionarioG <- getLine
    case opcaoFuncionarioG of
        "a" -> -- nome das funcoes
        "e" -> main
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


menuAlunoG :: IO()
menuAlunoG = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Aluno:              ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Quantidade de Aluno Cadastrados       ║"
    putStrLn "║   c. Listar alunos                         ║"
    putStrLn "║   e. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAlunoG <- getLine
    case opcaoAlunoG of
        "a" -> quantidadeAluno
        "b" -> listarAlunos
        "e" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAlunoG


funcionario :: IO()
funcionario = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   de Funcionario:                          ║"
    putStrLn "║                                            ║"
    putStrLn "║   1. Funcionario                           ║"
    putStrLn "║   2. Aluno                                 ║"
    putStrLn "║   3. Treinos                               ║"
    putStrLn "║   4. Extra                                 ║"
    putStrLn "║   5. Sair                                  ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuFuncionarioF
        "2" -> menuAlunoF
        "3" -> menuTreinoF
        "4" -> menuExtraF
        "5" -> sair
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

menuFuncionarioF :: IO()
menuFuncionarioF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Funcionario:        ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Consultar funcionario                 ║"
    putStrLn "║   b. Listar funcionario                    ║"
    putStrLn "║   c. Registrar horario de trabalho         ║"
    putStrLn "║   d. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoFuncionarioF <- getLine
    case opcaoFuncionarioF of
        "a" -> -- nome das funcoes
        "d" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionarioF


menuAlunoF :: IO()
menuAlunoF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Alunos:             ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Controle de entrada/saida de alunos   ║"
    putStrLn "║   b. Cadastro de aluno                     ║"
    putStrLn "║   c. Atualizar aluno                       ║"
    putStrLn "║   d. Remover Aluno                         ║"
    putStrLn "║   e. Listar aluno                          ║"
    putStrLn "║   f. Consultar aluno                       ║"
    putStrLn "║   g. Remover Aluno                         ║"
    putStrLn "║   g. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAlunoF <- getLine
    case opcaoAlunoF of
        "a" -> -- nome das funcoes
        "d" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAlunoF







































































aluno :: IO()
aluno = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   de Aluno:                                ║"
    putStrLn "║                                            ║"
    putStrLn "║   1. Treino                                ║"
    putStrLn "║   2. Pagamento                             ║"
    putStrLn "║   3. Reserva                               ║"
    putStrLn "║   4. Sair                                  ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuTreinoA
        "2" -> menuPagamentoA
        "3" -> menuReservaA
        "4" -> sair
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente."


academia :: IO()
academia = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   do Sistema da Academia:                  ║"
    putStrLn "║                                            ║"
    putStrLn "║   1. Gestor                                ║"
    putStrLn "║   2. Espaços                               ║"
    putStrLn "║   3. Aulas Coletivas                       ║"
    putStrLn "║   4. Sair                                  ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> menuGestorAcad
        "2" -> menuEspaçosAcad
        "3" -> menuAulasAcad
        "4" -> sair
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente."   