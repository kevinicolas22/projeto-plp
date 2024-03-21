{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (mapMaybe)
import Funcionario
import FuncionarioService
import Manager
import ManagerController
import Maquina
import MaquinaController
import System.Exit
import System.IO
import Text.Read (readMaybe)
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

<<<<<<< HEAD
menuGestor :: IO ()
menuGestor = do
  putStrLn "Opções sobre Gestor:"
  putStrLn "a. Consultar gestor"
  putStrLn "b. Criar gestor"
  putStrLn "c. Listar gestores"
  putStrLn "d. Atualizar gestor"
  putStrLn "e. Remover gestor"
  putStrLn "f. Imprimir folha de pagamento de um funcionário"
  putStrLn "g. Voltar para o menu"
  opcaoGestor <- getLine
  case opcaoGestor of
    "a" -> consultarGestor
    "b" -> criarNovoGestor
    "c" -> listarGestores
    "d" -> atualizarGestor
    "e" -> removerGestor
    --"f" -> folhaDePagamento
    "g" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestor
=======
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
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> gestor

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
>>>>>>> ca03193bf79e9c251b53d19da90ba4222ea0734c

consultarGestor :: IO ()
consultarGestor = do
  putStrLn "Digite o ID do gestor que deseja buscar:"
  id <- getLine
  case readMaybe id of
    Just gestorId -> lerGestorPorId gestorId
<<<<<<< HEAD
    Nothing       -> putStrLn "ID inválido. Por favor, digite um número válido." >> menuGestor
  menuGestor
=======
    Nothing -> putStrLn "ID inválido. Por favor, digite um número válido." >> main
  main
>>>>>>> 491cc3cf30003ae4a9e820815fa4078b621f2e8a

listarGestores :: IO ()
listarGestores = do
<<<<<<< HEAD
  putStrLn "Lista de Todos os Gestores:"
  listarTodosGestores
  main

-- Opção para atualizar um gestor
atualizarGestor :: IO ()
atualizarGestor = do
  putStrLn "Digite o ID do gestor que deseja atualizar:"
  id <- getLine
  putStrLn "Escolha o dado do gestor a ser atualizado:"
  putStrLn "1. cpfGG"
  putStrLn "2. nomeG"
  putStrLn "3. Endereço"
  putStrLn "4. telefoneG"
  putStrLn "5. Data de Nascimento"
  escolha <- getLine
  case escolha of
    "1" -> do
      putStrLn "Digite o novo cpfG:"
      novocpfG <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = novocpfG, nomeG = "", dataNascimento = "", telefoneG = "", enderecoG = ""})
    "2" -> do
      putStrLn "Digite o novo nomeG:"
      novonomeG <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = novonomeG, dataNascimento = "", telefoneG = "", enderecoG = ""})
    "3" -> do
      putStrLn "Digite o novo endereço:"
      novoenderecoG <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = "", telefoneG = "", enderecoG = novoenderecoG})
    "4" -> do
      putStrLn "Digite o novo telefoneG:"
      novotelefoneG <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = "", telefoneG = novotelefoneG, enderecoG = ""})
    "5" -> do
      putStrLn "Digite a nova data de nascimento:"
      novaData <- getLine
      atualizarGestorPorId (read id) (Manager {managerId = read id, cpfG = "", nomeG = "", dataNascimento = novaData, telefoneG = "", enderecoG = ""})
    _ -> putStrLn "Opção inválida."
  main

removerGestor :: IO ()
removerGestor = do
  putStrLn "Digite o ID do gestor que deseja remover:"
  id <- getLine
  removerGestorPorId (read id)
  putStrLn "Gestor removido com sucesso!"
  main

--folhaDePagamento :: IO ()
--folhaDePagamento = do


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
    -- "b" -> verificarDatasManutencao
=======
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
    putStrLn "║   f. Voltar para o menu                    ║"
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
    --"b" -> verificarDatasManutencao
>>>>>>> ca03193bf79e9c251b53d19da90ba4222ea0734c
    "c" -> listarEquipamentos
    "d" -> do
      putStrLn "Informe o ID: "
      id <- getLine
      putStrLn "Informe o nomeG: "
      nomeG <- getLine
      putStrLn "Informe a data de manutenção: "
      dataMan <- getLine
<<<<<<< HEAD
      adicionarMaquinaReparo (Maquina (show id) nome dataMan) >> menuMaquinaG

    "e" -> imprimirMaquinasReparo "maquina_reparo.txt" >> menuMaquinaG

=======
      adicionarMaquinaReparo (Maquina (show id) nomeG dataMan) >> menuMaquina
    "e" -> imprimirMaquinasReparo "maquina_reparo.txt" >> menuMaquina
>>>>>>> 491cc3cf30003ae4a9e820815fa4078b621f2e8a
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
<<<<<<< HEAD
  lerMaquinas "maquina.txt" >> menuMaquinaG
=======
  lerMaquinas "maquina.txt" >> menuMaquina
>>>>>>> 491cc3cf30003ae4a9e820815fa4078b621f2e8a

{-verificarDatasManutencao :: IO ()
verificarDatasManutencao = do
    -- as máquinas em reparo do arquivo
    maquinasReparo <- lerMaquinas "maquina_reparo.txt"
    -- ordem de manutenção
    let maquinasOrdenadas = listarMaquinaPorManutencao maquinasReparo
    -- máquinas ordenadas
    mapM_ mostrarMaquinas maquinasOrdenadas
    menuMaquina-}

<<<<<<< HEAD
menuAluno :: IO ()
menuAluno = do
  putStrLn "Opções sobre Aluno:"
  putStrLn "a. Verificar quantidade de Aluno"
  opcaoAluno <- getLine
  case opcaoAluno of {}
=======

menuAlunoG :: IO()
menuAlunoG = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Aluno:              ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Quantidade de Aluno Cadastrados       ║"
    putStrLn "║   b. Listar alunos                         ║"
    putStrLn "║   c. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAlunoG <- getLine
    case opcaoAlunoG of
        "a" -> -----
        "b" -> -----
        "c" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAlunoG


menuAlunoG :: IO()
menuAlunoG = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Financeiro:         ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Folha de Pagamento                    ║"
    putStrLn "║   b. Renda e Gastos Mensais                ║"
    putStrLn "║   c. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoAlunoG <- getLine
    case opcaoAlunoG of
        "a" -> ----
        "b" -> ----
        "c" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAlunoG
>>>>>>> ca03193bf79e9c251b53d19da90ba4222ea0734c


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
        "5" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> funcionario

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
        "g" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuAlunoF


menuTreinoF :: IO()
menuTreinoF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║           Opções sobre Treinos:            ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. ModificarTreinos                      ║"
    putStrLn "║   b. Gerar Treinos                         ║"
    putStrLn "║   c. Progresso do aluno                    ║"
    putStrLn "║   d. Avaliação fisíca do aluno             ║"
    putStrLn "║   e. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoTreinoF <- getLine
    case opcaoTreinoF of
        "a" -> -- nome das funcoes
        "d" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuTreinoF


menuExtraF :: IO()
menuExtraF = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║      Opções sobre Atividades Extras:       ║"
    putStrLn "║                                            ║"
    putStrLn "║   a. Publicar Horário de Aulas Coletivas   ║"
    putStrLn "║   b. Atualizar aulas coletivas             ║"
    putStrLn "║   c. Voltar para o menu                    ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcaoExtraF <- getLine
    case opcaoExtraF of
        "a" -> -- nome das funcoes
        "c" -> main
        _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuExtraF


aluno :: IO()
aluno = do
    putStrLn "╔════════════════════════════════════════════╗"
    putStrLn "║   Escolha o assunto da funcionalidade      ║"
    putStrLn "║   de Aluno:                                ║"
    putStrLn "║                                            ║"
    putStrLn "║   1.                            ║"
    putStrLn "║   2.                                 ║"
    putStrLn "║   3.                                ║"
    putStrLn "║   4.                                  ║"
    putStrLn "║   5. Sair                                  ║"
    putStrLn "╚════════════════════════════════════════════╝"
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "3" -> 
        "4" -> 
        "5" -> main
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> aluno


academia :: IO()
academia = do
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
    _ -> putStrLn "Opção inválida. Por favor, escolha novamente." >> academia

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

  -- Opção para atualizar um gestor
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

  

































  