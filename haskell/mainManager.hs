import System.IO
import System.Directory
import System.Exit
import Funcionario
import Data.Maybe (mapMaybe)

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
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> main

menuGestor :: IO ()
menuGestor = do
    putStrLn "Opções sobre Gestor:"
    putStrLn "a. Consultar gestor"
    putStrLn "b. Criar gestor"
    putStrLn "c. Listar gestores"
    opcaoGestor <- getLine
    case opcaoGestor of
        "a" -> -- a funcao que tem em cima
        "b" -> criarNovoGestor
        "e" -> main
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuGestor

criarNovoGestor :: IO ()
criarNovoGestor = do
    novoGestor <- criarGestor
    adicionarGestor novoGestor
    putStrLn "Gestor adicionado com sucesso!"
    main


menuFuncionario :: IO ()
menuFuncionario = do
    putStrLn "Opções sobre Funcionário:"
    putStrLn "a. Excluir funcionario"
    opcaoFuncionario <- getLine
    case opcaoFuncionario of
        "a" -> -- funcionalidade nome
        "c" -> main
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuFuncionario

menuMaquina :: IO ()
menuMaquina = do
    putStrLn "Opções sobre Máquina:"
    putStrLn "a. Controle de datas de manutenção dos equipamentos"
    putStrLn "b. Listar equipamentos cadastrados"
    putStrLn "c. Voltar ao menu principal"
    opcaoMaquina <- getLine
    case opcaoMaquina of
        "a" -> controleStatusEqp
        "b" -> listarEquipamentos
        "c" -> main
        _   -> putStrLn "Opção inválida. Por favor, escolha novamente." >> menuMaquina

menuAluno :: IO
menuAluno = do
    putStrLn "Opções sobre Aluno:"
    putStrLn "a. Verificar quantidade de Aluno"
    opcaoAluno <- getLine
    case opcaoAluno of
        "a" -> verificarAlunos

sair :: IO ()
sair = do
    putStrLn "Saindo..."
    exitSuccess