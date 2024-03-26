module AlunoController where

import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Control.Concurrent (threadDelay)
import System.Console.ANSI

import System.Environment
import System.IO
import Data.Char (isDigit)
import Treino
import Aluno
import Planos
import Data.List.Split



criarAluno :: IO()
criarAluno = do
  hFlush stdout
  clearScreen
  setCursorPosition 0 0
  putStrLn "   ===== CADASTRO / ALUNO ===== "
  putStr "Nome do aluno: "
  hFlush stdout
  nomeAluno <- getLine
  if not(notNull nomeAluno)
    then do 
      putStrLn "\n> O Nome do aluno não pode estar em branco !" 
      threadDelay (2 * 1000000)
      criarAluno
  else do
    putStr "\nCPF do aluno: "
    hFlush stdout
    cpfAluno <- getLine
    if existeEspaçoEmBranco cpfAluno
      then do
        putStrLn "> Formato inválido ! O cpf não pode ter espaços vazios"
        threadDelay (2 * 1000000)
        criarAluno
    else do
      let cpfDelimitado= delimitarCpf cpfAluno
      if not(length cpfAluno==11)
        then do
          putStrLn "> Formato inválido ! O cpf precisa ter 11 digitos\n"
          threadDelay (2 * 1000000)
          criarAluno 
      else do
        putStr "\nEmail do aluno: "
        hFlush stdout
        emailAluno <- getLine
        putStr "\nEndereço do aluno: "
        hFlush stdout
        endereçoAluno <- getLine
        putStr "\nContato do aluno: "
        hFlush stdout
        contatoAluno <- getLine
        putStrLn "\nEscolha o Plano: \n[A] LIGHT   [B] GOLD     [C] PREMIUM (Default: Light)"
        hFlush stdout
        opção <- getLine
        let planoEscolhido = case opção of
              "a" -> Planos.Light
              "A" -> Planos.Light
              "b" -> Planos.Gold
              "B" -> Planos.Gold
              "c" -> Planos.Premium
              "C" -> Planos.Premium
              _ -> Planos.Light
        conexao <- openFile "haskell/Aluno.txt" ReadMode
        conteudo<- hGetContents conexao
        seq conteudo $ return ()
        let linhas = lines conteudo
            matriculas = primeirosElementos linhas
            matriculaAluno = "00" ++ show(length matriculas+1)
        if not(verificarString matriculaAluno)
          then do
            putStrLn("Formato inválido! ")
            threadDelay (2 * 1000000)
            criarAluno
        else do
          putStr ("\nMatricula : "++ matriculaAluno++"\nNova senha de acesso do aluno (Min: 4 dígitos): ")
          hFlush stdout
          senhaAluno <- getLine
          if not(verificarStringSenha senhaAluno)
          then do
            putStrLn("Formato inválido! ")
            threadDelay (2 * 1000000)
            criarAluno
          else do
            putStrLn "\nConfirmar Cadastro... Pressione ENTER"
            confirma <- getChar
            when (confirma /= '\n') $ void getChar -- Aguarda o Enter
            let alunoCriado = Aluno { alunoId = 0, nome = nomeAluno, cpf = cpfDelimitado, endereço = endereçoAluno, contato = contatoAluno, planoAluno = planoEscolhido, treinos = [], emDia = False, matricula= matriculaAluno, senha= senhaAluno, email = emailAluno, aulas = []}
            appendFile "haskell/Aluno.txt" (alunoToString alunoCriado ++ "\n") 
          



                      
exibeTreinosAluno:: [Treino]-> String
exibeTreinosAluno treinos = unlines (map exibeTreino treinos)

adicionaTreinoAluno :: Aluno -> Treino -> Aluno
adicionaTreinoAluno aluno novoTreino = aluno { treinos = novoTreino : treinos aluno }

exibeTreino::Treino-> String
exibeTreino treino= do
  showTreino(treino)

exibirAluno:: Aluno->String
exibirAluno aluno= show aluno

verificarString :: String -> Bool
verificarString str = length str >= 3 && all isDigit str

verificarStringSenha:: String-> Bool
verificarStringSenha str= length str>= 4

existeEspaçoEmBranco:: String-> Bool
existeEspaçoEmBranco str= (' ' `elem` str)

delimitarCpf :: String -> String
delimitarCpf cpfA
    | length numeros == 11  = cpfFormatado
    where
        numeros = filter isDigit cpfA
        cpfFormatado = intercalate "."[chunk 0 3, chunk 3 6, chunk 6 9] ++ "-" ++ take 2(drop 9 numeros)
        chunk start end = take(end - start)(drop start numeros)

notNull:: String-> Bool
notNull str = do
    not(length str==0)
  


alunoToString :: Aluno -> String
alunoToString alunoTo =
      let planoStr = case planoAluno alunoTo of
            Planos.Light -> "Light"
            Planos.Gold -> "Gold"
            Planos.Premium -> "Premium"
      in intercalate ";" [ matricula alunoTo
                        , show (alunoId alunoTo)
                        , nome alunoTo
                        , cpf alunoTo
                        , endereço alunoTo
                        , contato alunoTo
                        , planoStr
                        , show (treinos alunoTo)
                        , show (emDia alunoTo)
                        , senha alunoTo
                        , email alunoTo
                        , show (aulas alunoTo)
                        ]