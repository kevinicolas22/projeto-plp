module AlunoController where

import Control.Monad
import Network.Mail.SMTP 
import qualified Network.Mail.Mime (Mail(..), Address(..), simpleMail) 
import Data.Text(pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Mail.SMTP.Types (Address(..))
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import LoginService(delimitarCpf)
import System.Environment
import System.IO
import Data.Char (isDigit)
import Treino
import Aluno
import Planos
import Aula
import Data.List.Split
import System.Directory (removeFile, renameFile)
import Data.List (elemIndex,intercalate)
import Data.Char(toUpper, isSpace)

--Função que realiza a leitura dos dados de um aluno para transforma-los em uma instância Aluno, e por fim adiciona suas informações no arquivo Aluno.txt
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
        matriculaAluno <- gerarMatriculaUnica 1 linhas  -- funçao que gera uma matricula automaticamente
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
            threadDelay (2 * 1000000) -- atraso temporario da execuçao
            criarAluno
          else do
            putStrLn "\nConfirmar Cadastro... Pressione ENTER"
            confirma <- getChar
            when (confirma /= '\n') $ void getChar -- Aguarda o Enter
            --Instancia aluno
            let alunoCriado = Aluno { alunoId = 0, nomeAluno = nomeAluno, cpfAluno = cpfDelimitado, endereçoAluno = endereçoAluno, contatoAluno = contatoAluno, planoAluno = planoEscolhido, treinos = [], emDia = False, matricula= matriculaAluno, senhaAluno= senhaAluno, emailAluno = emailAluno, aulas = []}
            appendFile "haskell/Aluno.txt" (alunoToString alunoCriado) 

-- Funçao que deleta o aluno do arquivo de dados a partir da sua matrícula
deletarAluno:: String->IO()
deletarAluno matriculaAluno = do
  conexao<- openFile "haskell/Aluno.txt" ReadMode
  conteudo<- hGetContents conexao
  let linhas = lines conteudo
      matriculas = primeirosElementos linhas
  alunos <-mapM recuperaAlunoMatricula matriculas
  if matriculaAluno `elem` matriculas
    then do
      let novaListaAlunos = filter (\aluno -> matricula aluno /= matriculaAluno) alunos
      (tempName, tempHandle) <- openTempFile "." "temp"
      hPutStr tempHandle (concatMap alunoToString novaListaAlunos)
      hClose conexao
      hClose tempHandle
      removeFile "haskell/Aluno.txt"
      renameFile tempName "haskell/Aluno.txt"
      putStrLn " Aluno excluído com Sucesso !!"
    else do
      putStrLn" Matrícula não encontrada !!"
            

-- Função que recebe uma matrícula e retorna uma instância do tipo Aluno, que possui essa matrícula
recuperaAlunoMatricula:: String-> IO Aluno
recuperaAlunoMatricula matStr= do
      conexao <- openFile "haskell/Aluno.txt" ReadMode
      conteudo<- hGetContents conexao
      seq conteudo $ return ()
      let linhas = lines conteudo
          matriculas = primeirosElementos linhas
          dadosAluno = filtrarMatricula matStr linhas
          treinosStr = read (dadosAluno !! 7) :: [(String, [String])]
          treinos = map (\(tipo, exercicios) -> Treino tipo exercicios) treinosStr
          aulasStr = read (dadosAluno !! 11) :: [(String, String, [String])]
          aulas = map (\(nome, horario, planos) -> Aula nome horario (map readPlanosTipo planos)) aulasStr
      let aluno = Aluno{ matricula = dadosAluno !! 0
                              , alunoId = read (dadosAluno !! 1)
                              , nomeAluno = dadosAluno !! 2
                              , cpfAluno = dadosAluno !! 3
                              , endereçoAluno = dadosAluno !! 4
                              , contatoAluno = dadosAluno !! 5
                              , planoAluno = readPlano (dadosAluno !! 6)
                              , treinos = treinos
                              , emDia = read (dadosAluno !! 8)
                              , senhaAluno = dadosAluno !! 9
                              , emailAluno = dadosAluno !! 10
                              , aulas = aulas
                              }
      hClose conexao 
      return aluno  
  
-- Função para gerar uma matrícula única
gerarMatriculaUnica :: Int-> [String]-> IO String
gerarMatriculaUnica count linhas= do
  let matriculas = primeirosElementos linhas
      matriculaAluno = "00" ++ show (count)
  if matriculaAluno `elem` matriculas
    then gerarMatriculaUnica (count+1) linhas -- Se a matrícula já existe, tente novamente
    else return matriculaAluno

--Função para exibir os treinos de um aluno, a partir da sua lista de Treinos
exibeTreinosAluno:: [Treino]-> String
exibeTreinosAluno treinos = unlines (map exibeTreino treinos)

-- Função que complementa a função acima, recebendo um treino e retornando o seu valor de String
exibeTreino::Treino-> String
exibeTreino treino= do
  showTreino(treino)

-- Funçao que recebe um Treino e o adiciona a lista de treinos do aluno
adicionaTreinoAluno :: Aluno -> Treino -> Aluno
adicionaTreinoAluno aluno novoTreino = aluno { treinos = novoTreino : treinos aluno }

--Funçao para exibir os dados de um aluno
exibirAluno:: Aluno->String
exibirAluno aluno= show aluno

--Verifica se a string passada como matrícula possui o formato adotado pelo sistema (minimo de 3 digitos numericos)
verificarString :: String -> Bool
verificarString str = length str >= 3 && all isDigit str

--Verifica se a String passada como senha possui o formato adotado pelo sistema( tamanho minimo = 4)
verificarStringSenha:: String-> Bool
verificarStringSenha str= length str>= 4

--Função que garante que não há espaços em branco que possam corromper a leitura dos dados
existeEspaçoEmBranco:: String-> Bool
existeEspaçoEmBranco str= (' ' `elem` str)


notNull:: String-> Bool
notNull str = do
    not(length str==0)
  
-- Função que gera a representação dos dados de um Aluno para a inserção no arquivo Txt 
alunoToString :: Aluno -> String
alunoToString alunoTo =
      let planoStr = case planoAluno alunoTo of
            Planos.Light -> "Light"
            Planos.Gold -> "Gold"
            Planos.Premium -> "Premium"
      in intercalate ";" [ matricula alunoTo
                        , show (alunoId alunoTo)
                        , nomeAluno alunoTo
                        , cpfAluno alunoTo
                        , endereçoAluno alunoTo
                        , contatoAluno alunoTo
                        , planoStr
                        , show (treinos alunoTo)
                        , show (emDia alunoTo)
                        , senhaAluno alunoTo
                        , emailAluno alunoTo
                        , show (aulas alunoTo)
                        ] ++ "\n"
      
-- vai pro controller
obterPlanoParaAluno :: Aluno -> Plano
obterPlanoParaAluno aluno = case planoAluno aluno of
      Light -> planoLight
      Gold -> planoGold
      Premium -> planoPremium  
      
-- Funçao  para enviar um Email de confirmação do pagamento para a academia (Substituir 'joao.pedro.arruda.silva@ccc' pelo email que irá receber a mensagem (monitor/Professor))
enviarEmail :: Aluno->String-> IO ()
enviarEmail aluno opçao= do
    let planoAtual = obterPlanoParaAluno aluno
        nomeCaps = map toUpper (nomeAluno aluno)
     -- Configurar as informações do servidor SMTP
    let from       = Address Nothing (T.pack (emailAluno aluno))
        to         = [Address (Just (T.pack "Codefit")) (T.pack "joao.pedro.arruda.silva@ccc.ufcg.edu.br")] --colocar o email aqui (monitor/professor) para correção, envia a mensagem para o email especificado acima
        cc         = []
        bcc        = []
        subject    = T.pack "Pagamento de Mensalidade"
        body       = plainTextPart (TL.pack (nomeCaps++" EFETUOU O PAGAMENTO DO MÊS\n\n R$"++ show (valorMensal planoAtual)++"\n FORMA DE PAGAMENTO: "++ opçao++ "\n MATRÍCULA: "++ show(matricula aluno)))
    let mail = simpleMail from to cc bcc subject [body]
    sendMailWithLoginSTARTTLS "smtp.gmail.com" "alunocodefit@gmail.com" "dhvz rvdb bhsv goqu" mail -- email do qual será enviado a mensagem

-- função que recebe uma lista de String com os dados dos alunos (vindos do Txt) e exibe-os
exibeAlunos:: [String]-> IO()
exibeAlunos []= putStrLn "Não há nenhum Aluno cadastrado. "
exibeAlunos matriculas = do
  alunos <-mapM recuperaAlunoMatricula matriculas
  mapM_ exibeAluno alunos

exibeAluno :: Aluno -> IO ()
exibeAluno aluno = putStrLn(showAlunoLista aluno++"\n")


readAula :: String -> Aula
readAula str = let (nomeAula, rest1) = break (== ',') str
                   (horario, planosStr) = break (== ',') (tail rest1)
                   planosAceitos = readPlanos (tail planosStr)
                  in Aula nomeAula horario planosAceitos

readPlanos :: String -> [PlanoTipo]
readPlanos str = map readPlanosTipo (splitOn "," str)

--Função que lê uma String e define do tipo de plano para as instâncias
readPlanosTipo :: String -> PlanoTipo
readPlanosTipo "Light" = Light
readPlanosTipo "Gold" = Gold
readPlanosTipo "Premium" = Premium
readPlanosTipo _ = error "Tipo de plano desconhecido"


trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace


readPlano :: String -> PlanoAluno
readPlano "Light" = Light
readPlano "Gold" = Gold
readPlano "Premium" = Premium
readPlano _ = error "Plano desconhecido"


--funçao que retorna a posição da matricula na lista de matrículas recebida
posicaoMatLista :: String -> [String] -> Int
posicaoMatLista matricula lista =
    let posUltimo = length lista - 1
    in case elemIndex matricula lista of
        Just pos -> pos
        Nothing -> error "Matrícula não encontrada na lista"


--Função que recebe uma matrícula e retorna os dados da linha que contem a matrícula
filtrarMatricula :: String -> [String] -> [String]
filtrarMatricula matriculaStr listaG = do
    let listaP = primeirosElementos listaG
        posicao = posicaoMatLista matriculaStr listaP
    --sabendo que a posicao da listaP e a mesma da listaG, com os mesmos valores
    splitOn ";" (listaG !! posicao)                        