{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import AlunoController
import Aluno
import System.Console.ANSI
import Planos
import Control.Monad
import System.IO
import Control.Concurrent (threadDelay)
import Network.Mail.SMTP (sendMail) 
import Network.Mail.SMTP.Auth 
import Network.Mail.SMTP.Types ()
import Network.Mail.SMTP 
import qualified Network.Mail.Mime (Mail(..), Address(..), simpleMail) 
import Data.Text(pack)
import Data.Char(toUpper, isSpace)
import Aula
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Mail.SMTP.Types (Address(..))
import Data.List (elemIndex,intercalate)
import Treino
import Data.List.Split(splitOn)
import System.Directory (removeFile, renameFile)
import qualified Network.HTTP.Conduit as HTTP
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as Simple


defineAulas :: [Aula]
defineAulas = [aulaDança, aulaBoxe]
  where
    aulaDança = Aula { nomeAula = "DANÇA COLETIVA",
                       horarioAula = "Segunda e quarta, das 18:00 às 19:30",
                       planosPermitidos = [Light, Gold, Premium]
                     }
    aulaBoxe = Aula { nomeAula = "BOXE",
                      horarioAula = "Terça e Quinta, das 20:00 às 21:30",
                      planosPermitidos = [Premium]
                    }


exibeAulas :: [Aula] -> IO()
exibeAulas [] =  putStrLn "Não há aulas cadastradas."
exibeAulas aulas = mapM_ exibeAula aulas

exibeAula :: Aula -> IO ()
exibeAula aula = do
      putStrLn(showAula aula++"\n\n")

main :: IO ()
main = do 
      loginAluno
-- Funçao que inicia a pagina manuseada pelo aluno, necessitando da matricula e senha para acessa
existeMatricula :: String -> String -> Bool
existeMatricula strMat conteudo = strMat `elem` primeirosElementos (lines conteudo)

-- Vai pro controller
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
                              , nome = dadosAluno !! 2
                              , cpf = dadosAluno !! 3
                              , endereço = dadosAluno !! 4
                              , contato = dadosAluno !! 5
                              , planoAluno = readPlano (dadosAluno !! 6)
                              , treinos = treinos
                              , emDia = read (dadosAluno !! 8)
                              , senha = dadosAluno !! 9
                              , email = dadosAluno !! 10
                              , aulas = aulas
                              }
      
      hClose conexao 
      return aluno  

readAula :: String -> Aula
readAula str = let (nomeAula, rest1) = break (== ',') str
                   (horario, planosStr) = break (== ',') (tail rest1)
                   planosAceitos = readPlanos (tail planosStr)
                  in Aula nomeAula horario planosAceitos

readPlanos :: String -> [PlanoTipo]
readPlanos str = map readPlanosTipo (splitOn "," str)

readPlanosTipo :: String -> PlanoTipo
readPlanosTipo "Light" = Light
readPlanosTipo "Gold" = Gold
readPlanosTipo "Premium" = Premium
readPlanosTipo _ = error "Tipo de plano desconhecido"

parseTreinos :: String -> [(TipoTreino, Exercicios)]
parseTreinos treinosStr = map parseTreino (splitOn ";" treinosStr)

parseTreino :: String -> (TipoTreino, Exercicios)
parseTreino treinoStr =
      let (tipoTreino:exerciciosStr) = splitOn ":" treinoStr
      in (tipoTreino, map trim exerciciosStr)

trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace


readPlano :: String -> PlanoAluno
readPlano "Light" = Light
readPlano "Gold" = Gold
readPlano "Premium" = Premium
readPlano _ = error "Plano desconhecido"


-- Vai pro controller
posicaoMatLista :: String -> [String] -> Int
posicaoMatLista matricula lista =
    let posUltimo = length lista - 1
    in case elemIndex matricula lista of
        Just pos -> pos
        Nothing -> error "Matrícula não encontrada na lista"


-- vai pro controllrr
filtrarMatricula :: String -> [String] -> [String]
filtrarMatricula matriculaStr listaG = do
    let listaP = primeirosElementos listaG
        posicao = posicaoMatLista matriculaStr listaP
    --sabendo que a posicao da listaP e a mesma da listaG, com os mesmos valores
    splitOn ";" (listaG !! posicao)


loginAluno::IO()
loginAluno= do
      limparTerminal  -- funçao que limpa o terminal
      putStrLn "   ==== LOGIN/ALUNO ==== "
      putStr   "> Matrícula: "
      hFlush stdout
      matriculaAluno<- getLine
      conexao <- openFile "haskell/Aluno.txt" ReadMode -- leitura do arquivo
      conteudo<- hGetContents conexao -- atribuiçao do conteudo
      seq conteudo $ return () -- força a leitura (lidando com a avaliação preguiçosa de haskell)
      let matriculaExiste=existeMatricula matriculaAluno conteudo -- variavel booleana
      seq matriculaExiste $ return () -- força a leitura
      hClose conexao 
      if not matriculaExiste
            then do exibirMensagemTemporaria "\n!! Matricula não encontrada. Tente novamente... !!"
                    loginAluno
      else do
            alunoEncontrado<- recuperaAlunoMatricula matriculaAluno
            if not(matriculaAluno == matricula alunoEncontrado)
                  then do
                        limparTerminal
                        exibirMensagemTemporaria "\n!! Matricula não encontrada. Tente novamente... !!"
                        loginAluno 
            else do  
                  putStr "> Senha: " 
                  hFlush stdout
            senhaAluno<- getLine
            if not(senhaAluno == senha alunoEncontrado )
                  then do
                        limparTerminal
                        exibirMensagemTemporaria "\n!! Senha incorreta. Tente novamente... !!"
                        loginAluno
            else do
                  putStrLn "\nCarregando..."
                  threadDelay (2 * 1000000)
            homeAluno alunoEncontrado

homeAluno:: Aluno-> IO()
homeAluno alunoAtual = do 
      clearScreen
      setCursorPosition 1 0
      putStrLn "╔═════════════════════════════════════════╗"
      putStrLn ("║              Olá "++ primeiroNome(nome alunoAtual)++" !")
      putStrLn "║                                         ║"
      putStrLn "║   [1] Consultar Plano atual             ║"
      putStrLn "║   [2] Alterar Plano                     ║"
      putStrLn "║   [3] Meus Dados                        ║"
      putStrLn "║   [4] Aulas Coletivas                   ║"
      putStrLn "║   [5] Treinos                           ║"
      putStrLn "║   [6] Realizar Pagamento                ║"
      putStrLn "║   [7] Sair                              ║"
      putStrLn "║                                         ║"
      putStrLn "║   > Digite a opção:                     ║ "
      putStrLn "╚═════════════════════════════════════════╝"
      opçao<- getLine
      case opçao of
            "1"-> exibePlano (alunoAtual)
            "2"-> alterarPlano alunoAtual
            "3"-> meusDados alunoAtual
            "4"-> aulasColetivas alunoAtual
            "5"-> menuTreinos alunoAtual
            "6"-> realizaPagamento alunoAtual
            "7"-> funçaoSaida
            _ -> do
                  putStr "Opção inválida!!"
                  homeAluno alunoAtual


funçaoSaida:: IO()
funçaoSaida = do
      limparTerminal
      putStrLn "\n SAINDO..."
      threadDelay (1 * 1000000)
      main


menuTreinos:: Aluno-> IO()
menuTreinos aluno= do
      limparTerminal   
      putStrLn ("        ======= Meus Treinos =======\n")
      if (length (treinos aluno))==0
            then do
                  putStrLn "\n > Você ainda não possui treinos cadastrados !\n\n [0] Voltar\n"
                  opçao<- getLine
                  case opçao of
                        "0"-> homeAluno aluno
                        _  -> menuTreinos aluno
      else do
            putStrLn (exibeTreinosAluno (treinos aluno))
            putStrLn " [0] Voltar\n"
            opçao<- getLine
            case opçao of
                  "0"-> homeAluno aluno
                  _  -> menuTreinos aluno
                                

aulasColetivas:: Aluno-> IO()
aulasColetivas aluno= do
      limparTerminal
      putStrLn ("        ======= Aulas Coletivas =======\n")
      exibeAulas defineAulas
      putStrLn "\n\n [0] Voltar     [1] Inscrever-se     [2] Minhas aulas"
      opçao<- getLine
      case opçao of
            "0"-> homeAluno aluno
            "1"-> inscriçaoAula aluno
            "2"-> listarAulas aluno
            _ -> do
                  putStrLn "Opção inválida !!"
                  aulasColetivas aluno

listarAulas:: Aluno->IO()
listarAulas aluno= do
      limparTerminal
      putStrLn "    ===== MINHAS AULAS ====\n"
      
      mapM_ exibeAula (aulas aluno)
      putStrLn "\nPressione ENTER para voltar..."
      saida<- getLine
      aulasColetivas aluno  
                  
inscriçaoAula:: Aluno->IO()
inscriçaoAula aluno= do
      if not(emDia aluno)
            then do
                  putStrLn "\n > Não é possível realizar inscrições com a mensalidade pendente. Efetue o pagamento..."
                  threadDelay (3 * 1000000)
                  aulasColetivas aluno
            else do
                  putStr "\n > Nome da aula: "
                  hFlush stdout
                  aulaInsc<- getLine
                  let aulaInscMaiuscula = map toUpper aulaInsc
                  if  existeAula aulaInscMaiuscula defineAulas
                        then do 
                              if  aulaInscMaiuscula == nomeAula(verificaAula aulaInscMaiuscula defineAulas)
                                    then if planoIncluido aluno (verificaAula aulaInsc defineAulas)
                                          then if (estaInscrito aluno aulaInsc)
                                                then do
                                                      putStrLn " - Você já está Inscrito nessa aula !\n\n>Pressione ENTER para voltar"
                                                      voltar2<-getLine
                                                      homeAluno aluno 
                                          else do        
                                                putStrLn ("\x1b[32m" ++aulaInsc++ " adicionada na sua agenda de aulas." ++ "\x1b[0m")
                                                threadDelay (2 * 1000000)
                                                adicionaAulaAluno (verificaAula aulaInsc defineAulas) aluno
                                    else do
                                    
                                          putStrLn " - O seu Plano não permite a inscrição nesta aula\n   Vá ate a pagina 'Alterar plano' e adquira um plano compatível\n\n >Pressione ENTER para voltar..."
                                          voltar<- getLine
                                          homeAluno aluno
                              else do
                                    putStrLn"\n Aula não encontrada !"
                                    limparTerminal
                                    aulasColetivas aluno
                        else do     
                              putStrLn"\n Aula não encontrada !"
                              threadDelay (2 * 1000000)
                              limparTerminal
                              aulasColetivas aluno        


estaInscrito :: Aluno -> String -> Bool
estaInscrito aluno nomeAulaStr = any (\aula -> nomeAula aula == nomeAulaStrCaps) (aulas aluno)
      where nomeAulaStrCaps = map toUpper nomeAulaStr

planoIncluido :: Aluno -> Aula -> Bool
planoIncluido aluno aula = planoAluno aluno `elem` planosPermitidos aula

adicionaAulaAluno:: Aula-> Aluno-> IO()
adicionaAulaAluno aula aluno=do
      let alunoAtualizado = aluno { aulas = aula : aulas aluno }
      putStrLn " * Inscrição realizada com sucesso *"
      substituirAlunoTxt alunoAtualizado (matricula alunoAtualizado)
      homeAluno alunoAtualizado


existeAula:: String-> [Aula]-> Bool
existeAula _ [] = False
existeAula nomeAulaStr (aula:outrasAulas)=
      if nomeAula aula == nomeAulaStr
            then True
            else existeAula nomeAulaStr outrasAulas

verificaAula :: String -> [Aula] -> Aula
verificaAula nomeAulaStr (aula:outrasAulas) = 
      if map toUpper(nomeAula aula) == map toUpper nomeAulaStr
            then aula
            else verificaAula nomeAulaStr outrasAulas

meusDados:: Aluno-> IO() 
meusDados aluno= do
      limparTerminal
      putStrLn ("   ======= "++nome aluno++ " =======\n")
      putStr (exibirAluno aluno)
      putStrLn "\n\n>  [0] Voltar ao menu     [1] Editar dados "
      opçao <- getLine
      case opçao of
            "0"-> homeAluno aluno
            "1"-> editDados aluno
            _ -> meusDados aluno
      
      
editDados:: Aluno-> IO()
editDados aluno = do
      limparTerminal
      putStrLn "> Dado para edição:"
      putStrLn "\n1. Nome"
      putStrLn "2. Endereço"
      putStrLn "3. Contato"
      putStrLn "4. Senha do app"
      putStrLn "5. Email\n>"
      opçao <- getLine
      case opçao of
            "1" -> do 
                  putStrLn " -> Novo nome: "
                  novoNome<- getLine
                  let alunoNovo = aluno{ nome= novoNome}
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Nome atualizado com sucesso*"
                  homeAluno alunoNovo
                  
            "2" -> do 
                  putStrLn " -> Novo endereço: "
                  novoEndereco <- getLine
                  let alunoNovo = aluno { endereço = novoEndereco }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Endereço atualizado com sucesso*"
                  homeAluno alunoNovo
            "3" -> do 
                  putStrLn " -> Novo contato: "
                  novoContato <- getLine
                  let alunoNovo = aluno { contato = novoContato }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Contato atualizado com sucesso*"
                  homeAluno alunoNovo
            "4" -> do 
                  putStrLn " -> Nova senha: "
                  novaSenha <- getLine
                  let alunoNovo = aluno { senha = novaSenha }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Senha atualizada com sucesso*"
                  homeAluno alunoNovo
            "5" -> do 
                  putStrLn " -> Novo email: "
                  novoEmail <- getLine
                  let alunoNovo = aluno { senha = novoEmail }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Email atualizada com sucesso*"
                  homeAluno alunoNovo
            _ -> do
                  putStrLn "Opção inválida!"
                  exibirMensagemTemporaria " *Opção inválida*"
                  editDados aluno
 

-- vai pro controller
obterPlanoParaAluno :: Aluno -> Plano
obterPlanoParaAluno aluno = case planoAluno aluno of
      Light -> planoLight
      Gold -> planoGold
      Premium -> planoPremium                  

-- Funçao  para enviar um Email de confirmação do pagamento para a academia
enviarEmail :: Aluno->String-> IO ()
enviarEmail aluno opçao= do
    let planoAtual = obterPlanoParaAluno aluno
        nomeCaps = map toUpper (nome aluno)
     -- Configurar as informações do servidor SMTP
    let from       = Address Nothing (T.pack (email aluno))
        to         = [Address (Just (T.pack "Codefit")) (T.pack "joao.pedro.arruda.silva@ccc.ufcg.edu.br")] --colocar o email aqui (monitor/professor) para correção, envia a mensagem para o email especificado acima
        cc         = []
        bcc        = []
        subject    = T.pack "Pagamento de Mensalidade"
        body       = plainTextPart (TL.pack (nomeCaps++" EFETUOU O PAGAMENTO DO MÊS\n\n R$"++ show (valorMensal planoAtual)++"\n FORMA DE PAGAMENTO: "++ opçao++ "\n MATRÍCULA: "++ show(matricula aluno)))
    let mail = simpleMail from to cc bcc subject [body]
    sendMailWithLoginSTARTTLS "smtp.gmail.com" "alunocodefit@gmail.com" "dhvz rvdb bhsv goqu" mail



realizaPagamento:: Aluno-> IO()
realizaPagamento aluno= do
      limparTerminal
      putStrLn "   ===== FINANCEIRO =====\n"
      if emDia aluno
            then do 
                  putStrLn " - Sua mensalidade está em dia !" 
                  putStrLn "\n>Pressione ENTER para voltar..."
                  _ <- getLine -- Aguarda o Enter
                  homeAluno aluno
      else do
            putStrLn " - Mensalidade Pendende."
            let planoAtual = obterPlanoParaAluno aluno
            putStrLn ("\x1b[32m" ++" - R$ "++ show(valorMensal planoAtual)++ "\x1b[0m")
            putStrLn "\n> Forma de pagamento:   \n\n [P] Pix"
            putStrLn " [D] Cartão de debito "
            putStrLn " [C] Cartão de crédito"
            putStr " [B] Boleto\n\n>"
            hFlush stdout
            opçao <- getLine
            let opçaoPagamento = case opçao of
                  "P"-> "Pix"
                  "D"-> "Cartão de Débito"
                  "C"-> "Cartão de Crédito"
                  "B"-> "Boleto"
                  "p"-> "Pix"
                  "d"-> "Cartão de Débito"
                  "c"-> "Cartão de Crédito"
                  "b"-> "Boleto"
                  _ -> "opçao invalida"
            if opçaoPagamento == "opçao invalida"
                  then do
                        putStrLn "> Opçao Inválida !!"
                        threadDelay (2 * 1000000)
                        realizaPagamento aluno
                  else do      
                        exibirMensagemTemporaria "\n *Processando pagamento...*"
                        let alunoNovo = aluno{emDia = True}
                        putStrLn $ "\x1b[32m" ++ " Pagamento Realizado." ++ "\x1b[0m"
                        threadDelay (2 * 1000000)
                        enviarEmail aluno opçaoPagamento
                        homeAluno alunoNovo

alterarPlano:: Aluno-> IO()
alterarPlano aluno= do 
      clearScreen
      setCursorPosition 0 0
      putStrLn "> Planos disponíveis: \n"
      let plano1 = detalhesPlano (Planos.Light)
      let plano2 = detalhesPlano (Planos.Gold)
      let plano3 = detalhesPlano (Planos.Premium)
      putStrLn (plano1++"\n")
      putStrLn (plano2++"\n")
      putStrLn (plano3++"\n")
      putStr "\n>Opçao de plano ([L] Light | [G] Gold | [P] Premium): "
      hFlush stdout
      opçao<- getLine
      let planoEscolhido = case opçao of
            "L" -> Planos.Light
            "l" -> Planos.Light
            "G" -> Planos.Gold
            "g" -> Planos.Gold
            "P" -> Planos.Premium
            "p" -> Planos.Premium
            _   -> planoAluno aluno 
      let alunoNovo= aluno {    planoAluno = planoEscolhido, emDia = False }
      substituirAlunoTxt alunoNovo (matricula alunoNovo)
      putStrLn "\n>Pressione ENTER para confirmar..."
      _ <- getLine -- Aguarda o Enter
      homeAluno alunoNovo


substituirAlunoTxt :: Aluno -> String -> IO ()
substituirAlunoTxt novoAluno matriculaAlvo = do
      
      handle <- openFile "haskell/Aluno.txt" ReadMode
      contents <- hGetContents handle
      (tempName, tempHandle) <- openTempFile "." "temp"
      let linhas = lines contents
          novasLinhas = map (substituirSeMatriculaIgual novoAluno (matricula novoAluno)) linhas
      hPutStr tempHandle (unlines novasLinhas)
      hClose handle
      hClose tempHandle
      removeFile "haskell/Aluno.txt"
      renameFile tempName "haskell/Aluno.txt"
      
substituirSeMatriculaIgual :: Aluno -> String -> String -> String
substituirSeMatriculaIgual novoAluno matriculaAlvo linha
    | head (splitOn ";" linha) == matriculaAlvo = alunoToString novoAluno
    | otherwise = linha


      
      --alterarPlanoAluno aluno
exibePlano:: Aluno-> IO()
exibePlano aluno = do 
      let plano =detalhesPlano(planoAluno aluno)
      clearScreen
      setCursorPosition 0 0
      putStrLn plano
      putStrLn "\n>Pressione ENTER para voltar..."
      confirma <- getChar
      when (confirma /= '\n') $ void getChar -- Aguarda o Enter
      homeAluno aluno

limparTerminal :: IO ()
limparTerminal = do
      clearScreen
      setCursorPosition 0 0


exibirMensagemTemporaria :: String -> IO ()
exibirMensagemTemporaria msg = do
      putStrLn msg
      threadDelay (3 * 1000000)


primeiroNome :: String -> String
primeiroNome nome = head (words nome)
