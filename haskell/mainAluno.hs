
module MainAluno where
import AlunoController
import Aluno
import Debug.Trace
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
import AvaliacaoFisica



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
                    
recuperarAulas :: String -> [Aula]
recuperarAulas conteudo = map parseLinha (lines conteudo)

parseLinha :: String -> Aula
parseLinha linha =  
      let elems = splitOn ";" linha
          planosStr = read (elems !! 2) :: [String]
          nome = elems !! 0
          horario = elems !! 1
      in Aula {nomeAula=nome, horarioAula= horario, planosPermitidos= parsePlanos planosStr  }
      
            

parsePlanos :: [String] -> [PlanoTipo]
parsePlanos planosStrs = map readPlanosTipo (concatMap removeBrackets planosStrs)
      where removeBrackets str = words (filter (\c -> c /= '[' && c /= ']') str)



exibeAulas :: [Aula] -> IO()
exibeAulas [] =  putStrLn "Não há aulas cadastradas."
exibeAulas aulas = mapM_ exibeAula aulas

exibeAula :: Aula -> IO ()
exibeAula aula = do
      putStrLn(showAula aula++"\n\n")

-- Funçao que inicia a pagina manuseada pelo aluno, necessitando da matricula e senha para acessa
existeMatricula :: String -> String -> Bool
existeMatricula strMat conteudo = strMat `elem` primeirosElementos (lines conteudo)

exibeAlunos:: [String]-> IO()
exibeAlunos []= putStrLn "Não há nenhum Aluno cadastrado. "
exibeAlunos matriculas = do
  alunos <-mapM recuperaAlunoMatricula matriculas
  mapM_ exibeAluno alunos

exibeAluno :: Aluno -> IO ()
exibeAluno aluno = putStrLn(showAlunoLista aluno++"\n")


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


loginAluno::IO()-> IO()
loginAluno menuPrincipal= do
      limparTerminal  -- funçao que limpa o terminal
      putStrLn "══════════════════════LOGIN/ALUNO═══════════════════════ "
      putStrLn "Digite \"!\" para voltar."
      putStr   "\n> Matrícula: "
      hFlush stdout
      matriculaAluno<- getLine
      case matriculaAluno of
            "!"-> menuPrincipal
            _ -> do
                  conexao <- openFile "haskell/Aluno.txt" ReadMode -- leitura do arquivo
                  conteudo<- hGetContents conexao -- atribuiçao do conteudo
                  seq conteudo $ return () -- força a leitura (lidando com a avaliação preguiçosa de haskell)
                  let matriculaExiste=existeMatricula matriculaAluno conteudo -- variavel booleana
                  seq matriculaExiste $ return () -- força a leitura
                  hClose conexao 
                  if not matriculaExiste
                        then do 
                              exibirMensagemTemporaria "\n!! Matricula não encontrada. Tente novamente... !!"
                              loginAluno menuPrincipal
                  else do
                        alunoEncontrado<- recuperaAlunoMatricula matriculaAluno
                        if not(matriculaAluno == matricula alunoEncontrado)
                              then do
                                    limparTerminal
                                    exibirMensagemTemporaria "\n!! Matricula não encontrada. Tente novamente... !!"
                                    loginAluno menuPrincipal
                        else do  
                              putStr "> Senha: " 
                              hFlush stdout
                        senhaAluno1<- getLine
                        if not(senhaAluno1 == senhaAluno alunoEncontrado )
                              then do
                                    limparTerminal
                                    exibirMensagemTemporaria "\n!! Senha incorreta. Tente novamente... !!"
                                    loginAluno menuPrincipal
                        else do
                              putStrLn "\nCarregando..."
                              threadDelay (2 * 1000000)
                        homeAluno alunoEncontrado menuPrincipal

homeAluno:: Aluno->IO()->IO()
homeAluno alunoAtual menuPrincipal= do 
      clearScreen
      setCursorPosition 1 0
      putStrLn "╔═════════════════════════════════════════╗"
      putStrLn ("║              Olá "++ primeiroNome(nomeAluno alunoAtual)++" !")
      putStrLn "║                                         ║"
      putStrLn "║   [1] Consultar Plano atual             ║"
      putStrLn "║   [2] Alterar Plano                     ║"
      putStrLn "║   [3] Meus Dados                        ║"
      putStrLn "║   [4] Aulas Coletivas                   ║"
      putStrLn "║   [5] Treinos                           ║"
      putStrLn "║   [6] Realizar Pagamento                ║"
      putStrLn "║   [7] Consultar Avaliação física        ║"
      putStrLn "║   [8] Sair                              ║"
      putStrLn "║                                         ║"
      putStrLn "║   > Digite a opção:                     ║ "
      putStrLn "╚═════════════════════════════════════════╝"
      opçao<- getLine
      case opçao of
            "1"-> exibePlano (alunoAtual) menuPrincipal
            "2"-> alterarPlano alunoAtual menuPrincipal
            "3"-> meusDados alunoAtual menuPrincipal
            "4"-> aulasColetivas alunoAtual menuPrincipal
            "5"-> menuTreinos alunoAtual menuPrincipal
            "6"-> realizaPagamento alunoAtual menuPrincipal
            "7"-> calculoImc alunoAtual menuPrincipal
            "8"-> funçaoSaida menuPrincipal 
            _ -> do
                  putStr "Opção inválida!!"
                  homeAluno alunoAtual menuPrincipal

calculoImc:: Aluno -> IO()-> IO()
calculoImc aluno menuPrincipal = do
      limparTerminal
      putStrLn ("═════════════════ Avaliação Física ════════════════════\n")
      conexao<- openFile "haskell/avaliacoes_fisicas.txt" ReadMode
      conteudo<- hGetContents conexao
      let linhas = lines conteudo
      existeAvaliacao<- verificaAvaliacao linhas (matricula aluno)
      if not(existeAvaliacao)
            then do
                  putStrLn "\n Você ainda não realizou nenhuma avaliação física..."
                  putStrLn "\n\n [0] Voltar"
                  opçao<- getLine
                  case opçao of
                        "0" -> homeAluno aluno menuPrincipal
                        _ -> calculoImc aluno menuPrincipal
            else do
                  avaliacaoEncontrada<- recuperaAvaliacaoAluno linhas (matricula aluno)
                  exibeAvaliacao avaliacaoEncontrada 
                  putStrLn "\n\n [0] Voltar"
                  opçao<- getLine
                  case opçao of
                        "0" -> homeAluno aluno menuPrincipal
                        _ -> calculoImc aluno menuPrincipal

exibeAvaliacao::  AvaliacaoFisica-> IO()
exibeAvaliacao avaliacao = do
      putStrLn $  "\nData da avaliação: " ++  (dataAvaliacao avaliacao) ++
                  "\nPeso: " ++ (show(peso avaliacao)) ++
                  "\nAltura: " ++  (show(altura avaliacao)) ++
                  "\nIdade: " ++(show(idade avaliacao)) ++
                  "\nObjetivo: " ++ (objetivo avaliacao)

recuperaAvaliacaoAluno:: [String] -> String -> IO AvaliacaoFisica
recuperaAvaliacaoAluno (x:xs) matAluno= do
      let atual = splitOn "," x
      if matAluno `elem` atual
            then return (parseAvaliacao atual)
            else do
            recuperaAvaliacaoAluno xs matAluno

parseAvaliacao :: [String] -> AvaliacaoFisica
parseAvaliacao [id, dataAv, peso, altura, idade, objetivo, matricula] =
      AvaliacaoFisica (read id) dataAv (read peso) (read altura) (read idade) objetivo matricula
parseAvaliacao _ = error "Formato de avaliação inválido"

verificaAvaliacao:: [String]->String-> IO Bool
verificaAvaliacao [] _= return False
verificaAvaliacao (x:xs) matAluno=do 
      let atual = splitOn "," x
      if matAluno `elem` atual
            then return True
      else do
            verificaAvaliacao xs matAluno

funçaoSaida:: IO() -> IO()
funçaoSaida menuPrincipal= do
      limparTerminal
      putStrLn "\n SAINDO..."
      threadDelay (1 * 1000000)
      menuPrincipal
      
      
menuTreinos:: Aluno-> IO()-> IO()
menuTreinos aluno menuPrincipal= do
      limparTerminal   
      putStrLn ("════════════════ Meus Treinos ════════════════\n")
      if (length (treinos aluno))==0
            then do
                  putStrLn "\n > Você ainda não possui treinos cadastrados !\n\n [0] Voltar    [1] Solicitar Treino\n"
                  opçao1<- getLine
                  case opçao1 of
                        "0"-> homeAluno aluno menuPrincipal
                        "1"-> do
                              putStr "\n> Informe o tipo de treino  desejado (Ex: peito, perna...):"
                              hFlush stdout
                              tipoDesejado<- getLine
                              solicitaTreino aluno tipoDesejado
                              putStrLn ("\x1b[32m" ++" TREINO SOLICITADO COM SUCESSO !\x1b[0m")
                              threadDelay (2 * 1000000)
                              menuTreinos aluno menuPrincipal
                        _  -> menuTreinos aluno menuPrincipal
      else do
            putStrLn (exibeTreinosAluno (treinos aluno))
            putStrLn " [0] Voltar    [1] Solicitar Treino\n"
            opçao<- getLine
            case opçao of
                  "0"-> homeAluno aluno menuPrincipal
                  "1"-> do
                        putStr "\n> Informe o tipo de treino  desejado (Ex: peito, perna...):"
                        hFlush stdout
                        tipoDesejado<- getLine
                        solicitaTreino aluno tipoDesejado
                        putStrLn ("\x1b[32m" ++" TREINO SOLICITADO COM SUCESSO !\x1b[0m")
                        threadDelay (2 * 1000000)
                        menuTreinos aluno menuPrincipal
                  _  -> menuTreinos aluno menuPrincipal
                                

solicitaTreino:: Aluno-> String-> IO()
solicitaTreino aluno tipoTreino= do
      appendFile "haskell//solicitacoes.txt" (matricula aluno++"," ++tipoTreino ++"\n")

aulasColetivas:: Aluno-> IO()->IO()
aulasColetivas aluno menuPrincipal= do
      limparTerminal
      putStrLn (" ═════════════════ Aulas Coletivas ════════════════\n")
      conexao<- openFile "haskell/aulas.txt" ReadMode
      conteudo<- hGetContents conexao
      let aulas= recuperarAulas conteudo
      exibeAulas aulas
      putStrLn "\n\n [0] Voltar     [1] Inscrever-se     [2] Minhas aulas"
      opçao<- getLine
      hClose conexao
      case opçao of
            "0"-> homeAluno aluno menuPrincipal
            "1"-> inscriçaoAula aluno menuPrincipal
            "2"-> listarAulas aluno menuPrincipal
            _ -> do
                  putStrLn "Opção inválida !!"
                  aulasColetivas aluno menuPrincipal

listarAulas:: Aluno->IO()->IO()
listarAulas aluno menuPrincipal= do
      limparTerminal
      putStrLn "══════════════════ MINHAS AULAS ══════════════════\n"
      
      mapM_ exibeAula (aulas aluno)
      putStrLn "\n [0] Voltar        [1] Excluir Aula"
      saida<- getLine
      case saida of
            "0"->aulasColetivas aluno menuPrincipal 
            "1"-> do
                  putStrLn "\n > Nome da aula a ser excluída: "
                  nomeExcluir<- getLine
                  if estaInscrito aluno nomeExcluir
                        then do
                              alunoAtualizado<-removerAula aluno nomeExcluir
                              putStrLn "\n AULA EXCLUÍDA COM SUCESSO !"
                              threadDelay (2 * 1000000)
                              listarAulas alunoAtualizado menuPrincipal
                  else do
                        putStrLn "\n !! Aula inexistente em sua agenda !!"
                        threadDelay (2 * 1000000)
                        listarAulas aluno menuPrincipal
            _ -> listarAulas aluno menuPrincipal 
            
removerAula :: Aluno -> String -> IO Aluno
removerAula aluno nomeAulaEx =do
      let nomeAulaExCaps = map toUpper nomeAulaEx
          novoAluno = aluno { aulas = filter (\aula -> nomeAulaExCaps /= nomeAula aula) (aulas aluno) }
      substituirAlunoTxt novoAluno (matricula novoAluno)
      return novoAluno
      
inscriçaoAula:: Aluno->IO()->IO()
inscriçaoAula aluno menuPrincipal= do
      conexao<- openFile "haskell/aulas.txt" ReadMode
      conteudo<- hGetContents conexao
      
      let aulas= recuperarAulas conteudo
      if not(emDia aluno)
            then do
                  putStrLn "\n > Não é possível realizar inscrições com a mensalidade pendente. Efetue o pagamento..."
                  threadDelay (3 * 1000000)
                  aulasColetivas aluno menuPrincipal
            else do
                  putStr "\n > Nome da aula: "
                  hFlush stdout
                  aulaInsc<- getLine
                  let aulaInscMaiuscula = map toUpper aulaInsc
                  if  existeAula aulaInscMaiuscula aulas
                        then do 
                              if  aulaInscMaiuscula == nomeAula(verificaAula aulaInscMaiuscula aulas)
                                    then if planoIncluido aluno (verificaAula aulaInsc aulas)
                                          then if (estaInscrito aluno aulaInsc)
                                                then do
                                                      putStrLn " - Você já está Inscrito nessa aula !\n\n>Pressione ENTER para voltar"
                                                      hClose conexao
                                                      voltar2<-getLine
                                                      homeAluno aluno menuPrincipal
                                          else do        
                                                putStrLn ("\x1b[32m" ++aulaInsc++ " adicionada na sua agenda de aulas." ++ "\x1b[0m")
                                                threadDelay (2 * 1000000)
                                                adicionaAulaAluno (verificaAula aulaInsc aulas) aluno menuPrincipal
                                                hClose conexao 
                                                menuPrincipal
                                    else do
                                    
                                          putStrLn " - O seu Plano não permite a inscrição nesta aula\n   Vá ate a pagina 'Alterar plano' e adquira um plano compatível\n\n >Pressione ENTER para voltar..."
                                          voltar<- getLine
                                          hClose conexao
                                          homeAluno aluno menuPrincipal
                              else do
                                    putStrLn"\n Aula não encontrada !"
                                    limparTerminal
                                    hClose conexao
                                    aulasColetivas aluno menuPrincipal
                        else do     
                              putStrLn"\n Aula não encontrada !"
                              threadDelay (2 * 1000000)
                              limparTerminal
                              hClose conexao
                              aulasColetivas aluno menuPrincipal        


estaInscrito :: Aluno -> String -> Bool
estaInscrito aluno nomeAulaStr = any (\aula -> nomeAula aula == nomeAulaStrCaps) (aulas aluno)
      where nomeAulaStrCaps = map toUpper nomeAulaStr

planoIncluido :: Aluno -> Aula -> Bool
planoIncluido aluno aula = planoAluno aluno `elem` planosPermitidos aula

adicionaAulaAluno:: Aula-> Aluno-> IO()->IO()
adicionaAulaAluno aula aluno menuPrincipal=do
      let alunoAtualizado = aluno { aulas = aula : aulas aluno }
      putStrLn " * Inscrição realizada com sucesso *"
      substituirAlunoTxt alunoAtualizado (matricula alunoAtualizado)
      aulasColetivas alunoAtualizado menuPrincipal


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

meusDados:: Aluno-> IO()->IO() 
meusDados aluno menuPrincipal= do
      limparTerminal
      putStrLn ("════════════════ "++nomeAluno aluno++ " ════════════════\n")
      putStr (exibirAluno aluno)
      putStrLn "\n\n>  [0] Voltar ao menu     [1] Editar dados "
      opçao <- getLine
      case opçao of
            "0"-> homeAluno aluno menuPrincipal
            "1"-> editDados aluno menuPrincipal
            _ -> meusDados aluno menuPrincipal
      
      
editDados:: Aluno-> IO()->IO()
editDados aluno menuPrincipal= do
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
                  let alunoNovo = aluno{ nomeAluno= novoNome}
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Nome atualizado com sucesso*"
                  homeAluno alunoNovo menuPrincipal
                  
            "2" -> do 
                  putStrLn " -> Novo endereço: "
                  novoEndereco <- getLine
                  let alunoNovo = aluno { endereçoAluno = novoEndereco }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Endereço atualizado com sucesso*"
                  homeAluno alunoNovo menuPrincipal
            "3" -> do 
                  putStrLn " -> Novo contato: "
                  novoContato <- getLine
                  let alunoNovo = aluno { contatoAluno = novoContato }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Contato atualizado com sucesso*"
                  homeAluno alunoNovo menuPrincipal
            "4" -> do 
                  putStrLn " -> Nova senha: "
                  novaSenha <- getLine
                  let alunoNovo = aluno { senhaAluno = novaSenha }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Senha atualizada com sucesso*"
                  homeAluno alunoNovo menuPrincipal
            "5" -> do 
                  putStrLn " -> Novo email: "
                  novoEmail <- getLine
                  let alunoNovo = aluno { emailAluno = novoEmail }
                  substituirAlunoTxt alunoNovo (matricula alunoNovo)
                  exibirMensagemTemporaria " *Email atualizada com sucesso*"
                  homeAluno alunoNovo menuPrincipal
            _ -> do
                  putStrLn "Opção inválida!"
                  exibirMensagemTemporaria " *Opção inválida*"
                  editDados aluno menuPrincipal
 

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
        nomeCaps = map toUpper (nomeAluno aluno)
     -- Configurar as informações do servidor SMTP
    let from       = Address Nothing (T.pack (emailAluno aluno))
        to         = [Address (Just (T.pack "Codefit")) (T.pack "joao.pedro.arruda.silva@ccc.ufcg.edu.br")] --colocar o email aqui (monitor/professor) para correção, envia a mensagem para o email especificado acima
        cc         = []
        bcc        = []
        subject    = T.pack "Pagamento de Mensalidade"
        body       = plainTextPart (TL.pack (nomeCaps++" EFETUOU O PAGAMENTO DO MÊS\n\n R$"++ show (valorMensal planoAtual)++"\n FORMA DE PAGAMENTO: "++ opçao++ "\n MATRÍCULA: "++ show(matricula aluno)))
    let mail = simpleMail from to cc bcc subject [body]
    sendMailWithLoginSTARTTLS "smtp.gmail.com" "alunocodefit@gmail.com" "dhvz rvdb bhsv goqu" mail



realizaPagamento:: Aluno->IO()-> IO()
realizaPagamento aluno menuPrincipal= do
      limparTerminal
      putStrLn "════════════════ FINANCEIRO ════════════════\n"
      if emDia aluno
            then do 
                  putStrLn " - Sua mensalidade está em dia !" 
                  putStrLn "\n>Pressione ENTER para voltar..."
                  _ <- getLine -- Aguarda o Enter
                  homeAluno aluno menuPrincipal
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
                        realizaPagamento aluno menuPrincipal
                  else do      
                        exibirMensagemTemporaria "\n *Processando pagamento...*"
                        let alunoNovo = aluno{emDia = True}
                        substituirAlunoTxt alunoNovo (matricula alunoNovo)
                        putStrLn $ "\x1b[32m" ++ " Pagamento Realizado." ++ "\x1b[0m"
                        threadDelay (2 * 1000000)
                        adicionaPagamento (valorMensal planoAtual) (show planoAtual)
                        enviarEmail aluno opçaoPagamento
                        homeAluno alunoNovo menuPrincipal

adicionaPagamento:: Float-> String-> IO()
adicionaPagamento valor plano = do
      appendFile "haskell//pagamentos.txt" (show(valor)++"," ++plano ++"\n")
      
alterarPlano:: Aluno->IO()-> IO()
alterarPlano aluno menuPrincipal= do 
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
      homeAluno alunoNovo menuPrincipal


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
exibePlano:: Aluno->IO()-> IO()
exibePlano aluno menuPrincipal= do 
      let plano =detalhesPlano(planoAluno aluno)
      clearScreen
      setCursorPosition 0 0
      putStrLn plano
      putStrLn "\n>Pressione ENTER para voltar..."
      confirma <- getChar
      when (confirma /= '\n') $ void getChar -- Aguarda o Enter
      homeAluno aluno menuPrincipal

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
