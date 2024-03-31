
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
--import qualified Network.HTTP.Conduit as HTTP
import Control.Exception
import qualified Data.ByteString.Lazy as BL
--import qualified Network.HTTP.Simple as Simple
import AvaliacaoFisica

-- Funçao que recebe o conteudo das aulas como String e retorna uma lista de Aulas instanciadas
recuperarAulas :: String -> [Aula]
recuperarAulas conteudo = map parseLinha (lines conteudo)

-- Funçao que transforma os dados de uma aula como String e retorna a Aula Definida
parseLinha :: String -> Aula
parseLinha linha =  
      let elems = splitOn ";" linha
          planosStr = read (elems !! 2) :: [String]
          nome = elems !! 0
          horario = elems !! 1
      in Aula {nomeAula=nome, horarioAula= horario, planosPermitidos= parsePlanos planosStr  }
 
-- Funçao que recebe uma lista com os nomes dos Planos e retorna um lista com os Planos definidos
parsePlanos :: [String] -> [PlanoTipo]
parsePlanos planosStrs = map readPlanosTipo (concatMap removeBrackets planosStrs)
      where removeBrackets str = words (filter (\c -> c /= '[' && c /= ']') str)

-- Funçao que recebe um array de aulas e exibe suas representações em Strings
exibeAulas :: [Aula] -> IO()
exibeAulas [] =  putStrLn "Não há aulas cadastradas."
exibeAulas aulas = mapM_ exibeAula aulas

--Funçao que complementa a função acima
exibeAula :: Aula -> IO ()
exibeAula aula = do
      putStrLn(showAula aula++"\n\n")

-- Funçao que inicia a pagina manuseada pelo aluno, necessitando da matricula e senha para acessa
existeMatricula :: String -> String -> Bool
existeMatricula strMat conteudo = strMat `elem` primeirosElementos (lines conteudo)

--Funçao que exibe e realiza o processo de login do aluno, verificando a matrícula e a senha
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

-- FUnçao que exibe o menu Principal do aluno e captura a opção escolhida pelo usuário
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
            "7"-> exibeAvaliacaoFisica alunoAtual menuPrincipal
            "8"-> funçaoSaida menuPrincipal 
            _ -> do
                  putStr "Opção inválida!!"
                  homeAluno alunoAtual menuPrincipal

-- Funçao que, caso ja exista, exibe a avaliaçao física do aluno
exibeAvaliacaoFisica:: Aluno -> IO()-> IO()
exibeAvaliacaoFisica aluno menuPrincipal = do
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
                        _ -> exibeAvaliacaoFisica aluno menuPrincipal
            else do
                  avaliacaoEncontrada<- recuperaAvaliacaoAluno linhas (matricula aluno)
                  exibeAvaliacao avaliacaoEncontrada 
                  putStrLn "\n\n [0] Voltar"
                  opçao<- getLine
                  case opçao of
                        "0" -> homeAluno aluno menuPrincipal
                        _ -> exibeAvaliacaoFisica aluno menuPrincipal

exibeAvaliacao::  AvaliacaoFisica-> IO()
exibeAvaliacao avaliacao = do
      putStrLn $  "\nData da avaliação: " ++  (dataAvaliacao avaliacao) ++
                  "\nPeso: " ++ (show(peso avaliacao)) ++
                  "\nAltura: " ++  (show(altura avaliacao)) ++
                  "\nIdade: " ++(show(idade avaliacao)) ++
                  "\nObjetivo: " ++ (objetivo avaliacao)

-- Funçao que recebe a matrícula do aluno e retorna a avaliaçao física que a possui
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

-- Funçao que sai da conta do aluno e retorna ao menu Inicial do Sistema
funçaoSaida:: IO() -> IO()
funçaoSaida menuPrincipal= do
      limparTerminal
      putStrLn "\n SAINDO..."
      threadDelay (1 * 1000000)
      menuPrincipal
      
--Funçao para manuseio dos trinos | Exibe os treinos existentes e solicita um novo treino para o funcionário da academia
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
                                
-- Funçao que recebe o tipo de treino e adiciona a solicitação do aluno ao Arquivo Txt de solicitações
solicitaTreino:: Aluno-> String-> IO()
solicitaTreino aluno tipoTreino= do
      appendFile "haskell//solicitacoes.txt" (matricula aluno++"," ++tipoTreino ++"\n")

-- Funçao para manuseio das aulas coletivas | Exibe, inscreve e exclui aula
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

-- Função que lista as aulas nas quais o aluno está inscrito
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

-- Função que recebe o nome de uma aula e a remove da lista de aulas do Aluno
removerAula :: Aluno -> String -> IO Aluno
removerAula aluno nomeAulaEx =do
      let nomeAulaExCaps = map toUpper nomeAulaEx
          novoAluno = aluno { aulas = filter (\aula -> nomeAulaExCaps /= nomeAula aula) (aulas aluno) }
      substituirAlunoTxt novoAluno (matricula novoAluno)
      return novoAluno
      
--Função que realiza a inscriçao do aluno em uma aula
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

-- Função booleana para verificar se o aluno está inscrito na aula passada como argumento
estaInscrito :: Aluno -> String -> Bool
estaInscrito aluno nomeAulaStr = any (\aula -> nomeAula aula == nomeAulaStrCaps) (aulas aluno)
      where nomeAulaStrCaps = map toUpper nomeAulaStr

-- Função que verifica se o atual plano do aluno está na lista de planos permitidos da aula
planoIncluido :: Aluno -> Aula -> Bool
planoIncluido aluno aula = planoAluno aluno `elem` planosPermitidos aula

--função que adiciona uma aula à lista de aulas do aluno
adicionaAulaAluno:: Aula-> Aluno-> IO()->IO()
adicionaAulaAluno aula aluno menuPrincipal=do
      let alunoAtualizado = aluno { aulas = aula : aulas aluno }
      putStrLn " * Inscrição realizada com sucesso *"
      substituirAlunoTxt alunoAtualizado (matricula alunoAtualizado)
      aulasColetivas alunoAtualizado menuPrincipal

--Função que recebe o nome de uma aula e verifica se ela existe na lista de aulas
existeAula:: String-> [Aula]-> Bool
existeAula _ [] = False
existeAula nomeAulaStr (aula:outrasAulas)=
      if nomeAula aula == nomeAulaStr
            then True
            else existeAula nomeAulaStr outrasAulas

-- Função que recebe o nome de uma aula e retorna um tipo Aula instanciada
verificaAula :: String -> [Aula] -> Aula
verificaAula nomeAulaStr (aula:outrasAulas) = 
      if map toUpper(nomeAula aula) == map toUpper nomeAulaStr
            then aula
            else verificaAula nomeAulaStr outrasAulas

--Função que exibe e edita os dados do Aluno
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
      
-- Função que manuseia o tipo de dado que será editado e recebe o novo dado    
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
                 


-- Funçao que exibe o débito pendente e realiza o pagamento da mensalidade, por fim envia um email de confirmação para a academia
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

-- Função que adiciona os detalhes do pagamento ao arquivo Txt de pagamentos
adicionaPagamento:: Float-> String-> IO()
adicionaPagamento valor plano = do
      appendFile "haskell//pagamentos.txt" (show(valor)++"," ++plano ++"\n")
      
-- Função que realiza a alteração do tipo de plano do aluno
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

-- Função que recebe um aluno e substitui os seus dados no arquivo Txt
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

-- Função que, caso a matrícula da linha seja igual a matricula requisitada para alteração, substitui a linha pelos novos dados
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

-- Função que limpa o terminal para melhor organização
limparTerminal :: IO ()
limparTerminal = do
      clearScreen
      setCursorPosition 0 0


exibirMensagemTemporaria :: String -> IO ()
exibirMensagemTemporaria msg = do
      putStrLn msg
      threadDelay (3 * 1000000)

--Função que recebe um nome e retorna apenas o primeiro nome do aluno (ex: Joao pedro -> Joao)
primeiroNome :: String -> String
primeiroNome nome = head (words nome)
