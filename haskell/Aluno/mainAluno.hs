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


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Network.Mail.SMTP.Types (Address(..))

main :: IO ()
main = do 
      alunoSaida<- criarAluno
      loginAluno alunoSaida
      
      
     {- alunoSaida<- criarAluno
      let detalhes = detalhesPlano(planoAluno alunoSaida)
      putStrLn detalhes
      clearScreen
      setCursorPosition 0 0
      putStrLn ("\n\nAAAAAAluno criado!!"++ show(alunoSaida))-}

-- Funçao que inicia a pagina manuseada pelo aluno, necessitando da matricula e senha para acessar
loginAluno::Aluno->IO()
loginAluno alunoSaida= do
      limparTerminal  -- funçao que limpa o terminal
      putStrLn "   ==== LOGIN/ALUNO ==== "
      putStr   "> Matrícula: "
      hFlush stdout
      matriculaAluno<- getLine
      if not(matriculaAluno == matricula alunoSaida)
            then do
                  limparTerminal
                  exibirMensagemTemporaria "\n!! Matricula não encontrada. Tente novamente... !!"
                  loginAluno alunoSaida 
      else do  
            putStr "> Senha: " 
            hFlush stdout
      senhaAluno<- getLine
      if not(senhaAluno == senha alunoSaida)
            then do
                  limparTerminal
                  exibirMensagemTemporaria "\n!! Senha incorreta. Tente novamente... !!"
                  loginAluno alunoSaida 
                  
      else do
            exibirMensagemTemporaria "\nCarregando..."
      homeAluno alunoSaida

homeAluno:: Aluno-> IO()
homeAluno alunoAtual = do 
      clearScreen
      setCursorPosition 0 0
      putStrLn (" ________ Olá "++ primeiroNome(nome alunoAtual)++ " _________\n")
      putStrLn " [1] Consultar Plano atual"
      putStrLn " [2] Alterar Plano"
      putStrLn " [3] Meus Dados"
      putStrLn " [4] Treinos"
      putStrLn  " [5] Realizar Pagamento"
      putStrLn  "\n- Digite a opção: "
      opçao<- getLine
      case opçao of
            "1"-> exibePlano (alunoAtual)
            "2"-> alterarPlano alunoAtual
            "3"-> meusDados alunoAtual
            "5"-> realizaPagamento alunoAtual
            _ -> do
                  putStr "Opção inválida!!"
                  homeAluno alunoAtual
           


meusDados:: Aluno-> IO() 
meusDados aluno= do
      limparTerminal
      putStrLn ("   ======= "++nome aluno++ " =======\n")
      putStr (exibirAluno aluno)
      putStrLn "\n\n>       [0] Voltar ao menu     [1] Editar dados "
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
                  exibirMensagemTemporaria " *Nome atualizado com sucesso*"
                  homeAluno alunoNovo
                  
            "2" -> do 
                  putStrLn " -> Novo endereço: "
                  novoEndereco <- getLine
                  let alunoNovo = aluno { endereço = novoEndereco }
                  exibirMensagemTemporaria " *Endereço atualizado com sucesso*"
                  homeAluno alunoNovo
            "3" -> do 
                  putStrLn " -> Novo contato: "
                  novoContato <- getLine
                  let alunoNovo = aluno { contato = novoContato }
                  exibirMensagemTemporaria " *Contato atualizado com sucesso*"
                  homeAluno alunoNovo
            "4" -> do 
                  putStrLn " -> Nova senha: "
                  novaSenha <- getLine
                  let alunoNovo = aluno { senha = novaSenha }
                  exibirMensagemTemporaria " *Senha atualizada com sucesso*"
                  homeAluno alunoNovo
            "5" -> do 
                  putStrLn " -> Novo email: "
                  novoEmail <- getLine
                  let alunoNovo = aluno { senha = novoEmail }
                  exibirMensagemTemporaria " *Email atualizada com sucesso*"
                  homeAluno alunoNovo
            _ -> do
                  putStrLn "Opção inválida!"
                  exibirMensagemTemporaria " *Opção inválida*"
                  editDados aluno
 
obterPlanoParaAluno :: Aluno -> Plano
obterPlanoParaAluno aluno = case planoAluno aluno of
      Light -> planoLight
      Gold -> planoGold
      Premium -> planoPremium                  

-- Funçao  para enviar um Email de confirmação do pagamento para a academia
enviarEmail :: Aluno-> IO ()
enviarEmail aluno= do
    let planoAtual = obterPlanoParaAluno aluno
     -- Configurar as informações do servidor SMTP
    let from       = Address Nothing (T.pack (email aluno))
        to         = [Address (Just (T.pack "Codefit")) (T.pack "joao.pedro.arruda.silva@ccc.ufcg.edu.br")] --colocar o email aqui (monitor/professor) para correção, envia a mensagem para o email especificado acima
        cc         = []
        bcc        = []
        subject    = T.pack "Pagamento de Mensalidade"
        body       = plainTextPart (TL.pack (nome aluno++" efetuou o pagamento do mês\n R$"++ show (valorMensal planoAtual)++"\n- Matrícula: "++ show(matricula aluno)))
       
    
    let mail = simpleMail from to cc bcc subject [body]
    
    sendMailWithLoginSTARTTLS "smtp.gmail.com" "jpcros40414@gmail.com" "sudx dymy kypg ocez" mail






realizaPagamento:: Aluno-> IO()
realizaPagamento aluno= do
      limparTerminal
      putStrLn "   ==== FINANCEIRO ====\n"
      if emDia aluno
            then do 
                  putStrLn " - Sua mensalidade está em dia !" 
                  putStrLn "\n>Pressione ENTER para voltar..."
                  _ <- getLine -- Aguarda o Enter
                  homeAluno aluno
      else do
            putStrLn " - Mensalidade Pendende."
            putStrLn "\n> Forma de pagamento: \n   [P]Pix    [D]cartão de debito   [C]cartão de cradito   [B]Boleto"
            hFlush stdout
            opçao <- getLine
            exibirMensagemTemporaria "\n *Processando pagamento...*"
            
            let alunoNovo = aluno{emDia = True}
            putStrLn $ "\x1b[32m" ++ " Pagamento Realizado." ++ "\x1b[0m"
            threadDelay (2 * 1000000)
            enviarEmail aluno
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
      putStrLn "\n>Pressione ENTER para confirmar..."
      _ <- getLine -- Aguarda o Enter
      homeAluno alunoNovo
      
      
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
{-recuperaMatricula:: String-> Bool
recuperaMatricula matricula = 


recuperaSenha:: String-> Bool
recuperaSenha senha= 

recuperaAluno:: String-> Aluno
recuperaAluno matricula-}