
import Login
import LoginService
import Data.Char (toUpper)

--Interface sistema
main :: IO()
main = do
    menuLogin

menuLogin :: IO()
menuLogin = do 
    putStrLn "╔═══════════════════════════════════════════════════════╗"
    putStrLn "║               Seja Bem-vindo a CodeFit                ║"
    putStrLn "╚═══════════════════════════════════════════════════════╝"
    putStrLn "══════════════════════Faça seu login═════════════════════"
    putStrLn "Digite o seu cpf: "
    cpf <- getLine
    cpfValido <- cpfCorreto cpf

    putStrLn "Digite sua senha: "
    senha <- getLine
    senhaValida <- senhaCorreta senha 

    putStrLn "Digite o tipo de usuario (1 - ALUNO, 2 - GESTOR, 3 - FUNCIONARIO): "
    tipoFuncionario <- readLn :: IO Int
    tipoFuncionarioValidado <- tipoUsuarioCorreto tipoFuncionario
    
    existeCadastroR <- existeCadastro cpfValido

    tipoEsenhaCorreto <- cadastroCondizComTipoESenha cpfValido tipoFuncionarioValidado senhaValida
    if existeCadastroR
        then do
            if tipoEsenhaCorreto
                then do
                    tipo <- tipoMenu cpfValido
                    case (tipo) of
                        "1" -> putStrLn "Sou aluno" --aqui chama o menu de aluno
                        "2" -> putStrLn "Sou gestor" -- chama menu de gestor
                        "3" -> putStrLn "Sou funcionatio" --chama menu de funcionario
                else do
                    putStrLn "Senha inválida ou tipo de usuario inválido, tente novamente!"
                    menuLogin
        else do
            cadastraLogin cpfValido senhaValida tipoFuncionarioValidado
            putStrLn "--Seu primeiro acesso foi cadastrado, faça o login novamente"
            menuLogin


cpfCorreto :: String -> IO String
cpfCorreto cpf = do
    case delimitarCpf cpf of
        Just x -> return x
        Nothing -> do
            putStrLn "CPF não está no formato 000.000.000-00, digite novamente: "
            novocpf <- getLine
            cpfCorreto novocpf

tipoUsuarioCorreto :: Int ->IO Int
tipoUsuarioCorreto x = if verificarIntTipoFuncionario x
                            then return x
                            else do
                                putStrLn "Opção inválida, tente novamente!"
                                novaOpcao <- readLn :: IO Int
                                tipoUsuarioCorreto novaOpcao

senhaCorreta :: String -> IO String
senhaCorreta senha = do
    if length senha >= 6
        then return senha
        else do
            putStrLn "Senha tem que possuir no mínimo 6 caracters, tente novamente!"
            novaSenha <- getLine
            senhaCorreta novaSenha 

verificarIntTipoFuncionario :: Int -> Bool
verificarIntTipoFuncionario x = x >= 1 && x <= 3
