import Funcionario

main :: IO()
main = do
    novo_funcionario <- criarFuncionario
    adcionarFuncionario novo_funcionario
