# CODEFIT

O CODEFIT, é um sistema de gerenciamento de academia que pode ser utilizado por gestores, funcionários e alunos. O sistema contém funcionalidades específicas para cada usuário visando modularizar o sistema, atribuindo cada funcionalidade a entidade específica.



## Funcionalidades

- Conheça todas as funcionalidades disponíveis acessando a [documentação oficial do projeto](https://docs.google.com/document/d/1bcGVitOdJ7p6JWy1ikgXhyCYMsLPlYAsG7g90WwrG-o/edit#heading=h.phrzmz7sb5x0). 

- Vídeo de apresentação do CODEFIT:[Sistema CODEFIT](https://youtu.be/QI1rMvbUfJw?si=pbMADGmhkyflZ2QQ).


## Rodando localmente

### Cuidado!
É preciso que você tenha o Cabal instalado e atualizado em sua máquina.
Caso não tenha o cabal, essas informações vão te ajudar...

Cole em seu terminal
```bash
  Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```
Em caso de dúvidas acompanhe o vídeo:
```bash
  [https://youtu.be/bB4fmQiUYPw?feature=shared]
```

Tudo pronto? Vamos em frente!

Clone o projeto em sua IDE

```bash
  git clone [https://github.com/kevinicolas22/projeto-plp.git]
```

Entre no diretório do projeto

```bash
  cd haskell
```

Gerar arquivo .cabal

```bash
  cabal init
```
Instalar dependências

```bash
  cabal build
```

Execute o programa com

```bash
  cabal run
```

Ainda é possível que o programa apresente erros porque você não está com o cabal devidamente atualizado. Atualize-o com o comando abaixo e tente novamente.
```bash
  cabal upgrade
```

