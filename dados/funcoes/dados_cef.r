### RESUMO ###
# new_function() realiza [descreva aqui o que a função faz].
# Exemplo: Consolida e processa dados de [origem], retornando uma lista
# contendo [descrição dos outputs].

### UTILIZAÇÃO ###
# new_function(
#   arg1,
#   arg2,
#   ...
# )

### ARGUMENTOS ###
# arg1: Descrição do primeiro argumento.
# arg2: Descrição do segundo argumento.
# ...: Outras opções de argumentos.

# Pacotes -----------------------------------------------------------------
library(here) # Facilita a construção de caminhos
library(tidyverse) # Pacote com funcoes úteis para manipulação de dados
library(magrittr) # Operador pipe e outras utilidades
library(lubridate) # Manipulação de datas

# Função ------------------------------------------------------------------
new_function <- function(arg1, arg2, ...) {
  # Validação de argumentos (opcional)
  if (missing(arg1)) {
    stop("arg1 deve ser informado.")
  }

  # Insira aqui a lógica da função...

  # Exemplo de criação de uma lista para retorno:
  result <- list(
    param1 = arg1,
    param2 = arg2
    # Adicione outros componentes conforme necessário
  )

  return(result)
}

# Teste -------------------------------------------------------------------
# Exemplo de uso:
# dados_novos <- new_function("valor1", "valor2")
# str(dados_novos)
# View(dados_novos$param1)
