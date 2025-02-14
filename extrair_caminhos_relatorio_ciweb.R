# Descrição ---------------------------------------------------------------

### RESUMO ###

# extrair_caminhos_relatorio_ciweb() compila numa lista todos os caminhos dos
# arquivos da CIWEB a serem extraídos para a elaboração do relatório.

### UTILIZAÇÃO ###

# extrair_caminhos_relatorio_ciweb(
#   caminho_pasta_ciweb.c = 
#     "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/"
#     "Relatórios - Documentos/Relatorios - CIWEB",
#   quais.c = "Todos"
# )

### ARGUMENTOS ###

# caminho_pasta_ciweb.c: Uma string com o caminho para a pasta 
# "Relatorios - CIWEB".

# quais.c: Uma string que define quais relatórios devem ser analisados. Ou "Todos"
# ou "Mais recentes".

# Pacotes -----------------------------------------------------------------

library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(stringr) # Funções para formatar códigos, e.g. style_file()
library(styler) # Funções para formatar códigos, e.g. style_file()
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2


# Criando função para listar os relatórios CIWEB 
extrair_caminhos_relatorio_ciweb <-
  ## Salvando o caminho para a pasta "Relatorios - CIWEB"
  function(caminho_pasta_ciweb.c = 
             paste0("C:/Users/Ampla/AMPLA INCORPORADORA LTDA/",
                    "Relatórios - Documentos/Relatorios - CIWEB"),
           quais.c = "Todos") {
  ## Mensagem de erro caso a pasta "Relatorios - CIWEB" não seja encontrada
    if (!dir.exists(caminho_pasta_ciweb.c)) {
      stop("A pasta 'Relatorios - CIWEB' não foi encontrada.")
    }
    ## Pastas contidas na pasta "Relatorios - CIWEB"
    caminhos.pastas.relatorios.ciweb_vc <- 
      list.dirs(caminho_pasta_ciweb.c,
                recursive = F)
    ## Identificando quais dessas pastas representam um empreendimento
    caminhos.pastas.empreendimentos_vc <- c()
    for(pasta.c in caminhos.pastas.relatorios.ciweb_vc) {
      ### Vetor com os nomes das subpastas dentro das pastas dos empreendimentos
      caminhos.subpastas.relatorios.ciweb_vc <- 
        basename(list.dirs(pasta.c,
                           recursive = F,
                           full.names = T))
      #### Preencher vetor com pastas que tenham subpasta nomeada "%d.%m.%y"
      if (any(str_detect(caminhos.subpastas.relatorios.ciweb_vc, 
                         "^\\d{2}\\.\\d{2}\\.\\d{2}$"))) {
        caminhos.pastas.empreendimentos_vc <- c(caminhos.pastas.empreendimentos_vc, pasta.c)
        #### Nome das pastas de empreendimentos
        nomes.pastas.empreendimentos_vc <- basename(caminhos.pastas.empreendimentos_vc)
      }
    }
    ## Arquivos contidos nas pastas dos empreeendimentos
    caminhos.arquivos.pastas.empreendimentos_vc <- 
      list.files(caminhos.pastas.empreendimentos_vc,
                 recursive = T,
                 full.names = T)
    ## Relatórios ECN contidos nas pastas dos empreendimentos
    caminhos.arquivos.ecn_vc <- 
      caminhos.arquivos.pastas.empreendimentos_vc[
        str_detect(
          basename(caminhos.arquivos.pastas.empreendimentos_vc),
          "EMPREENDIMENTO_CONSTRUCAO"
        )
      ]
    ### Lista de relatórios ECN por empreendimento
    caminhos.arquivos.ecn_l <- list()
    for(empreendimento.c in nomes.pastas.empreendimentos_vc) {
      caminhos.arquivos.ecn_l[[empreendimento.c]] <- 
        caminhos.arquivos.ecn_vc[str_detect(caminhos.arquivos.ecn_vc,
                                empreendimento.c)]
    }
    ## Lista a ser retornada se quais.c = "Todos"
    relatorios.ciweb_l <- list("ECN" = caminhos.arquivos.ecn_l,
                             "Empreendimentos" = nomes.pastas.empreendimentos_vc)
    ## Caso em que quais.c = "Mais recentes"
    if (quais.c == "Mais recentes") {
      ### Lista dos relatórios ECN mais atuais de cada empreendimento
      relatorios.ciweb.ecn.atuais_l <- list()
      for(empreendimento.c in nomes.pastas.empreendimentos_vc) {
        #### Vetor com as datas da última modificação
        relatorios.ciweb.ecn.modificacao_l <- 
          file.info(caminhos.arquivos.ecn_l[[empreendimento.c]])$mtime
        #### Relatório mais atual
        relatorios.ciweb.ecn.atuais_l[[empreendimento.c]] <- 
          caminhos.arquivos.ecn_l[[empreendimento.c]][
            which.max(relatorios.ciweb.ecn.modificacao_l)
          ]
      }
      
      relatorios.ciweb_l <- list("ECN" = relatorios.ciweb.ecn.atuais_l,
                               "Empreendimentos" = nomes.pastas.empreendimentos_vc)
    }
    ## Lista de contagem dos relatórios extraídos
    contagem.relatorios.ciweb.extraidos_l <- 
        list()
    ### Quantidades de cada tipo de relatório
      for (i in setdiff(names(relatorios.ciweb_l),
                        "Empreendimentos")) {
        contagem.relatorios.ciweb.extraidos_l[[i]] <- 
          sapply(relatorios.ciweb_l[!names(relatorios.ciweb_l) %in% 
                                          "Empreendimentos"][[i]], 
                 length)
      }
      relatorios.ciweb_l <- c(relatorios.ciweb_l,
                            list("Contagem" = contagem.relatorios.ciweb.extraidos_l))
    return(relatorios.ciweb_l)
  }

# Teste -------------------------------------------------------------------

#extrair_caminhos_relatorio_ciweb(quais.c = "Todos")