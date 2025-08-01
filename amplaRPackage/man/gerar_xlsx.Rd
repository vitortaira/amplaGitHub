\name{gerar_xlsx}
\alias{gerar_xlsx}
\title{Gera planilha Excel personalizada}
\usage{
gerar_xlsx(
  data,
  wb_load = NULL,
  tab_names = NULL,
  col_width_def = 18,
  col_width_spec = NULL,
  col_monetary = NULL,
  col_clip = NULL,
  save = NULL
)
}
\arguments{
\item{data}{Um tibble/dataframe ou uma lista nomeada de tibbles/dataframes}

\item{wb_load}{Caminho opcional para arquivo de template a ser copiado e carregado.
Se vazio, cria um novo workbook}

\item{tab_names}{Vetor de strings com nomes das abas. Se NULL, usa nomes da lista
ou "Dados" para tibble único}

\item{col_width_def}{Largura padrão das colunas (valor numérico). Padrão: 18}

\item{col_width_spec}{Vetor nomeado com larguras específicas para colunas por nome}

\item{col_monetary}{Vetor com nomes de colunas que devem ser formatadas como valores monetários}

\item{col_clip}{Vetor com nomes de colunas de texto que devem ter quebra de texto habilitada}

\item{save}{Lista com 2 elementos: (1) nome do arquivo, (2) caminho de destino.
Se NULL, não salva o arquivo}
}
\value{
Caminho do arquivo salvo (se save não for NULL) ou objeto workbook
}
\description{
Função genérica para criar planilhas Excel com formatação padronizada.
Pode receber um tibble/dataframe (para uma aba) ou uma lista de tibbles
(para múltiplas abas). A formatação é baseada nos tipos das colunas e
argumentos específicos para colunas monetárias e com quebra de texto.
}
