% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_dados.R
\name{e_dados}
\alias{e_dados}
\title{Consolidação de Dados: CEF e Informakon}
\usage{
e_dados()
}
\value{
Retorna uma lista contendo duas entradas:
\itemize{
\item \code{cef}: Resultado da função \code{e_cef()}
\item \code{ik}: Resultado da função \code{e_ik()}
}
}
\description{
A função \strong{e_dados()} consolida dados de diferentes fontes (CEF, Informakon)
em um único objeto, facilitando o acesso e manipulação.
}
\details{
Internamente chama \code{\link{e_cef}} e \code{\link{e_ik}} para obter
informações relacionadas a relatórios, extratos e dados específicos.
}
\examples{
\dontrun{
# Exemplo simples de chamada
dados <- e_dados()
str(dados)
s
}

}
\seealso{
\code{\link{e_cef}}, \code{\link{e_ik}}
}
