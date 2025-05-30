% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_cef.R
\name{e_cef}
\alias{e_cef}
\title{Consolidação de Dados dos Relatórios CEF}
\usage{
e_cef()
}
\value{
Retorna uma lista contendo todos os dados extraídos das várias fontes:
CMF_CN, ECN, EPR e Extratos.
}
\description{
A função \strong{e_cef()} consolida e retorna dados de diversos relatórios da CEF,
como CMF_CN, ECN, EPR e Extratos, num só objeto (lista).
}
\details{
Internamente chama as funções \code{e_cef_cmfcns()}, \code{e_cef_ecns()},
\code{e_cef_eprs()} e \code{e_cef_extcefs()}, reunindo tudo num único objeto.
}
\examples{
\dontrun{
# Utilizando as configurações padrão
lista_cef <- e_cef()
str(lista_cef)
}

}
