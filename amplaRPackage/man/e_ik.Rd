% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_ik.R
\name{e_ik}
\alias{e_ik}
\title{Consolidação de Dados dos Relatórios Informakon}
\usage{
e_ik()
}
\value{
Retorna uma lista contendo todos os dados extraídos das várias fontes:
Despesas e Receitas.
}
\description{
A função \strong{e_ik()} consolida e retorna dados de diversos relatórios
(Despesas e Receitas) da Informakon em um só objeto (lista).
}
\details{
Internamente chama as funções \code{e_ik_desp()} e \code{e_ik_rec()} e
reúne tudo em um único objeto.
}
\examples{
\dontrun{
# Utilizando as configurações padrão
lista_ik <- e_ik()
str(lista_ik)
}

}
