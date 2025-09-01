#' Gráficos de Conciliação Bancária vs CMF/IK
#'
#' @description
#' Função para criar gráficos de linha comparando trajetórias de entradas,
#' saídas e saldos líquidos entre dados bancários (extratos) e CMF/IK.
#' Gera três gráficos plotly: entradas, saídas e saldos líquidos.
#'
#' @param dadosFluxos Tibble com dados de fluxos por conta e mês.
#'   Se NULL (padrão), extrai automaticamente usando e_extratos()$fluxosContaMes
#'
#' @return Lista com três objetos plotly:
#'   - graficoEntradas: Gráfico de linha para entradas vs entradas.ik
#'   - graficoSaidas: Gráfico de linha para saídas vs saídas.ik
#'   - graficoSaldos: Gráfico de linha para saldo.liquido vs saldo.liquido.ik
#'
#' @export
#'
g_conciliacao <- function(dadosFluxos = NULL) {
  # Obter dados se não fornecidos
  if (is.null(dadosFluxos)) {
    message("Extraindo dados de e_extratos()...")
    dadosExtratos <- e_extratos()
    dadosFluxos <- dadosExtratos$fluxosContaMes
  }

  # Validar dados
  if (is.null(dadosFluxos) || nrow(dadosFluxos) == 0) {
    stop("Nenhum dado encontrado")
  }

  # Cores para as contas
  cores <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )

  contas <- unique(dadosFluxos$identificacao.conta)
  mapCores <- setNames(rep(cores, length.out = length(contas)), contas)

  # Função para criar um gráfico
  criarGrafico <- function(dados, colunaExtrato, colunaIk, titulo) {
    p <- plotly::plot_ly()

    for (conta in contas) {
      dadosConta <- dados[dados$identificacao.conta == conta, ]
      if (nrow(dadosConta) == 0) next

      cor <- mapCores[conta]

      # Linha sólida para extratos
      p <- plotly::add_lines(p,
        x = dadosConta$mes,
        y = dadosConta[[colunaExtrato]],
        name = paste(conta, "- Extratos"),
        line = list(color = cor, dash = "solid"),
        legendgroup = conta
      )

      # Linha tracejada para IK
      p <- plotly::add_lines(p,
        x = dadosConta$mes,
        y = dadosConta[[colunaIk]],
        name = paste(conta, "- IK"),
        line = list(color = cor, dash = "dash"),
        legendgroup = conta
      )
    }

    p <- plotly::layout(p,
      title = titulo,
      xaxis = list(title = "Mês"),
      yaxis = list(title = "Valor (R$)")
    )

    return(p)
  }

  # Criar os três gráficos
  graficoEntradas <- criarGrafico(
    dadosFluxos, "entradas", "entradas.ik",
    "Entradas: Extratos vs IK"
  )

  graficoSaidas <- criarGrafico(
    dadosFluxos, "saidas", "saidas.ik",
    "Saídas: Extratos vs IK"
  )

  graficoSaldos <- criarGrafico(
    dadosFluxos, "saldo.liquido", "saldo.liquido.ik",
    "Saldos Líquidos: Extratos vs IK"
  )

  # Retornar lista
  return(list(
    graficoEntradas = graficoEntradas,
    graficoSaidas = graficoSaidas,
    graficoSaldos = graficoSaldos
  ))
}
