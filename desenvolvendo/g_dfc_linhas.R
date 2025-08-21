# -*- coding: utf-8 -*-
# =============================================================================
# GRAFICO: DFC em linhas por empresa (Repasse e PJ separados)
# =============================================================================

library(dplyr)
library(plotly)
library(lubridate)
library(tidyr)
library(tools)

# Carregar dados
dados <- e_extratos()$DFC

# Preparar os dados para o gráfico (formato longo)
dados_long <- dados %>%
  tidyr::pivot_longer(
    cols = c("repasse", "pj"),
    names_to = "tipo",
    values_to = "valor",
    values_drop_na = FALSE
  ) %>%
  dplyr::mutate(
    valor = tidyr::replace_na(.data$valor, 0),
    tipo = tools::toTitleCase(.data$tipo) # Capitalizar primeira letra
  )

# Separar os dados por tipo
dados_repasse <- dados_long %>% dplyr::filter(tipo == "Repasse")
dados_pj <- dados_long %>% dplyr::filter(tipo == "Pj")

# Criar o gráfico
grafico_dfc <- plotly::plot_ly() %>%
  # Adicionar linhas para repasse
  plotly::add_trace(
    data = dados_repasse,
    x = ~mes,
    y = ~valor,
    color = ~empresa,
    name = ~ paste(empresa, "(Repasse)"),
    type = "scatter",
    mode = "lines+markers",
    line = list(dash = "solid"),
    hoverinfo = "text",
    text = ~ paste(
      "Empresa:", empresa,
      "\nMês:", format(mes, "%b/%Y"),
      "\nRepasse: R$", format(valor, big.mark = ".", decimal.mark = ",", digits = 2)
    )
  ) %>%
  # Adicionar linhas para PJ
  plotly::add_trace(
    data = dados_pj,
    x = ~mes,
    y = ~valor,
    color = ~empresa,
    name = ~ paste(empresa, "(PJ)"),
    type = "scatter",
    mode = "lines+markers",
    line = list(dash = "dot"),
    hoverinfo = "text",
    text = ~ paste(
      "Empresa:", empresa,
      "\nMês:", format(mes, "%b/%Y"),
      "\nPJ: R$", format(valor, big.mark = ".", decimal.mark = ",", digits = 2)
    )
  ) %>%
  plotly::layout(
    title = list(
      text = "DFC por Empresa - Repasse e PJ",
      x = 0.5, # Centralizar título
      font = list(size = 20)
    ),
    xaxis = list(
      title = "Mês",
      tickformat = "%b/%Y",
      gridcolor = "#E5E5E5",
      showgrid = TRUE
    ),
    yaxis = list(
      title = "Valor (R$)",
      gridcolor = "#E5E5E5",
      showgrid = TRUE,
      tickformat = ",.2f",
      tickprefix = "R$ "
    ),
    legend = list(
      orientation = "h", # Legenda horizontal
      xanchor = "center", # Ancora no centro
      x = 0.5, # Posiciona no centro
      y = -0.2 # Posiciona abaixo do gráfico
    ),
    margin = list(b = 100), # Margem inferior para acomodar a legenda
    plot_bgcolor = "#FFFFFF", # Fundo branco
    paper_bgcolor = "#FFFFFF" # Fundo do papel branco
  )

# Exibir o gráfico
grafico_dfc

# Exportar para HTML se necessário
# htmlwidgets::saveWidget(grafico_dfc, "relatorios/g_dfc_linhas.html")
teste <- dplyr::filter(
  cef,
  (empresa == "AMP") &
    (month(data.movimentacao) == 7) &
    (year(data.movimentacao) == 2024) &
    (descricao == "CRE D IMOB")
) %>%
  pull(valor) %>%
  sum()

View(dplyr::filter(
  cef,
  (empresa == "AMP") &
    (month(data.movimentacao) == 7) &
    (year(data.movimentacao) == 2024)
))
