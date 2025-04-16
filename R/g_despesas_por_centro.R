library(dplyr)
library(htmltools)
library(ggplot2)
library(plotly)

ig_despesas_por_centro <-
  function(dados_bp_informakon.df) {
    g.despesas.por.centro <-
      ggplot(
        dados_bp_informakon.df,
        aes(
          x = `Mês`,
          y = `Total Pago no Mês`,
          fill = `Centro de Negócio`
        )
      ) +
      geom_bar(stat = "identity", position = "stack") +
      scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
      scale_y_continuous(
        labels =
          scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
      ) +
      labs(
        x = "Data",
        y = "Valor (em R$)",
        fill = "Centro de negócio"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom"
      )
    ig.despesas.por.centro <-
      tagList(
        tags$h4("[Gráfico] Trajetória das despesas por centro de negócio"),
        ggplotly(g.despesas.por.centro) %>%
          layout(width = 1200, height = 900, autosize = F)
      )
    return(ig.despesas.por.centro)
  }
