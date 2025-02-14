library(dplyr)
library(htmltools)
library(ggplot2)

ig_receitas_por_empreendimento <- 
  function(dados_pasta_informakon.l) {
    g.receitas.por.empreendimento <- 
      ggplot(dados.receitas.bp.informakon_df,
             aes(
               x = Mês,
               y = Total,
               fill = Empreendimento)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
      scale_y_continuous(
        labels = 
          scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
      ) +
      labs(
        x = "Data",
        y = "Valor (em R$)",
        fill = "Empreendimento"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    ig.receitas.por.empreendimento <- 
      tagList(
        tags$h4("[Gráfico] Trajetória das receitas por empreendimento")
      )
    return(ig.receitas.por.empreendimento)
  }
g.receitas.por.empreendimento
