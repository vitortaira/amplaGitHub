library(dplyr)
library(htmltools)
library(ggplot2)

ig_vcxve <- 
  function(dados_relatorio.l) {
  vcxve_gdf <- 
    data.frame(
      barra = c("Valor do empréstimo", "Valor do empréstimo", 
                "Valor creditado", "Valor creditado"),
      tipo = c("Valor teto", "Valor reduzido", "Valor liberado de terreno", "Valor liberado de obra"),
      valor = 
        c(dados.relatorio_l[[empreendimento.c]][[t]][[i]]$Emprestimo$`Valor Empréstimo` - 
            dados.relatorio_l[[empreendimento.c]][[t]][[i]]$Emprestimo$`Valor Reduzido`,
          dados.relatorio_l[[empreendimento.c]][[t]][[i]]$Emprestimo$`Valor Reduzido`,
          sum(dados.relatorio_l[[empreendimento.c]][[t]][[i]]$Unidades$`Valor Liberado Terreno`),
          sum(dados.relatorio_l[[empreendimento.c]][[t]][[i]]$Unidades$`Valor Liberado Obra`))
    ) %>%
    group_by(barra) %>%
    mutate(cumsum_valor = cumsum(valor))
  # Create the stacked barplot with custom arrows and labels
  g.vcxve <- 
    ggplot(vcxve_gdf, aes(x = barra, y = valor, fill = tipo)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(data = vcxve_gdf %>% filter(tipo != "Valor teto"),
                aes(label = tipo, y = cumsum_valor - valor / 2), color = "black", size = 4) + 
      # Dashed line at the height of 'teto'
      geom_hline(data = vcxve_gdf %>%
                   filter(tipo == "Valor teto") %>%
                   select(barra, cumsum_valor), aes(yintercept = cumsum_valor), 
                 linetype = "dashed", color = "gray") +
      # Add the "Valor" label at the left extremity and above the hline
      geom_label(aes(x = 0, y = max(vcxve_gdf %>%
                                      filter(tipo == "Valor teto") %>%
                                      select(barra, cumsum_valor) %>% pull(cumsum_valor)),
                     label = '"Valor teto"'),
                color = "gray", fill = "white", size = 4, hjust = 0) +
      labs(y = "Valor (em R$)", x = "") +
      scale_fill_manual(values = c("Valor teto" = "blue", 
                                   "Valor reduzido" = "red", 
                                   "Valor liberado de terreno" = "green", 
                                   "Valor liberado de obra" = "gray")) +
      scale_y_continuous(labels = scales::label_number(
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 1
      )) +
      theme_minimal() +
      theme(legend.position = "none")
  ig.vcxve <- 
    tagList(
      tags$h4("[Gráfico] Valor creditado x valor do empréstimo"),
      ggplotly(g.vcxve)
    )
  return(ig.vcxve)
  }