ig_despesas_trajetoria <- function(dados_pasta_informakon.df, stack_var) {
  
  # Check column names
  print(colnames(dados_pasta_informakon.df))  # Debugging step
  
  # Ensure correct column reference
  p <- ggplot(
    data = dados_pasta_informakon.df,
    aes(
      x    = Mês,
      y    = .data[["Total Pago"]],  # Corrected column reference
      fill = .data[[stack_var]]      # Dynamically selecting the grouping variable
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
    ) +
    labs(
      x    = "Data",
      y    = "Valor (em R$)",
      fill = stack_var
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      legend.position = "bottom"
    )
  
  # Convert to plotly
  p_plotly <- ggplotly(p) %>%
    layout(width = 1200, height = 900, autosize = FALSE)
  
  # If you’re using tagList within a Shiny app:
  tagList(
    tags$h4("[Gráfico] Trajetória das despesas"),
    p_plotly
  )
}
