# app.R (or server.R + ui.R as you prefer)
library(here)
library(readr)
library(plotly)
library(shiny)
library(tidyverse)
#source(here::here("here_config.R"))
#source(here("dados", "funcoes", "extrair_dados_pasta_informakon.R"))
print(here)
dados.pasta.informakon.despesas <-
  read_csv(here("Ampla_Shiny", "dados.pasta.informakon.despesas.csv"))
ui <- fluidPage(
  titlePanel("[Gráfico] Trajetória das despesas"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variavel",
        label   = "Selecione a variável para agrupar:",
        choices = c("Agente Financeiro", "Centro de Negócio", "Empresa"),
        selected = "Centro de Negócio"
      )
    ),
    mainPanel(
      plotlyOutput("stackedPlot", width = "1200px", height = "900px")
    )
  )
)
server <- function(input, output, session) {
  output$stackedPlot <- renderPlotly({
    req(input$variavel)
    # 1) Summarize your data by Mês + the variable chosen
    df_summary <- dados.pasta.informakon.despesas %>%
      group_by(Mês, Var = .data[[input$variavel]]) %>%
      summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop")
    # 2) Build a stacked bar chart with plot_ly
    plot_ly(
      data   = df_summary,
      x      = ~Mês,
      y      = ~Total,
      color  = ~Var,
      type   = "bar"
    ) %>%
      layout(
        title    = list(text = "[Gráfico] Trajetória das despesas"),
        barmode  = "stack",
        # Force local-style number separators (period as thousands, comma as decimal)
        separators = ".,",
        xaxis = list(
          title      = "Data",
          tickformat = "%b-%y",         # Month-Year format on x-axis
          tickangle  = 45               # Similar to element_text(angle=45)
        ),
        yaxis = list(
          title     = "Valor (em R$)",
          autorange = TRUE
          # If you want to limit decimal places, you can do something like:
          # tickformat = ".0f"
        ),
        legend = list(
          orientation = "h",  # position legend at bottom (horizontal)
          x = 0.05,
          y = -0.15
        ),
        width    = 1200,
        height   = 900,
        autosize = FALSE
      )
  })
}
shinyApp(ui, server)