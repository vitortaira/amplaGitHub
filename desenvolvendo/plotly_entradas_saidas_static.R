# Static plotly barplot: Entradas vs Saídas, stacked by (Variável, Status), colored by Variável
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)

# Data (manually entered from user table)
df <- tibble::tribble(
  ~Variavel, ~Valor, ~Natureza, ~Status, ~Fonte, ~Cor,
  "VGV", 58847200, "Estimativa", "Estimativa", "Viabilidade", "black",
  "Receitas de pró soluto", 10015786.43, "Entrada", "Realizado", "Informakon", "lightgreen",
  "Receitas de repasse (PF)", 7034962, "Entrada", "Realizado", "CEF (ECN)", "green",
  "Empréstimo (PJ)", 1438405.41, "Entrada", "Realizado", "CEF (ECN)", "darkgreen",
  "Receitas de pró soluto", 47360828.82, "Entrada", "A realizar", "Informakon", "lightgreen",
  "Receitas de repasse (PF)", 24235453, "Entrada", "A realizar", "CEF (DCD)", "green",
  "Receitas de repasse (PF) pendentes", 0, "Entrada", "A realizar", "Informakon (???)", "red",
  "Despesas relacionadas ao custo obra", 3834818, "Saída", "Realizado", "Planilha do Renato", "orange",
  "Demais despesas", 9959631, "Saída", "Realizado", "Planilha do Renato", "yellow",
  "Despesas relacionadas ao custo obra", 30474997, "Saída", "A realizar", "Viabilidade + Planilha do Renato", "orange",
  "Demais despesas", 12364052, "Saída", "A realizar", "Viabilidade + Planilha do Renato", "yellow",
  "Lucro estimado", 33451938, "Saída", "A realizar", "Essa tabela", "blue"
)

# Remove VGV from bar data, keep for line
bar_df <- df %>% filter(Variavel != "VGV")

# For stacking: group by Natureza (Entrada/Saída), Status, Variavel
# We'll use Status as the stack, but color by Variavel

# Set color vector for plotly
color_map <- setNames(bar_df$Cor, bar_df$Variavel)

# Plot
p <- plot_ly()

for (natureza in c("Entrada", "Saída")) {
  sub <- bar_df %>% filter(Natureza == natureza)
  for (status in unique(sub$Status)) {
    sub2 <- sub %>% filter(Status == status)
    if (nrow(sub2) > 0) {
      p <- p %>% add_trace(
        data = sub2,
        x = natureza,
        y = ~Valor,
        type = "bar",
        name = paste0(sub2$Variavel, " (", status, ")"),
        marker = list(color = sub2$Cor),
        text = ~scales::comma(Valor, big.mark = ".", decimal.mark = ","),
        hoverinfo = "text",
        hovertext = ~paste0(Variavel, "<br>Status: ", Status, "<br>Valor: R$ ", scales::comma(Valor, big.mark = ".", decimal.mark = ","))
      )
    }
  }
}

# Add VGV as a dashed line
p <- p %>% add_trace(
  x = c("Entrada", "Saída"),
  y = rep(df$Valor[df$Variavel == "VGV"], 2),
  type = "scatter",
  mode = "lines",
  name = "VGV",
  line = list(color = "black", dash = "dash", width = 2),
  showlegend = TRUE
)

p <- p %>% layout(
  barmode = "stack",
  title = "Entradas e Saídas - Stacked por Status, colorido por Variável",
  xaxis = list(title = "Natureza"),
  yaxis = list(title = "Valor (R$)", tickformat = ",.0f"),
  legend = list(orientation = "v")
)

p
