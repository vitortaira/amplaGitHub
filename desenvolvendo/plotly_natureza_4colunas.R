# Interactive plotly barplot with 4 columns by Natureza, stacking order as in CSV, colored by 'Cor'
library(plotly)
library(dplyr)
library(readr)

# Data embedded as a string (from user CSV)
data_csv <- 'Variável,Valor,Natureza,Fonte,Cor
"Despesas obra (a realizar)","R$ 30.474.997","Saídas","Viabilidade + Planilha do Renato","red/"
"Despesas não-obra (a realizar)","R$ 16.134.977","Saídas","Viabilidade + Planilha do Renato","orange/"
"Despesas obra (realizado)","R$ 3.834.818","Saídas","Planilha do Renato","red"
"Despesas não-obra (realizado)","R$ 9.959.631","Saídas","Planilha do Renato","orange"
"Estoque","R$ 3.928.182","Entradas","Anapro","black|"
"Receitas de repasses CEF pendentes","R$ 5.496.750","Entradas","Planilha da Carol","blue|"
"Receitas de repasse CEF (a realizar)","R$ 24.235.453","Entradas","CEF (DCD)","blue/"
"Receitas de pró soluto (a realizar)","R$ 9.161.249","Entradas","Informakon","green/"
"Receitas de repasse CEF (realizado)","R$ 6.101.788","Entradas","CEF (Extratos classificados Fran)","blue"
"Receitas de pró soluto (realizado)","R$ 10.015.786","Entradas","Informakon","green"
"VGV","R$ 61.944.421","Viabilidade","Viabilidade","dashed"
"Terreno permuta física","R$ 4.968.691","Viabilidade","Viabilidade","pink"
"Total de despesas obra","R$ 34.309.815","Viabilidade","Viabilidade","red"
"Total de despesas não-obra","R$ 19.969.795","Viabilidade","Viabilidade","orange"
"Impostos sobre lucros","R$ 1.129.866","Viabilidade","Viabilidade","lightgray"
"Impostos sobre receita","R$ 1.224.022","Viabilidade","Viabilidade","gray"
"Lucro líquido","R$ 10.279.614","Viabilidade","Viabilidade","lightgreen"
"Saldo de repasse CEF","R$ 24.235.453","CEF","CEF (DCD)","blue/"
"Saldo empréstimo PJ","R$ 13.649.531","CEF","CEF (DCD)","purple/"'

# Read data, fix colnames, and convert 'Valor' to numeric
names_df <- c("Variavel", "Valor", "Natureza", "Fonte", "Cor")
df <- read_csv(data_csv, locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8"), show_col_types = FALSE)
names(df)[1:length(names_df)] <- names_df

df$Valor <- gsub("[^0-9,]", "", df$Valor)
df$Valor[df$Valor == ""] <- NA
df$Valor <- as.numeric(gsub(",", ".", df$Valor))

# Keep row order for stacking
natureza_levels <- c("Entradas", "CEF", "Saídas", "Viabilidade")
df$Natureza <- factor(df$Natureza, levels = natureza_levels)
df$stack_id <- seq_len(nrow(df))

# Remove VGV from bar segments, but keep for line
bar_df <- df %>% filter(!is.na(Variavel) & Variavel != "VGV" & !is.na(Valor))

# Helper: get marker style based on Cor suffix
get_marker_style <- function(cor) {
  if (grepl("/$", cor)) {
    list(pattern = list(shape = "/", fgcolor = cor_no_marker(cor), bgcolor = "white"))
  } else if (grepl("\\|$", cor)) {
    list(pattern = list(shape = "x", fgcolor = cor_no_marker(cor), bgcolor = "white"))
  } else {
    NULL
  }
}
cor_no_marker <- function(cor) {
  sub("[\\/|]$", "", cor)
}

# Helper: create white box annotation for value labels
make_whitebox_anno <- function(x, y, text, font_color = "white", font_size = 8, width = 32, height = 12) {
  list(
    x = x,
    y = y,
    text = text,
    showarrow = FALSE,
    font = list(color = font_color, size = font_size, family = "Arial Black"),
    align = "center",
    xanchor = "center",
    yanchor = "middle",
    bgcolor = "white",
    bordercolor = "#e0e0e0",
    borderpad = 1,
    opacity = 1,
    borderwidth = 1,
    width = width,
    height = height
  )
}

# Compute segment positions for annotations
label_annotations <- list()
for (natureza in natureza_levels) {
  sub <- bar_df %>%
    filter(Natureza == natureza) %>%
    arrange(stack_id)
  y0 <- 0
  if (natureza == "Viabilidade" && any(sub$Variavel == "Terreno permuta física")) {
    terreno_idx <- which(sub$Variavel == "Terreno permuta física")
    # Negative segment
    y1 <- y0 - abs(sub$Valor[terreno_idx]) / 2
    seg_cor <- sub$Cor[terreno_idx]
    font_col <- ifelse(grepl("[\\/|]$", seg_cor), "black", "white")
    label_annotations[[length(label_annotations) + 1]] <- make_whitebox_anno(
      x = natureza,
      y = y1,
      text = paste0("-", scales::comma(sub$Valor[terreno_idx], big.mark = ".", decimal.mark = ",")),
      font_color = font_col
    )
    y0 <- y0 - abs(sub$Valor[terreno_idx])
    for (i in seq_len(nrow(sub))) {
      if (i != terreno_idx) {
        y1 <- y0 + sub$Valor[i] / 2
        seg_cor <- sub$Cor[i]
        font_col <- ifelse(grepl("[\\/|]$", seg_cor), "black", "white")
        label_annotations[[length(label_annotations) + 1]] <- make_whitebox_anno(
          x = natureza,
          y = y1,
          text = scales::comma(sub$Valor[i], big.mark = ".", decimal.mark = ","),
          font_color = font_col
        )
        y0 <- y0 + sub$Valor[i]
      }
    }
  } else if (nrow(sub) > 0) {
    for (i in seq_len(nrow(sub))) {
      y1 <- y0 + sub$Valor[i] / 2
      seg_cor <- sub$Cor[i]
      font_col <- ifelse(grepl("[\\/|]$", seg_cor), "black", "white")
      label_annotations[[length(label_annotations) + 1]] <- make_whitebox_anno(
        x = natureza,
        y = y1,
        text = scales::comma(sub$Valor[i], big.mark = ".", decimal.mark = ","),
        font_color = font_col
      )
      y0 <- y0 + sub$Valor[i]
    }
  }
}

# For each Natureza, preserve original CSV stacking and color order (do NOT reverse)
p <- plot_ly()
for (natureza in natureza_levels) {
  sub <- bar_df %>%
    filter(Natureza == natureza) %>%
    arrange(stack_id)
  if (natureza == "Viabilidade" && any(sub$Variavel == "Terreno permuta física")) {
    terreno_idx <- which(sub$Variavel == "Terreno permuta física")
    terreno_cor <- sub$Cor[terreno_idx]
    terreno_marker <- get_marker_style(terreno_cor)
    marker_list <- list(color = cor_no_marker(terreno_cor))
    if (!is.null(terreno_marker)) {
      marker_list$pattern <- terreno_marker$pattern
      text_color <- "black"
    } else {
      text_color <- "white"
    }
    # Add bar, but hide from legend
    p <- p %>% add_trace(
      x = natureza,
      y = -abs(sub$Valor[terreno_idx]),
      type = "bar",
      name = sub$Variavel[terreno_idx],
      marker = marker_list,
      text = NULL,
      hoverinfo = "text",
      hovertext = paste0(sub$Variavel[terreno_idx], "<br>Valor: -R$ ", scales::comma(sub$Valor[terreno_idx], big.mark = ".", decimal.mark = ","), "<br>Fonte: ", sub$Fonte[terreno_idx]),
      showlegend = FALSE
    )
    # Add all other segments above, in original CSV order
    y_cum <- 0
    for (i in seq_len(nrow(sub))) {
      if (i != terreno_idx) {
        seg_cor <- sub$Cor[i]
        seg_marker <- get_marker_style(seg_cor)
        marker_list <- list(color = cor_no_marker(seg_cor))
        if (!is.null(seg_marker)) {
          marker_list$pattern <- seg_marker$pattern
          text_color <- "black"
        } else {
          text_color <- "white"
        }
        y0 <- y_cum
        y1 <- y_cum + sub$Valor[i]
        y_cum <- y1
        p <- p %>% add_trace(
          x = natureza,
          y = sub$Valor[i],
          type = "bar",
          name = sub$Variavel[i],
          marker = marker_list,
          text = NULL,
          hoverinfo = "text",
          hovertext = paste0(sub$Variavel[i], "<br>Valor: R$ ", scales::comma(sub$Valor[i], big.mark = ".", decimal.mark = ","), "<br>Fonte: ", sub$Fonte[i])
        )
      }
    }
  }
  if (!(natureza == "Viabilidade" && any(sub$Variavel == "Terreno permuta física")) && nrow(sub) > 0) {
    y_cum <- 0
    for (i in seq_len(nrow(sub))) {
      seg_cor <- sub$Cor[i]
      seg_marker <- get_marker_style(seg_cor)
      marker_list <- list(color = cor_no_marker(seg_cor))
      if (!is.null(seg_marker)) {
        marker_list$pattern <- seg_marker$pattern
        text_color <- "black"
      } else {
        text_color <- "white"
      }
      y0 <- y_cum
      y1 <- y_cum + sub$Valor[i]
      y_cum <- y1
      p <- p %>% add_trace(
        x = natureza,
        y = sub$Valor[i],
        type = "bar",
        name = sub$Variavel[i],
        marker = marker_list,
        text = NULL,
        hoverinfo = "text",
        hovertext = paste0(sub$Variavel[i], "<br>Valor: R$ ", scales::comma(sub$Valor[i], big.mark = ".", decimal.mark = ","), "<br>Fonte: ", sub$Fonte[i])
      )
    }
  }
}

# Add VGV as a dashed line across all columns
vgv_val <- df$Valor[df$Variavel == "VGV" & !is.na(df$Valor)]
if (length(vgv_val) == 0 || is.na(vgv_val)) vgv_val <- 0
p <- p %>% add_trace(
  x = natureza_levels,
  y = rep(vgv_val, length(natureza_levels)),
  type = "scatter",
  mode = "lines",
  name = "VGV",
  line = list(color = "black", dash = "dash", width = 2),
  showlegend = TRUE
)

# Add bracket annotation for negative segment in Viabilidade
viab_x <- "Viabilidade"
terreno_val <- df$Valor[df$Natureza == "Viabilidade" & df$Variavel == "Terreno permuta física"]
if (length(terreno_val) > 0 && !is.na(terreno_val)) {
  # Add bracket as a shape
  p <- p %>% layout(
    shapes = list(
      list(
        type = "path",
        path = paste0(
          "M ", 4.05, ",", -abs(terreno_val), " ",
          "L ", 4.18, ",", -abs(terreno_val), " ",
          "L ", 4.18, ",", 0, " ",
          "L ", 4.05, ",", 0
        ),
        line = list(color = "black", width = 2)
      )
    )
  )
}

# Final layout (no duplicate annotation/layout calls)
p <- p %>% layout(
  barmode = "stack",
  title = list(
    text = "Resumo do UP Estação Vila Sônia",
    font = list(size = 20, family = "Arial Black"),
    x = 0.5,
    xanchor = "center"
  ),
  xaxis = list(
    title = NULL,
    categoryorder = "array",
    categoryarray = natureza_levels,
    showticklabels = TRUE
  ),
  yaxis = list(title = NULL, tickformat = ",.0f"),
  legend = list(orientation = "v")
)

# Add minimalistic value labels for each segment (autoadjusting box using annotation bgcolor, no width/height)
value_annotations <- list()
for (natureza in natureza_levels) {
  sub <- bar_df %>%
    filter(Natureza == natureza) %>%
    arrange(stack_id)
  y0 <- 0
  if (natureza == "Viabilidade" && any(sub$Variavel == "Terreno permuta física")) {
    terreno_idx <- which(sub$Variavel == "Terreno permuta física")
    # Negative segment (do NOT add value label for this one)
    y0 <- y0 - abs(sub$Valor[terreno_idx])
    for (i in seq_len(nrow(sub))) {
      if (i != terreno_idx) {
        y1 <- y0 + sub$Valor[i] / 2
        value_annotations[[length(value_annotations) + 1]] <- list(
          x = natureza,
          y = y1,
          text = scales::comma(sub$Valor[i], big.mark = ".", decimal.mark = ","),
          showarrow = FALSE,
          font = list(color = "#222", size = 9, family = "Arial"),
          xanchor = "center",
          yanchor = "middle",
          bgcolor = "white",
          bordercolor = "#e0e0e0",
          borderwidth = 1,
          opacity = 1
        )
        y0 <- y0 + sub$Valor[i]
      }
    }
  } else if (nrow(sub) > 0) {
    for (i in seq_len(nrow(sub))) {
      y1 <- y0 + sub$Valor[i] / 2
      value_annotations[[length(value_annotations) + 1]] <- list(
        x = natureza,
        y = y1,
        text = scales::comma(sub$Valor[i], big.mark = ".", decimal.mark = ","),
        showarrow = FALSE,
        font = list(color = "#222", size = 9, family = "Arial"),
        xanchor = "center",
        yanchor = "middle",
        bgcolor = "white",
        bordercolor = "#e0e0e0",
        borderwidth = 1,
        opacity = 1
      )
      y0 <- y0 + sub$Valor[i]
    }
  }
}

# --- Only keep value labels, remove all explaining labels and curly brackets except for Terreno permuta física ---
# Remove all curly bracket code and explaining labels except for Terreno permuta física
bracket_shapes <- list()
bracket_annos <- list()
if (length(terreno_val) > 0 && !is.na(terreno_val)) {
  y_min <- -abs(terreno_val)
  y_max <- 0
  x_brace <- 4.15
  y_mid <- (y_min + y_max) / 2
  curly_brace_path <- paste0(
    "M ", x_brace, ",", y_min, " ",
    "Q ", x_brace + 0.08, ",", (y_min + y_mid) / 2, " ", x_brace, ",", y_mid, " ",
    "Q ", x_brace - 0.08, ",", (y_mid + y_max) / 2, " ", x_brace, ",", y_max
  )
  bracket_shapes[[1]] <- list(
    type = "path",
    path = curly_brace_path,
    line = list(color = "black", width = 2)
  )
  bracket_annos[[1]] <- list(
    x = x_brace + 0.07,
    y = y_mid,
    xref = "x",
    yref = "y",
    text = "Terreno permuta física",
    showarrow = FALSE,
    font = list(color = "black", size = 10, family = "Arial"),
    align = "left",
    xanchor = "left",
    yanchor = "middle",
    bgcolor = "white",
    bordercolor = "#e0e0e0",
    borderwidth = 1,
    opacity = 1
  )
}

# Add value labels for each segment, but only the terreno curly bracket/label as explanation
p <- p %>% layout(
  shapes = bracket_shapes,
  annotations = c(value_annotations, bracket_annos),
  bargap = 0.65
)
p
