#' Plot temporal coverage heatmap for arquivos
#'
#' @param cobertura_t Tibble with columns: arquivo, empresa, periodo.inicio, periodo.fim, arquivo.tipo
#' @return Plotly heatmap object
#' @import dplyr, tidyr, lubridate, plotly, purrr
#' @export
plot_cobertura_temporal_heatmap <- function(cobertura_t = g_cobertura.temporal.arquivos()) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(plotly)
  library(purrr)

  # --- Helper Function: Prepare and Clean Data ---
  .prepare_heatmap_data <- function(raw_data) {
    required_cols <- c("arquivo.tipo", "empresa", "periodo.inicio", "periodo.fim", "arquivo")
    if (!all(required_cols %in% names(raw_data))) {
      missing_cols <- required_cols[!required_cols %in% names(raw_data)]
      stop(paste("Input \\'cobertura_t\\' is missing required columns:", paste(missing_cols, collapse=", ")))
    }

    # Initial cleaning and type conversion
    clean_data <- raw_data %>%
      mutate(
        periodo.inicio_parsed = ymd_hms(periodo.inicio, truncated = 3, quiet = TRUE),
        periodo.fim_parsed = ymd_hms(periodo.fim, truncated = 3, quiet = TRUE),
        periodo.inicio = if_else(is.na(periodo.inicio_parsed), ymd(periodo.inicio, quiet = TRUE), as.Date(periodo.inicio_parsed)),
        periodo.fim = if_else(is.na(periodo.fim_parsed), ymd(periodo.fim, quiet = TRUE), as.Date(periodo.fim_parsed)),
        arquivo.tipo = trimws(as.character(arquivo.tipo)),
        empresa = trimws(as.character(empresa))
      ) %>%
      select(-periodo.inicio_parsed, -periodo.fim_parsed) %>%
      filter(
        !is.na(arquivo.tipo) & !arquivo.tipo %in% c("", "0", "0-"),
        !is.na(empresa) & !empresa %in% c("", "0", "0-"),
        !is.na(periodo.inicio), !is.na(periodo.fim),
        periodo.inicio <= periodo.fim,
        periodo.inicio >= as.Date("2000-01-01"), # Filter out very old/invalid dates
        periodo.fim >= as.Date("2000-01-01")
      )

    if (nrow(clean_data) == 0) {
      if (interactive()) message("No valid data after initial filtering.")
      return(NULL)
    }

    # Expand to months
    expanded_data <- clean_data %>%
      rowwise() %>%
      do({
        row_df <- .
        months_seq <- seq(floor_date(row_df$periodo.inicio, "month"), floor_date(row_df$periodo.fim, "month"), by = "month")
        if (length(months_seq) == 0) tibble()
        else tibble(
          arquivo = row_df$arquivo,
          empresa = row_df$empresa,
          arquivo.tipo = row_df$arquivo.tipo,
          month_date = months_seq, # Keep as Date object
          periodo.inicio = row_df$periodo.inicio,
          periodo.fim = row_df$periodo.fim
        )
      }) %>%
      ungroup()

    if (nrow(expanded_data) == 0) {
      if (interactive()) message("No data after expanding to months.")
      return(NULL)
    }

    # Summarize coverage
    agg_data <- expanded_data %>%
      mutate(
        month_start = floor_date(month_date, "month"),
        month_end = ceiling_date(month_date, "month") - days(1),
        full_month_coverage = (periodo.inicio <= month_start & periodo.fim >= month_end)
      ) %>%
      group_by(arquivo.tipo, empresa, month_date) %>%
      summarise(
        n_paths = n(),
        n_full = sum(full_month_coverage),
        .groups = "drop"
      ) %>%
      mutate(
        n_partial = n_paths - n_full,
        color_code = case_when(
          n_paths == 0 ~ "empty",
          n_full > 1 ~ "multiple",
          n_full == 1 & n_partial == 0 ~ "full",
          n_full == 1 & n_partial > 0 ~ "partial",
          n_full == 0 & n_partial > 0 ~ "partial",
          TRUE ~ "other"
        )
      ) %>%
      filter(
        !is.na(arquivo.tipo) & !arquivo.tipo %in% c("", "0", "0-"),
        !is.na(empresa) & !empresa %in% c("", "0", "0-"),
        !is.na(month_date) & month_date >= as.Date("2000-01-01")
      )

    if (nrow(agg_data) == 0) {
      if (interactive()) message("No data after aggregation and final filtering.")
      return(NULL)
    }

    valid_row_pairs <- agg_data %>%
      distinct(arquivo.tipo, empresa) %>%
      filter(!arquivo.tipo %in% c("", "0", "0-", NA) & !empresa %in% c("", "0", "0-", NA))

    if(nrow(valid_row_pairs) == 0) {
        if(interactive()) message("No valid (arquivo.tipo, empresa) pairs after filtering agg_data.")
        return(NULL)
    }

    row_keys <- valid_row_pairs %>%
      mutate(label = paste0(arquivo.tipo, " | ", empresa)) %>%
      pull(label) %>%
      unique() %>%
      sort() # Reverted to ascending sort

    if (length(row_keys) == 0) {
      if (interactive()) message("No row keys generated.")
      return(NULL)
    }

    valid_months_dates <- agg_data %>%
      pull(month_date) %>%
      unique() %>%
      sort() %>%
      as.Date(origin="1970-01-01")

    valid_months_dates <- valid_months_dates[!is.na(valid_months_dates) & valid_months_dates >= as.Date("2000-01-01")]

    if (length(valid_months_dates) == 0) {
      if (interactive()) message("No valid month dates found in agg_data.")
      return(NULL)
    }

    formatted_months <- format(valid_months_dates, "%Y-%m")

    unique_formatted_months_map <- data.frame(
        formatted = formatted_months,
        original_date = valid_months_dates
    ) %>% distinct(formatted, .keep_all = TRUE)

    final_formatted_months <- unique_formatted_months_map$formatted
    final_month_dates <- unique_formatted_months_map$original_date


    if (length(final_formatted_months) == 0) {
      if (interactive()) message("No formatted month keys generated.")
      return(NULL)
    }

    if (interactive()) {
        message("--- POST-PREPARE DIAGNOSTICS ---")
        message(paste("Number of row_keys:", length(row_keys)))
        message(paste("Number of final_formatted_months:", length(final_formatted_months)))
        message(paste("Number of final_month_dates:", length(final_month_dates)))
    }

    return(list(
      agg_data = agg_data,
      row_keys = row_keys,
      formatted_months = final_formatted_months,
      month_dates = final_month_dates
    ))
  }

  # --- Helper Function: Create Heatmap Matrix, Colors, Text ---
  .create_heatmap_elements <- function(prepared_data) {
    agg <- prepared_data$agg_data
    row_keys <- prepared_data$row_keys
    formatted_months <- prepared_data$formatted_months
    month_dates <- prepared_data$month_dates

    mat <- matrix(NA_character_,
                  nrow = length(row_keys),
                  ncol = length(formatted_months),
                  dimnames = list(row_keys, formatted_months))

    for (r_idx in seq_along(row_keys)) {
      current_row_key <- row_keys[r_idx]
      key_parts <- strsplit(current_row_key, " | ", fixed = TRUE)[[1]]
      tipo <- key_parts[1]
      emp <- key_parts[2]

      for (c_idx in seq_along(formatted_months)) {
        current_month_date <- month_dates[c_idx]

        cell_data <- agg %>%
          filter(arquivo.tipo == tipo, empresa == emp, month_date == current_month_date)

        if (nrow(cell_data) == 0 || is.na(cell_data$color_code[1])) {
          mat[r_idx, c_idx] <- "empty"
        } else {
          mat[r_idx, c_idx] <- cell_data$color_code[1]
        }
      }
    }

    color_map <- c(
      "empty"    = "lightgray",
      "partial"  = "yellow",
      "multiple" = "red",
      "full"     = "#b7e3b7",
      "other"    = "magenta"
    )

    full_coverage_details <- agg %>%
      filter(color_code == "full" & n_paths > 0 & !is.na(n_paths)) %>%
      select(arquivo.tipo, empresa, month_date, n_paths)

    if (nrow(full_coverage_details) > 0) {
      max_val <- max(full_coverage_details$n_paths, na.rm = TRUE)
      if (is.finite(max_val) && max_val > 0) {
        green_palette <- colorRampPalette(c("#b7e3b7", "#006400"))(max_val)

        for (i in 1:nrow(full_coverage_details)) {
          detail <- full_coverage_details[i, ]
          row_label <- paste0(detail$arquivo.tipo, " | ", detail$empresa)
          month_label <- format(detail$month_date, "%Y-%m")

          r_idx <- which(row_keys == row_label)
          c_idx <- which(formatted_months == month_label)

          if (length(r_idx) == 1 && length(c_idx) == 1 && detail$n_paths > 0) {
            specific_full_code <- paste0("full_", detail$n_paths)
            mat[r_idx, c_idx] <- specific_full_code
            if (detail$n_paths <= max_val) {
                 color_map[specific_full_code] <- green_palette[detail$n_paths]
            } else {
                 color_map[specific_full_code] <- green_palette[max_val]
            }
          }
        }
      }
    }

    unique_mat_values <- unique(as.vector(mat))
    for (val in unique_mat_values) {
      if (!val %in% names(color_map)) {
        if(interactive()) message(paste("Warning: Mat value \\'", val, "\\' not in color_map. Assigning magenta.", sep=""))
        color_map[val] <- "magenta"
      }
    }

    ordered_color_names <- names(color_map)
    z <- matrix(match(mat, ordered_color_names),
                nrow = nrow(mat),
                ncol = ncol(mat),
                dimnames = NULL)

    text_matrix <- matrix("", nrow = length(row_keys), ncol = length(formatted_months))
     for (r_idx in seq_along(row_keys)) {
      current_row_key <- row_keys[r_idx]
      key_parts <- strsplit(current_row_key, " | ", fixed = TRUE)[[1]]
      tipo <- key_parts[1]
      emp <- key_parts[2]

      for (c_idx in seq_along(formatted_months)) {
        current_month_date <- month_dates[c_idx]
        cell_data <- agg %>%
          filter(arquivo.tipo == tipo, empresa == emp, month_date == current_month_date)

        if (nrow(cell_data) > 0) {
          text_matrix[r_idx, c_idx] <- paste0(
            "Tipo: ", tipo, "<br>",
            "Empresa: ", emp, "<br>",
            "Mês: ", format(current_month_date, "%Y-%m"), "<br>",
            "Status: ", mat[r_idx, c_idx], "<br>",
            "Nº arquivos: ", cell_data$n_paths[1]
          )
        } else {
           text_matrix[r_idx, c_idx] <- paste0(
            "Tipo: ", tipo, "<br>",
            "Empresa: ", emp, "<br>",
            "Mês: ", format(current_month_date, "%Y-%m"), "<br>",
            "Status: empty<br>",
            "Nº arquivos: 0"
          )
        }
      }
    }

    return(list(
      z = z,
      text_matrix = text_matrix,
      color_map = color_map,
      ordered_color_names = ordered_color_names
    ))
  }

  # --- Helper Function: Generate Plotly Figure ---
  .generate_plotly_figure <- function(plot_elements, row_keys, formatted_months) {
    z <- plot_elements$z
    text_matrix <- plot_elements$text_matrix
    color_map <- plot_elements$color_map # Available for heatmap_colorscale

    y_axis_labels <- factor(row_keys, levels = row_keys)
    x_axis_labels <- factor(formatted_months, levels = formatted_months)

    heatmap_ordered_color_names <- plot_elements$ordered_color_names
    heatmap_colorscale <- vector("list", length(heatmap_ordered_color_names))
    if (length(heatmap_ordered_color_names) > 0) {
      for(i in seq_along(heatmap_ordered_color_names)){
          scale_val <- (i-1) / max(1, (length(heatmap_ordered_color_names)-1))
          heatmap_colorscale[[i]] <- list(scale_val, color_map[[ heatmap_ordered_color_names[i] ]])
      }
      if (length(heatmap_ordered_color_names) == 1) { # Single color case
          heatmap_colorscale <- list(list(0, color_map[[heatmap_ordered_color_names[1]]]),
                                   list(1, color_map[[heatmap_ordered_color_names[1]]]))
      }
    } else { # Fallback if no colors somehow (should not happen if data exists)
        heatmap_colorscale <- list(list(0, "lightgray"), list(1, "lightgray"))
    }

    # Initialize plot
    p <- plot_ly()

    # --- Define and Add Discrete Legend Items ---
    legend_definitions <- list(
      list(id = "empty",    name = "Vazio",                base_color_key = "empty"),
      list(id = "partial",  name = "Parcial",              base_color_key = "partial"),
      list(id = "multiple", name = "Múltiplo",             base_color_key = "multiple"),
      list(id = "full",     name = "Completo (Base)",      base_color_key = "full") # Represents the base 'full' color
    )

    legend_group_name <- "cobertura_status_legend"

    for (leg_def in legend_definitions) {
      item_color <- color_map[[leg_def$base_color_key]]

      if (!is.null(item_color) && !is.na(item_color)) {
        p <- add_trace(p,
          type = "scatter",
          mode = "markers",
          x = list(NA), # Use list(NA) for legend-only items
          y = list(NA), # Use list(NA) for legend-only items
          name = leg_def$name,
          marker = list(color = item_color, symbol = "square", size = 10),
          legendgroup = legend_group_name,
          showlegend = TRUE,
          hoverinfo = "none",
          inherit = FALSE
        )
      } else {
        if (interactive()) {
            message(paste("Legend item '", leg_def$name, "' skipped, color not found for key '", leg_def$base_color_key, "'."))
        }
      }
    }

    # Add the heatmap trace
    p <- add_trace(p,
      x = x_axis_labels,
      y = y_axis_labels,
      z = z,
      type = "heatmap",
      colorscale = heatmap_colorscale,
      zmin = if (length(heatmap_ordered_color_names) > 0) 1 else NULL,
      zmax = if (length(heatmap_ordered_color_names) > 0) length(heatmap_ordered_color_names) else NULL,
      text = text_matrix,
      hoverinfo = "text",
      xgap = 0.5, ygap = 0.5,
      showscale = FALSE,    # Hide the continuous colorscale bar
      showlegend = FALSE    # The heatmap itself does not add to the discrete legend
    )

    # Apply layout
    p <- layout(p,
      title = "Cobertura Temporal dos Arquivos",
      xaxis = list(
        title = "Mês",
        type = "category",
        categoryorder = "array",
        categoryarray = levels(x_axis_labels), # Sorted month strings
        showgrid = FALSE,
        tickangle = -45
      ),
      yaxis = list(
        title = "Tipo de Arquivo | Empresa",
        type = "category",
        categoryorder = "array", # Use the order from categoryarray
        categoryarray = levels(y_axis_labels), # Sorted row keys (A-Z)
        autorange = "reversed", # Changed from TRUE to "reversed"
        showgrid = FALSE
      ),
      legend = list(
        title = list(text = "<b>Cobertura</b>"),
        orientation = "v",
        traceorder = "normal",
        bgcolor = "rgba(250, 250, 250, 0.8)", # Light background
        bordercolor = "rgba(100, 100, 100, 0.6)", # Border
        borderwidth = 1
      ),
      showlegend = TRUE # Global switch to ensure legend area is displayed
    )

    return(p)
  }

  # --- Main Workflow ---
  if (interactive()) message("Starting heatmap generation...")
  prepared_data <- .prepare_heatmap_data(cobertura_t)

  if (is.null(prepared_data) || length(prepared_data$row_keys) == 0 || length(prepared_data$formatted_months) == 0) {
    if (interactive()) message("Insufficient data to generate heatmap. Returning empty plot.")
    return(plot_ly() %>% layout(title = "No data available to display.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  if (interactive()) {
    message("--- Data prepared successfully. Proceeding to create heatmap elements. ---")
    message(paste("Row keys for matrix:", length(prepared_data$row_keys)))
    message(paste("Formatted months for matrix:", length(prepared_data$formatted_months)))
  }

  plot_elements <- .create_heatmap_elements(prepared_data)

  if (is.null(plot_elements$z)) {
      if (interactive()) message("Failed to create heatmap matrix (z). Returning empty plot.")
      return(plot_ly() %>% layout(title = "Error in matrix generation.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  if (interactive()) message("--- Heatmap elements created. Generating Plotly figure. ---")

  final_plot <- .generate_plotly_figure(plot_elements, prepared_data$row_keys, prepared_data$formatted_months)

  if (interactive()) message("--- Plotly figure generated. ---")
  return(final_plot)
}

# Example usage (ensure g_cobertura.temporal.arquivos() is available and returns data):
# if (interactive()) {
#   # sample_data <- tibble::tribble(
#   #  ~arquivo, ~empresa, ~periodo.inicio, ~periodo.fim, ~arquivo.tipo,
#   #  "file1.csv", "EMP A", "2023-01-01", "2023-03-15", "Type1",
#   #  "file2.csv", "EMP A", "2023-02-10", "2023-02-20", "Type1",
#   #  "file3.csv", "EMP B", "2023-03-01", "2023-03-31", "Type1",
#   #  "file4.csv", "EMP A", "2023-01-05", "2023-01-15", "Type2",
#   #  "file5.csv", "0-INV", "2023-01-01", "2023-01-31", "TypeX",
#   #  "file6.csv", "EMP C", "1999-01-01", "1999-01-31", "TypeY"
#   # )
#   # plot_cobertura_temporal_heatmap(sample_data)
# }

# Add to global variables to avoid R CMD check notes for NSE in dplyr
utils::globalVariables(c(".", "periodo.inicio_parsed", "periodo.fim_parsed", "month_date",
                         "month_start", "month_end", "full_month_coverage", "n_paths",
                         "n_full", "n_partial", "color_code", "label", "original_date"))
