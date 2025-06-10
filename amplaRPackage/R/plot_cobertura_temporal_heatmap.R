#' Plot temporal coverage heatmap for arquivos
#'
#' @param cobertura_t Tibble with columns: arquivo, empresa, periodo.inicio, periodo.fim, arquivo.tipo
#' @return Plotly heatmap object
#' @import dplyr, tidyr, lubridate, plotly, purrr
plot_cobertura_temporal_heatmap <- function(cobertura_t = g_cobertura.temporal.arquivos()) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(plotly)
  library(purrr)

  # Ensure essential columns exist
  required_cols <- c("arquivo.tipo", "empresa", "periodo.inicio", "periodo.fim", "arquivo")
  if (!all(required_cols %in% names(cobertura_t))) {
    missing_cols <- required_cols[!required_cols %in% names(cobertura_t)]
    stop(paste("Input 'cobertura_t' is missing required columns:", paste(missing_cols, collapse=", ")))
  }

  # Convert to Date and filter invalid/NA dates and identifiers early
  cobertura_t <- cobertura_t %>%
    mutate(
      # Attempt to parse periodo.inicio and periodo.fim, trying datetime then date
      periodo.inicio_parsed = ymd_hms(periodo.inicio, truncated = 3, quiet = TRUE),
      periodo.fim_parsed = ymd_hms(periodo.fim, truncated = 3, quiet = TRUE),
      periodo.inicio = if_else(is.na(periodo.inicio_parsed), ymd(periodo.inicio, quiet = TRUE), as.Date(periodo.inicio_parsed)),
      periodo.fim = if_else(is.na(periodo.fim_parsed), ymd(periodo.fim, quiet = TRUE), as.Date(periodo.fim_parsed)),
      # Ensure arquivo.tipo and empresa are characters for consistent filtering and trim whitespace
      arquivo.tipo = trimws(as.character(arquivo.tipo)),
      empresa = trimws(as.character(empresa))
    ) %>%
    select(-periodo.inicio_parsed, -periodo.fim_parsed) %>% # remove temporary parsing columns
    filter(
      !is.na(arquivo.tipo) & arquivo.tipo != '' & arquivo.tipo != '0', # Uses already trimmed versions
      !is.na(empresa) & empresa != '' & empresa != '0',         # Uses already trimmed versions
      !is.na(periodo.inicio), !is.na(periodo.fim),
      periodo.inicio <= periodo.fim, # Ensure start is not after end
      periodo.inicio >= as.Date('2000-01-01'), # Filter out very old start dates
      periodo.fim >= as.Date('2000-01-01')     # Filter out very old end dates
    )

  # If cobertura_t is empty after initial filtering, return empty plot
  if (nrow(cobertura_t) == 0) {
    return(plot_ly() %>% layout(title = "No valid data after initial filtering.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  # 1. Expand each row to all months it covers
  expand_to_months <- function(row) {
    seq_months <- seq(floor_date(row$periodo.inicio, 'month'), floor_date(row$periodo.fim, 'month'), by = 'month')
    tibble(
      arquivo = row$arquivo,
      empresa = as.character(row$empresa), # Ensure character
      arquivo.tipo = as.character(row$arquivo.tipo), # Ensure character
      month = seq_months,
      periodo.inicio = row$periodo.inicio,
      periodo.fim = row$periodo.fim
    )
  }
  expanded <- cobertura_t %>%
    rowwise() %>%
    do(expand_to_months(.)) %>%
    ungroup()

  if (nrow(expanded) == 0) {
    return(plot_ly() %>% layout(title = "No data after expanding to months.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  # 2. For each arquivo.tipo, empresa, month, get all paths and their coverage
  expanded <- expanded %>%
    mutate(
      month_start = floor_date(month, 'month'),
      month_end = ceiling_date(month, 'month') - days(1),
      full_month = (periodo.inicio <= month_start & periodo.fim >= month_end)
    )

  # 3. Summarize per arquivo.tipo, empresa, month
  summary <- expanded %>%
    group_by(arquivo.tipo, empresa, month) %>%
    summarise(
      n_paths = n(),
      n_full = sum(full_month),
      n_partial = n_paths - n_full,
      arquivos = paste(unique(arquivo), collapse = "; "),
      .groups = 'drop'
    )

  # 4. Determine color code per cell
  summary <- summary %>%
    mutate(
      color_code = case_when(
        n_paths == 0 ~ 'empty',
        n_full == 0 & n_partial > 0 ~ 'partial',
        n_full > 1 ~ 'multiple',
        n_full == 1 & n_partial == 0 & n_paths == 1 ~ 'full',
        n_full == 1 & n_partial == 0 & n_paths > 1 ~ 'multiple',
        n_full == 1 & n_partial > 0 ~ 'partial',
        TRUE ~ 'other'
      )
    )

  # 5. For each (arquivo.tipo, empresa), month
  agg <- summary %>%
    group_by(arquivo.tipo, empresa, month) %>%
    summarise(
      n_paths = sum(n_paths),
      n_full = sum(n_full),
      n_partial = sum(n_partial),
      n_multiple = sum(n_paths > 1),
      color_code = case_when(
        n_paths == 0 ~ 'empty',
        n_multiple > 0 ~ 'multiple',
        n_partial > 0 ~ 'partial',
        n_full > 0 ~ 'full',
        TRUE ~ 'other'
      ),
      .groups = 'drop'
    ) %>%
    mutate( # Ensure character types and trim whitespace
      arquivo.tipo = trimws(as.character(arquivo.tipo)),
      empresa = trimws(as.character(empresa))
    )

  # FINAL FILTER: Remove any rows with invalid arquivo.tipo, empresa, or month before building the matrix
  agg <- agg %>%
    filter(!is.na(arquivo.tipo) & arquivo.tipo != '' & arquivo.tipo != '0',
           !is.na(empresa) & empresa != '' & empresa != '0',
           !is.na(month) & month >= as.Date('2000-01-01'))

  # --- DIAGNOSTIC PRINTS --- START ---
  if (interactive()) {
    message("--- Aggregated data after final filtering (head) ---")
    print(head(agg))
    message("--- Unique arquivo.tipo in filtered agg ---")
    print(unique(agg$arquivo.tipo))
    message("--- Unique empresa in filtered agg ---")
    print(unique(agg$empresa))
    message("--- Range of month in filtered agg ---")
    if (nrow(agg) > 0) print(range(agg$month, na.rm = TRUE)) else message("Agg is empty after final filter")
  }
  # --- DIAGNOSTIC PRINTS --- END ---

  # If agg is empty after final filtering, return empty plot
  if (nrow(agg) == 0) {
    return(plot_ly() %>% layout(title = "No valid data for aggregation.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  # Only keep (arquivo.tipo, empresa) pairs with at least one path and valid names (repeat after final filter)
  valid_pairs <- agg %>%
    group_by(arquivo.tipo, empresa) %>%
    summarise(total_paths = sum(n_paths), .groups = 'drop') %>%
    filter(total_paths > 0) %>%
    select(arquivo.tipo, empresa)

  # --- DIAGNOSTIC PRINTS --- START ---
  if (interactive()) {
    message("--- Valid pairs (head) ---")
    print(head(valid_pairs))
    message("--- Unique arquivo.tipo in valid_pairs ---")
    print(unique(valid_pairs$arquivo.tipo))
    message("--- Unique empresa in valid_pairs ---")
    print(unique(valid_pairs$empresa))
  }
  # --- DIAGNOSTIC PRINTS --- END ---

  if (nrow(valid_pairs) == 0) {
    return(plot_ly() %>% layout(title = "No valid (arquivo.tipo, empresa) pairs to display.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  pair_labels <- valid_pairs %>% mutate(label = paste0(arquivo.tipo, ' | ', empresa))
  row_keys <- pair_labels$label

  # --- DIAGNOSTIC PRINTS --- START ---
  if (interactive()) {
    message("--- Row keys for heatmap (first 20) ---")
    print(head(row_keys, 20))
  }
  # --- DIAGNOSTIC PRINTS --- END ---

  # Derive months from the filtered agg, ensuring they are valid
  months_all <- agg$month %>% # agg is already filtered for month >= 2000-01-01
                unique() %>%
                sort()

  non_empty_months_data <- agg %>%
    filter(color_code != 'empty') # agg is already filtered for valid months

  if(nrow(non_empty_months_data) > 0) {
    min_month_val <- min(non_empty_months_data$month, na.rm = TRUE)
    months <- months_all[months_all >= min_month_val]
  } else {
    months <- months_all # Use all valid months from agg if no "non-empty", or empty if months_all is empty
  }

  # Ensure months are not NA and are valid
  months <- months[!is.na(months) & months >= as.Date('2000-01-01')]

  # --- DIAGNOSTIC PRINTS --- START ---
  if (interactive()) {
    message("--- Months for heatmap (first 20 formatted) ---")
    print(head(format(months, "%Y-%m"), 20))
    message(paste("Number of row_keys:", length(row_keys)))
    message(paste("Number of months:", length(months)))
  }
  # --- DIAGNOSTIC PRINTS --- END ---

  # Final check before matrix creation
  if (length(row_keys) == 0 || length(months) == 0) {
    return(plot_ly() %>% layout(title = "No data to display after final processing.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  # 6. Build matrix for heatmap
  mat <- matrix(NA_character_, nrow = length(row_keys), ncol = length(months),
                dimnames = list(row_keys, format(months, '%Y-%m')))
  for (i in seq_along(row_keys)) {
    tipo <- pair_labels$arquivo.tipo[i]
    emp  <- pair_labels$empresa[i]
    for (j in seq_along(months)) {
      row <- agg %>% filter(arquivo.tipo == tipo, empresa == emp, month == months[j])
      if (nrow(row) == 0) {
        mat[i, j] <- 'empty'
      } else {
        mat[i, j] <- row$color_code[1]
      }
    }
  }

  # 7. Discrete color scale
  color_map <- c(
    'empty' = 'lightgray',
    'partial' = 'yellow',
    'multiple' = 'red',
    'full' = '#b7e3b7' # Base for 'full', might be replaced by specific full_n
  )
  # For green ramp, count number of paths for full cells
  full_counts <- agg %>% filter(color_code == 'full') # agg is already heavily filtered
  # Ensure n_paths is numeric for max()
  full_counts <- full_counts %>% mutate(n_paths = as.numeric(n_paths))

  # Initialize color_map here, it will be modified
  color_map <- c(
    'empty' = 'lightgray',
    'partial' = 'yellow',
    'multiple' = 'red',
    'full' = '#b7e3b7' # Base for 'full', might be replaced by specific full_n
  )

  if (nrow(full_counts) > 0 && any(!is.na(full_counts$n_paths))) {
    max_n_paths <- max(full_counts$n_paths[full_counts$n_paths > 0], na.rm = TRUE) # Ensure positive paths
    if (is.finite(max_n_paths) && max_n_paths > 0) {
        green_ramp <- colorRampPalette(c('#b7e3b7', '#006400'))(max_n_paths)
        for (k in seq_len(nrow(full_counts))) {
            current_arquivo_tipo <- trimws(as.character(full_counts$arquivo.tipo[k]))
            current_empresa <- trimws(as.character(full_counts$empresa[k]))
            current_month_formatted <- format(full_counts$month[k], '%Y-%m')
            current_n_paths <- full_counts$n_paths[k]

            if (is.na(current_arquivo_tipo) || current_arquivo_tipo == '' ||
                is.na(current_empresa) || current_empresa == '' ||
                is.na(current_month_formatted) || is.na(current_n_paths) || current_n_paths <= 0) {
                if(interactive()) message(paste("Skipping green ramp due to NA/invalid data for row", k, "of full_counts:", current_arquivo_tipo, current_empresa, current_month_formatted, current_n_paths))
                next
            }

            current_label <- paste0(current_arquivo_tipo, ' | ', current_empresa)
            idx <- which(row_keys == current_label)
            jdx <- which(format(months, '%Y-%m') == current_month_formatted)

            if (length(idx) == 1 && length(jdx) == 1) {
                mat[idx, jdx] <- paste0('full_', current_n_paths)
                # Define the color for this specific 'full_N'
                if (current_n_paths > 0 && current_n_paths <= length(green_ramp)) {
                    color_map[paste0('full_', current_n_paths)] <- green_ramp[current_n_paths]
                } else if (current_n_paths > 0) { # If n_paths > max_n_paths (e.g. due to data change), use darkest green
                    color_map[paste0('full_', current_n_paths)] <- green_ramp[length(green_ramp)]
                }
                # If 'full' (the generic one) is still in mat[idx,jdx] because n_paths was 0 or NA, it will use the default green
            } else {
                if(interactive()){
                    message(paste("Skipping green ramp for:", current_label, "month:", current_month_formatted, "n_paths:", current_n_paths, "- idx/jdx not found or multiple."))
                    message(paste("idx length:", length(idx), "jdx length:", length(jdx)))
                }
            }
        }
    } else {
        if(interactive()) message("No valid positive n_paths found in full_counts for green ramp.")
    }
  } else {
      if(interactive()) message("No 'full' coverage rows or no valid n_paths in them for green ramp.")
  }

  # 8. Prepare z and text for plotly
  unique_mat_values <- unique(as.vector(mat))
  missing_keys <- setdiff(unique_mat_values, names(color_map))

  if (length(missing_keys) > 0) {
    if (interactive()) {
      message("--- Mat values not found in color_map, assigning debug color 'magenta' ---")
      print(missing_keys)
    }
    for(key_val in missing_keys) { # Renamed loop variable
      if (!is.na(key_val)) { # Ensure key_val is not NA before assigning
         color_map[key_val] <- 'magenta'
      }
    }
  }

  # Ensure 'full' (generic) is in color_map if it's still in mat and not replaced by full_N
  if (!('full' %in% names(color_map)) && 'full' %in% unique_mat_values) {
    color_map['full'] <- '#b7e3b7' # Default light green
  }


  z <- matrix(match(mat, names(color_map)), nrow = nrow(mat), ncol = ncol(mat))

  if (any(is.na(z)) && interactive()) {
    message("--- NAs found in z matrix! Problematic mat values: ---")
    print(unique(mat[is.na(z)]))
    message("--- Current color_map names: ---")
    print(names(color_map))
  }
  text <- matrix('', nrow = nrow(mat), ncol = ncol(mat))
  for (i in seq_along(row_keys)) {
    tipo <- pair_labels$arquivo.tipo[i]
    emp  <- pair_labels$empresa[i]
    for (j in seq_along(months)) {
      row <- agg %>% filter(arquivo.tipo == tipo, empresa == emp, month == months[j])
      if (nrow(row) > 0) {
        text[i, j] <- paste0(
          'Arquivo tipo: ', tipo, '<br>',
          'Empresa: ', emp, '<br>',
          'Mês: ', format(months[j], '%Y-%m'), '<br>',
          'Nº arquivos: ', row$n_paths, '<br>',
          'Cobertura: ', row$color_code
        )
      }
    }
  }

  # 9. Plotly heatmap with grid and custom legend
  final_color_names <- names(color_map)
  final_color_values <- unname(color_map)
  n_colors <- length(final_color_names)

  if (n_colors == 0) { # Prevent error if color_map is empty
    return(plot_ly() %>% layout(title = "Cannot generate plot: No colors defined.", annotations = list(text="No data to display", showarrow=FALSE)))
  }

  if (n_colors == 1) {
    plotly_colorscale <- list(list(0, final_color_values[1]), list(1, final_color_values[1]))
  } else {
    map_values <- seq(0, 1, length.out = n_colors)
    plotly_colorscale <- mapply(function(val, col) list(val, col), map_values, final_color_values, SIMPLIFY = FALSE)
  }

  p <- plot_ly(
    x = format(months, '%Y-%m'),
    y = row_keys,
    z = z,
    type = 'heatmap',
    colorscale = plotly_colorscale,
    zmin = 1,
    zmax = n_colors,
    showscale = FALSE,
    text = text,
    hoverinfo = 'text',
    xgap = 1, ygap = 1,
    showlegend = FALSE
  )

  # Add a custom legend using dummy scatter traces
  # Filter legend items to only those actually present in the 'mat' and thus in 'z'
  # Or, use all items defined in color_map that we want to show
  legend_items_to_show <- c('empty', 'partial', 'multiple', 'full') # Base categories
  # Add specific 'full_N' categories if they exist and we want them explicitly in legend
  # For simplicity, the current legend code iterates through all of color_map

  # Rebuild legend_items and legend_colors from the final color_map used for z
  # This ensures legend matches what's in the plot
  legend_map_ordered <- color_map[final_color_names] # Use the order from final_color_names

  # Custom legend: only show distinct color categories, not every single 'full_N'
  # Define what categories to show in legend
  # For 'full' counts, we might want a single 'full coverage' entry or a few representative ones.
  # For now, let's keep the existing legend logic which shows all entries in color_map.
  # If color_map becomes very large due to many full_N, legend might need adjustment.

  legend_display_names <- names(legend_map_ordered)
  legend_display_colors <- unname(legend_map_ordered)

  for (i in seq_along(legend_display_names)) {
    p <- add_trace(
      p,
      x = 0, y = 0, type = 'scatter', mode = 'markers',
      marker = list(size = 15, color = legend_display_colors[i], symbol = 'square'),
      name = legend_display_names[i],
      showlegend = TRUE,
      legendgroup = legend_display_names[i],
      hoverinfo = 'none',
      inherit = FALSE
    )
  }

  p <- layout(
    p,
    title = 'Cobertura temporal dos arquivos',
    xaxis = list(title = 'Mês',
                 type = 'category',
                 categoryorder = 'array',
                 categoryarray = format(months, '%Y-%m'),
                 showgrid = FALSE,
                 tickangle = -45),
    yaxis = list(title = 'Tipo de arquivo | Empresa', # Updated title
                 type = 'category',
                 categoryorder = 'array',
                 categoryarray = row_keys,
                 showgrid = FALSE),
    legend = list(title = list(text = 'Cobertura'), orientation = 'v')
  )
  p
}
