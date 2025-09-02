#' Plot temporal coverage heatmap for extratos
#'
#' @param cobertura_t Tibble with columns: arquivo, empresa, periodo.inicio, periodo.fim, arquivo.tipo, id
#' @return Plotly heatmap object
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import plotly
#' @import purrr
#' @import stringr
#' @export

g_coberturaExtratos <- function(cobertura_t = e_cobertura_extratos()) {
  # --- Helper Function: Prepare and Clean Data ---
  .prepare_heatmap_data <- function(raw_data) {
    # Verificar se os dados estão vazios
    if (is.null(raw_data) || nrow(raw_data) == 0) {
      return(NULL)
    }

    required_cols <- c("arquivo.tipo", "periodo.inicio", "periodo.fim", "arquivo")
    optional_cols <- c("empresa", "conta", "banco", "id")

    # Adicionar colunas faltantes com valores padrão
    for (col in c(required_cols, optional_cols)) {
      if (!col %in% names(raw_data)) {
        if (col %in% c("periodo.inicio", "periodo.fim")) {
          raw_data[[col]] <- as.Date(NA)
        } else {
          raw_data[[col]] <- NA_character_
        }
      }
    }

    # Verificar apenas colunas essenciais
    if (!all(required_cols %in% names(raw_data))) {
      missing_cols <- required_cols[!required_cols %in% names(raw_data)]
      stop(paste("Input \\'cobertura_t\\' is missing required columns:", paste(missing_cols, collapse = ", ")))
    }

    # Initial cleaning and type conversion
    clean_data <- raw_data %>%
      mutate(
        periodo.inicio_parsed = ymd_hms(periodo.inicio, truncated = 3, quiet = TRUE),
        periodo.fim_parsed = ymd_hms(periodo.fim, truncated = 3, quiet = TRUE),
        periodo.inicio = if_else(is.na(periodo.inicio_parsed), ymd(periodo.inicio, quiet = TRUE), as.Date(periodo.inicio_parsed)),
        periodo.fim = if_else(is.na(periodo.fim_parsed), ymd(periodo.fim, quiet = TRUE), as.Date(periodo.fim_parsed)),
        arquivo.tipo = trimws(as.character(arquivo.tipo)),
        empresa = trimws(as.character(empresa)),
        conta = trimws(as.character(conta)) # Added conta cleaning
      ) %>%
      select(-periodo.inicio_parsed, -periodo.fim_parsed) %>%
      filter(
        !is.na(empresa) & !empresa %in% c("", "0", "0-"),
        !is.na(conta) & !conta %in% c("", "0", "0-"),
        !is.na(banco) & !banco %in% c("", "0", "0-"), # Added banco filter
        !is.na(periodo.inicio), !is.na(periodo.fim),
        periodo.inicio <= periodo.fim,
        periodo.inicio >= as.Date("2000-01-01"), # Filter out very old/invalid dates
        periodo.fim >= as.Date("2000-01-01")
      )

    if (nrow(clean_data) == 0) {
      if (interactive()) message("No valid data after initial filtering.")
      return(NULL)
    }

    # Remove date pattern from descricao if present (robustness for downstream)
    if ("descricao" %in% names(clean_data)) {
      clean_data <- clean_data %>%
        dplyr::mutate(descricao = stringr::str_remove(descricao, "^\\d{1,2}\\s*/\\s*[a-zA-Z]{3}\\s*"))
    }

    # Expand to months
    expanded_data <- clean_data %>%
      rowwise() %>%
      do({
        row_df <- .
        months_seq <- seq(floor_date(row_df$periodo.inicio, "month"), floor_date(row_df$periodo.fim, "month"), by = "month")
        if (length(months_seq) == 0) {
          tibble()
        } else {
          # Always propagate tipo.xcef if present, else NA
          tipo_xcef_val <- if ("tipo.xcef" %in% names(row_df)) row_df$tipo.xcef else if ("arquivo.subtipo" %in% names(row_df)) row_df$arquivo.subtipo else NA_character_
          # Always propagate id if present, else NA
          id_val <- if ("id" %in% names(row_df)) row_df$id else NA_character_
          tibble(
            arquivo = row_df$arquivo,
            empresa = row_df$empresa,
            arquivo.tipo = row_df$arquivo.tipo,
            conta = row_df$conta,
            banco = row_df$banco, # Add banco column
            id = id_val, # Add id column
            month_date = months_seq,
            periodo.inicio = row_df$periodo.inicio,
            periodo.fim = row_df$periodo.fim,
            tipo.xcef = tipo_xcef_val
          )
        }
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
      group_by(empresa, conta, banco, id, month_date) %>%
      summarise(
        n_paths = n(),
        n_full = sum(full_month_coverage),
        arquivos = list(arquivo),
        # Ensure subtipos is always a character vector with no NAs or empty strings
        subtipos = list({
          st <- as.character(tipo.xcef)
          st <- st[!is.na(st) & st != ""]
          if (length(st) == 0 && "arquivo.subtipo" %in% names(cur_data())) {
            st2 <- as.character(cur_data()$arquivo.subtipo)
            st2 <- st2[!is.na(st2) & st2 != ""]
            st <- st2
          }
          if (length(st) == 0) st <- NA_character_
          st
        }),
        .groups = "drop"
      ) %>%
      mutate(
        n_incomplete = n_paths - n_full,
        color_code = case_when(
          n_paths == 0 ~ "empty",
          n_full > 1 ~ "multiple",
          n_full == 1 & n_incomplete == 0 ~ "full",
          n_full == 1 & n_incomplete > 0 ~ "incomplete",
          n_full == 0 & n_incomplete > 0 ~ "incomplete",
          TRUE ~ "other"
        )
      ) %>%
      filter(
        !is.na(empresa) & !empresa %in% c("", "0", "0-"),
        !is.na(conta) & !conta %in% c("", "0", "0-"),
        !is.na(banco) & !banco %in% c("", "0", "0-"), # Added banco filter
        !is.na(month_date) & month_date >= as.Date("2000-01-01")
      )

    if (nrow(agg_data) == 0) {
      if (interactive()) message("No data after aggregation and final filtering.")
      return(NULL)
    }

    # Determine the overall date range for the x-axis (months)
    if (nrow(agg_data) > 0 && any(!is.na(agg_data$month_date))) {
      min_overall_date <- min(agg_data$month_date, na.rm = TRUE)
      max_overall_date <- max(agg_data$month_date, na.rm = TRUE)

      # Create a complete sequence of months from min to max
      all_months_seq <- seq(floor_date(min_overall_date, "month"),
        floor_date(max_overall_date, "month"),
        by = "month"
      )
      all_months_seq <- as.Date(all_months_seq, origin = "1970-01-01") # Ensure it's Date
    } else {
      # Fallback if no valid dates at all, though earlier checks should prevent this
      if (interactive()) message("No valid month_date values to determine overall date range.")
      return(NULL)
    }

    if (length(all_months_seq) == 0) {
      if (interactive()) message("Month sequence is empty after generation.")
      return(NULL)
    }

    valid_row_pairs <- agg_data %>%
      distinct(empresa, conta, banco, id) %>% # Added id to distinct columns
      filter(!empresa %in% c("", "0", "0-", NA) &
        !conta %in% c("", "0", "0-", NA) &
        !banco %in% c("", "0", "0-", NA)) # Only check empresa, conta, banco

    if (nrow(valid_row_pairs) == 0) {
      if (interactive()) message("No valid (empresa, conta, banco, id) pairs after filtering agg_data.")
      return(NULL)
    }

    row_keys <- valid_row_pairs %>%
      mutate(label = paste0(empresa, " | ", banco, " | ", conta)) %>% # Use banco instead of arquivo.tipo
      arrange(id, label) %>% # Sort first by id, then by label (alphabetically)
      pull(label) %>%
      unique() # Remove duplicates while preserving order

    if (length(row_keys) == 0) {
      if (interactive()) message("No row keys generated.")
      return(NULL)
    }

    # Use the complete sequence of all months (all_months_seq) for the x-axis.
    # all_months_seq was generated earlier and is a sequence of Date objects (first day of each month).
    # Filter out any NA or very old dates.
    final_month_dates_temp <- all_months_seq[!is.na(all_months_seq) & all_months_seq >= as.Date("2000-01-01")]

    if (length(final_month_dates_temp) == 0) {
      if (interactive()) message("No valid month dates after creating full sequence from all_months_seq and filtering.")
      return(NULL)
    }

    # Create formatted month strings and ensure a unique, chronologically-ordered mapping
    # between formatted strings and original Date objects.
    month_map_df <- data.frame(original_date = final_month_dates_temp) %>%
      mutate(formatted = format(original_date, "%Y-%m")) %>%
      distinct(formatted, .keep_all = TRUE) %>%
      arrange(original_date)

    final_formatted_months <- month_map_df$formatted
    final_month_dates <- month_map_df$original_date

    # The existing check for length(final_formatted_months) == 0 will catch issues here.
    if (length(final_formatted_months) == 0) {
      if (interactive()) message("No formatted month keys generated.")
      return(NULL)
    }

    return(list(
      agg_data = agg_data,
      row_keys = row_keys,
      formatted_months = final_formatted_months,
      month_dates = final_month_dates
    ))
  }

  # --- Helper Function: Create Heatmap Matrix, Colors, Text ---
  .create_heatmap_elements <- function(prepared_data, status_translation_map) { # Added status_translation_map
    agg <- prepared_data$agg_data
    row_keys <- prepared_data$row_keys
    formatted_months <- prepared_data$formatted_months
    month_dates <- prepared_data$month_dates

    mat <- matrix(NA_character_,
      nrow = length(row_keys),
      ncol = length(formatted_months),
      dimnames = list(row_keys, formatted_months)
    )

    for (r_idx in seq_along(row_keys)) {
      current_row_key <- row_keys[r_idx]
      key_parts <- strsplit(current_row_key, " | ", fixed = TRUE)[[1]]
      emp <- key_parts[1] # empresa first
      banco <- key_parts[2] # banco second (was tipo)
      cta <- key_parts[3] # conta third

      for (c_idx in seq_along(formatted_months)) {
        current_month_date <- month_dates[c_idx]

        cell_data <- agg %>%
          filter(empresa == emp, banco == banco, conta == cta, month_date == current_month_date) # Updated filter

        if (nrow(cell_data) == 0 || is.na(cell_data$color_code[1])) {
          mat[r_idx, c_idx] <- "empty"
        } else {
          mat[r_idx, c_idx] <- cell_data$color_code[1]
        }
      }
    }

    color_map <- c(
      "empty" = "lightgray",
      "incomplete" = "yellow",
      "multiple" = "red",
      "full" = "#5cb85c", # Changed to a more vibrant green
      "other" = "magenta"
    )

    full_coverage_details <- agg %>%
      filter(color_code == "full" & n_paths > 0 & !is.na(n_paths)) %>%
      select(empresa, conta, banco, month_date, n_paths) # Removed arquivo.tipo, keep only empresa, conta, banco

    if (nrow(full_coverage_details) > 0) {
      max_val <- max(full_coverage_details$n_paths, na.rm = TRUE)
      if (is.finite(max_val) && max_val > 0) {
        green_palette <- colorRampPalette(c("#5cb85c", "#006400"))(max_val) # Updated start of green palette

        for (i in 1:nrow(full_coverage_details)) {
          detail <- full_coverage_details[i, ]
          row_label <- paste0(detail$empresa, " | ", detail$banco, " | ", detail$conta) # Use banco instead of arquivo.tipo
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
        if (interactive()) message(paste("Warning: Mat value \\'", val, "\\' not in color_map. Assigning magenta.", sep = ""))
        color_map[val] <- "magenta"
      }
    }

    ordered_color_names <- names(color_map)
    z <- matrix(match(mat, ordered_color_names),
      nrow = nrow(mat),
      ncol = ncol(mat),
      dimnames = NULL
    )

    text_matrix <- matrix("", nrow = length(row_keys), ncol = length(formatted_months))
    for (r_idx in seq_along(row_keys)) {
      current_row_key <- row_keys[r_idx]
      key_parts <- strsplit(current_row_key, " | ", fixed = TRUE)[[1]]
      emp <- key_parts[1] # empresa first
      banco <- key_parts[2] # banco second
      cta <- key_parts[3] # conta third

      for (c_idx in seq_along(formatted_months)) {
        current_month_date <- month_dates[c_idx]
        cell_data <- agg %>%
          filter(empresa == emp, banco == banco, conta == cta, month_date == current_month_date) # Updated filter

        raw_status_code <- mat[r_idx, c_idx]
        base_status_code <- if (startsWith(raw_status_code, "full_")) "full" else raw_status_code

        display_status_name <- status_translation_map[[base_status_code]]
        if (is.null(display_status_name) || is.na(display_status_name)) {
          display_status_name <- base_status_code # Fallback
        }

        # --- NEW: Basenames and arquivo.subtipo counts ---
        if (nrow(cell_data) > 0) {
          arquivos_basenames <- cell_data$arquivos[[1]]
          if (!is.null(arquivos_basenames) && length(arquivos_basenames) > 0) {
            arquivos_basenames <- basename(arquivos_basenames)
            arquivos_basenames_str <- paste(arquivos_basenames, collapse = ", ")
          } else {
            arquivos_basenames_str <- "-"
          }

          # Count subtipos
          subtipos_vec <- cell_data$subtipos[[1]]
          # Only count valid subtipos (non-NA, non-empty)
          subtipos_vec <- as.character(subtipos_vec)
          subtipos_vec <- subtipos_vec[!is.na(subtipos_vec) & subtipos_vec != ""]
          if (length(subtipos_vec) > 0) {
            subtipo_counts <- table(subtipos_vec)
            subtipo_str <- paste(names(subtipo_counts), subtipo_counts, sep = ": ", collapse = ", ")
          } else {
            subtipo_str <- "-"
          }

          text_matrix[r_idx, c_idx] <- paste0(
            "Empresa: ", emp, "<br>",
            "Banco: ", banco, "<br>", # Use banco variable instead of tipo
            "Conta: ", cta, "<br>",
            "Mês: ", format(current_month_date, "%Y-%m"), "<br>",
            "Status: ", display_status_name, "<br>",
            "Nº arquivos: ", cell_data$n_paths[1], "<br>",
            "Arquivos: ", arquivos_basenames_str, "<br>",
            "Subtipos: ", subtipo_str
          )
        } else {
          text_matrix[r_idx, c_idx] <- paste0(
            "Empresa: ", emp, "<br>",
            "Banco: ", banco, "<br>", # Use banco variable instead of tipo
            "Conta: ", cta, "<br>",
            "Mês: ", format(current_month_date, "%Y-%m"), "<br>",
            "Status: ", status_translation_map[["empty"]] %||% "Vazio", "<br>",
            "Nº arquivos: 0<br>",
            "Arquivos: -<br>",
            "Subtipos: -"
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
      for (i in seq_along(heatmap_ordered_color_names)) {
        scale_val <- (i - 1) / max(1, (length(heatmap_ordered_color_names) - 1))
        heatmap_colorscale[[i]] <- list(scale_val, color_map[[heatmap_ordered_color_names[i]]])
      }
      if (length(heatmap_ordered_color_names) == 1) { # Single color case
        heatmap_colorscale <- list(
          list(0, color_map[[heatmap_ordered_color_names[1]]]),
          list(1, color_map[[heatmap_ordered_color_names[1]]])
        )
      }
    } else { # Fallback if no colors somehow (should not happen if data exists)
      heatmap_colorscale <- list(list(0, "lightgray"), list(1, "lightgray"))
    }

    # Initialize plot
    p <- plot_ly()

    # --- Define and Add Discrete Legend Items ---
    legend_definitions <- list(
      list(id = "full", name = "Completo", base_color_key = "full"),
      list(id = "incomplete", name = "Incompleto", base_color_key = "incomplete"),
      list(id = "multiple", name = "Múltiplo", base_color_key = "multiple"),
      list(id = "empty", name = "Vazio", base_color_key = "empty")
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
      hovertemplate = "%{text}<extra></extra>", # Replaced hoverinfo with hovertemplate for better hover control
      xgap = 0.5, ygap = 0.5,
      showscale = FALSE, # Hide the continuous colorscale bar
      showlegend = FALSE # The heatmap itself does not add to the discrete legend
    )

    # --- Add curved arches for rows with same ID ---
    arch_shapes <- list()
    if (exists("prepared_data") && !is.null(prepared_data$agg_data)) {
      # Get ID information for each row
      row_id_map <- prepared_data$agg_data %>%
        distinct(empresa, conta, banco, id) %>%
        mutate(label = paste0(empresa, " | ", banco, " | ", conta)) %>%
        filter(!is.na(id) & id != "" & label %in% row_keys)

      # Group rows by ID to find connections
      id_groups <- row_id_map %>%
        group_by(id) %>%
        summarise(labels = list(label), .groups = "drop") %>%
        filter(lengths(labels) > 1) # Only IDs with multiple rows

      # Create curved arches for each ID group using shapes
      for (i in seq_len(nrow(id_groups))) {
        id_labels <- id_groups$labels[[i]]

        # Create curved arches between consecutive pairs
        for (j in 1:(length(id_labels) - 1)) {
          label1 <- id_labels[j]
          label2 <- id_labels[j + 1]

          # Get row positions in the factor levels
          y1_pos <- which(levels(y_axis_labels) == label1) - 1 # 0-based for plotly
          y2_pos <- which(levels(y_axis_labels) == label2) - 1 # 0-based for plotly

          if (length(y1_pos) == 1 && length(y2_pos) == 1) {
            # Create SVG path for curved arch
            y_mid <- (y1_pos + y2_pos) / 2
            arch_control_x <- -0.8 # Control point for curve

            # SVG path for quadratic Bezier curve (arch)
            path_string <- sprintf(
              "M -0.2,%d Q %f,%f -0.2,%d",
              y1_pos, arch_control_x, y_mid, y2_pos
            )

            arch_shapes[[length(arch_shapes) + 1]] <- list(
              type = "path",
              path = path_string,
              line = list(
                color = "rgba(0, 0, 0, 0.6)", # Black semi-transparent
                width = 2
              ),
              fillcolor = "rgba(0, 0, 0, 0)", # No fill
              xref = "x",
              yref = "y"
            )
          }
        }
      }
    }

    # Apply layout
    p <- plotly::layout(p, # Explicitly call plotly::layout
      title = list(text = str_c("Cobertura temporal dos extratos em ", format(Sys.time(), "%d/%m/%Y %H:%M")), pad = list(t = 20)),
      xaxis = list(
        title = list(text = "Mês"), # Ensure title is a list for consistency
        type = "category",
        categoryorder = "array",
        categoryarray = levels(x_axis_labels), # Sorted month strings
        showgrid = FALSE,
        tickangle = -45,
        rangeslider = list(visible = FALSE) # Ensure horizontal range slider is removed
      ),
      yaxis = list(
        title = list(text = "Empresa | Banco | Conta", standoff = 15), # Updated order and changed "Tipo de arquivo" to "Banco"
        type = "category",
        categoryorder = "array", # Use the order from categoryarray
        categoryarray = levels(y_axis_labels), # Sorted row keys (A-Z)
        autorange = "reversed",
        showgrid = FALSE,
        rangeslider = list(visible = FALSE) # Ensure vertical range slider is removed
      ),
      shapes = arch_shapes, # Add the curved arch shapes
      legend = list(
        title = list(text = "<b>Status</b>"),
        orientation = "v",
        traceorder = "normal",
        bgcolor = "rgba(250, 250, 250, 0.8)",
        bordercolor = "rgba(100, 100, 100, 0.6)",
        borderwidth = 1
      ),
      showlegend = TRUE
    ) %>%
      config(
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
          "sendDataToCloud", "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "toggleSpikelines", "resetViewMapbox"
        ),
        displaylogo = FALSE, # Remove Plotly logo
        locale = "pt-BR" # Set locale for button hover text
      )

    return(p)
  }

  # --- Main Workflow ---
  prepared_data <- .prepare_heatmap_data(cobertura_t)

  if (is.null(prepared_data) || length(prepared_data$row_keys) == 0 || length(prepared_data$formatted_months) == 0) {
    # Fix: Use layout with a list for annotations
    return(plot_ly() %>% layout(
      title = list(text = "No data available to display."),
      annotations = list(list(text = "No data to display", showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5, font = list(size = 16)))
    ))
  }

  if (interactive()) {
    message("--- Data prepared successfully. Proceeding to create heatmap elements. ---")
    message(paste("Row keys for matrix:", length(prepared_data$row_keys)))
    message(paste("Formatted months for matrix:", length(prepared_data$formatted_months)))
  }

  # Define status translation map based on legend names used in .generate_plotly_figure
  # This map helps keep hover text consistent with legend text.
  status_translation_map <- c(
    "full" = "Completo",
    "incomplete" = "Incompleto",
    "multiple" = "Múltiplo",
    "empty" = "Vazio",
    "other" = "Outro" # Default for 'other' status, not in current legend_definitions
  )

  # Check for rlang::`%||%` and provide a simple alternative if not available/desired
  `%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b


  plot_elements <- .create_heatmap_elements(prepared_data, status_translation_map) # Pass the map

  if (is.null(plot_elements$z)) {
    if (interactive()) message("Failed to create heatmap matrix (z). Returning empty plot.")
    # Fix: Use layout with a list for annotations
    return(plot_ly() %>% layout(
      title = list(text = "Error in matrix generation."),
      annotations = list(list(text = "No data to display", showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5, font = list(size = 16)))
    ))
  }

  if (interactive()) message("--- Heatmap elements created. Generating Plotly figure. ---")

  final_plot <- .generate_plotly_figure(plot_elements, prepared_data$row_keys, prepared_data$formatted_months)

  if (interactive()) message("--- Plotly figure generated. ---")
  return(final_plot)
}

# Example usage (ensure e_cobertura_extratos() is available and returns data):
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
#   # g_cobertura.arquivos(sample_data)
# }

# Add to global variables to avoid R CMD check notes for NSE in dplyr
utils::globalVariables(c(
  ".", "periodo.inicio_parsed", "periodo.fim_parsed", "month_date",
  "month_start", "month_end", "full_month_coverage", "n_paths",
  "n_full", "n_incomplete", "color_code", "label", "original_date", "conta", "banco", "id", "labels",
  # Added for tidy evaluation warnings
  "tipo.xcef", "arquivo.tipo", "empresa", "arquivo", "periodo.inicio", "periodo.fim", "descricao", "month_date", "formatted", "n_paths", "n_full", "n_incomplete", "color_code", "label", "original_date", "subtipos", "cur_data"
))
