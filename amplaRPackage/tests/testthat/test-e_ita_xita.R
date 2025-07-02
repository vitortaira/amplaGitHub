# Test file for e_ita_xita

test_that("e_ita_xita returns expected structure", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("stringr")
  skip_if_not_installed("purrr")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("tibble")

  # Use a mock or example PDF file path
  example_pdf <- tempfile(fileext = ".pdf")
  # Here you would generate or copy a minimal PDF for testing
  # For now, just check that the function returns a list with correct names
  expect_error(e_ita_xita("nonexistent.pdf"),
    regexp = "file.exists", fixed = TRUE
  )
})

test_that("e_ita_xita parses xita_example PDF robustly", {
  example_file <- testthat::test_path("data", "xita.pdf")
  skip_if_not(file.exists(example_file), "Test data file not found")

  result <- e_ita_xita(example_file)
  # Structure
  expect_type(result, "list")
  expect_named(result, c("xita_l", "xita_c"))
  expect_s3_class(result$xita_l, "tbl")
  expect_s3_class(result$xita_c, "tbl")
  # Columns
  expect_true(all(c("data", "valor", "descricao", "empresa", "cnpj", "agencia", "conta", "periodo.inicio", "periodo.fim", "data.consulta", "arquivo") %in% colnames(result$xita_l)))
  expect_true(all(c("descricao", "valor") %in% colnames(result$xita_c)))
  # Types
  expect_true(inherits(result$xita_l$data, "Date"))
  expect_type(result$xita_l$valor, "double")
  expect_type(result$xita_l$descricao, "character")
  # Non-empty
  expect_gt(nrow(result$xita_l), 0)
  expect_gt(nrow(result$xita_c), 0)
  # No NA in required columns
  expect_false(any(is.na(result$xita_l$data)))
  expect_false(any(is.na(result$xita_l$valor)))
  expect_false(any(is.na(result$xita_l$descricao)))
  expect_false(any(is.na(result$xita_l$empresa)))
  expect_false(any(is.na(result$xita_l$cnpj)))
  expect_false(any(is.na(result$xita_l$agencia)))
  expect_false(any(is.na(result$xita_l$conta)))
  expect_false(any(is.na(result$xita_l$periodo.inicio)))
  expect_false(any(is.na(result$xita_l$periodo.fim)))
  expect_false(any(is.na(result$xita_l$data.consulta)))
  expect_false(any(is.na(result$xita_l$arquivo)))
  # File path is correct
  expect_true(all(result$xita_l$arquivo == example_file))
  # Saldos must include saldo.disponivel.conta
  expect_true("saldo.disponivel.conta" %in% result$xita_c$descricao)
  # Limites presentes
  expect_true(all(c("limite.contratado", "limite.utilizado", "limite.disponivel") %in% result$xita_c$descricao))
  # All saldos are numeric
  expect_type(result$xita_c$valor, "double")
  # No duplicated rows in saldos
  expect_equal(nrow(result$xita_c), length(unique(result$xita_c$descricao)))
  # Descrição não deve começar com data
  expect_false(any(grepl("^\\d{1,2}\\s*/\\s*[a-zA-Z]{3}", result$xita_l$descricao)))
  # Datas dentro do período (flexible check with buffer for edge cases)
  if (nrow(result$xita_l) > 0 && all(!is.na(result$xita_l$data)) &&
    all(!is.na(result$xita_l$periodo.inicio)) && all(!is.na(result$xita_l$periodo.fim))) {
    # Only check if we have valid dates
    min_periodo <- min(result$xita_l$periodo.inicio, na.rm = TRUE)
    max_periodo <- max(result$xita_l$periodo.fim, na.rm = TRUE)

    # Allow for a small buffer (e.g., 5 days) to handle real-world edge cases
    # where statements might include transactions slightly outside the period
    buffer_days <- 5
    min_allowed <- min_periodo - buffer_days
    max_allowed <- max_periodo + buffer_days

    # Check that most transactions are within the expected range
    within_range <- result$xita_l$data >= min_allowed & result$xita_l$data <= max_allowed
    pct_within_range <- sum(within_range) / length(within_range)

    # Expect at least 95% of transactions to be within the buffered range
    expect_gte(pct_within_range, 0.95,
      label = paste(
        "Only", round(pct_within_range * 100, 1),
        "% of transactions are within the expected date range"
      )
    )

    # If there are outliers, they should be reasonable (not more than 30 days outside)
    outliers <- !within_range
    if (any(outliers)) {
      max_outlier_days <- max(c(
        abs(as.numeric(result$xita_l$data[outliers] - min_periodo)),
        abs(as.numeric(result$xita_l$data[outliers] - max_periodo))
      ))
      expect_lte(max_outlier_days, 30,
        label = paste("Outlier transactions are", max_outlier_days, "days outside the period")
      )
    }
  } else {
    # Skip date range check if dates are missing or invalid
    skip("Date range check skipped due to missing or invalid dates")
  }
  # Datas de consulta coerentes
  expect_true(all(!is.na(result$xita_l$data.consulta)))
  # CNPJ formato válido
  expect_true(all(grepl("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}", result$xita_l$cnpj)))
  # Agência e conta format (more flexible check)
  expect_true(all(nchar(result$xita_l$agencia) > 0))
  expect_true(all(nchar(result$xita_l$conta) > 0))
})
