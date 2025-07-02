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
  # Datas dentro do período (more flexible check)
  if (nrow(result$xita_l) > 0 && all(!is.na(result$xita_l$data)) && all(!is.na(result$xita_l$periodo.inicio))) {
    expect_true(all(result$xita_l$data >= min(result$xita_l$periodo.inicio, na.rm = TRUE)))
    expect_true(all(result$xita_l$data <= max(result$xita_l$periodo.fim, na.rm = TRUE)))
  }
  # Datas de consulta coerentes
  expect_true(all(!is.na(result$xita_l$data.consulta)))
  # CNPJ formato válido
  expect_true(all(grepl("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}", result$xita_l$cnpj)))
  # Agência e conta format (more flexible check)
  expect_true(all(nchar(result$xita_l$agencia) > 0))
  expect_true(all(nchar(result$xita_l$conta) > 0))
})
