# Test file for e_ita_extita

test_that("e_ita_extita returns expected structure", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("stringr")
  skip_if_not_installed("purrr")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("tibble")

  # Use a mock or example PDF file path
  example_pdf <- tempfile(fileext = ".pdf")
  # Here you would generate or copy a minimal PDF for testing
  # For now, just check that the function returns a list with correct names
  expect_error(e_ita_extita("nonexistent.pdf"),
    regexp = "file.exists", fixed = TRUE
  )
})

test_that("e_ita_extita parses extita_example PDF robustly", {
  example_file <- testthat::test_path("data", "extita.pdf")
  expect_true(file.exists(example_file))
  result <- e_ita_extita(example_file)
  # Structure
  expect_type(result, "list")
  expect_named(result, c("extita_l", "extita_c"))
  expect_s3_class(result$extita_l, "tbl")
  expect_s3_class(result$extita_c, "tbl")
  # Columns
  expect_true(all(c("data", "valor", "descricao", "empresa", "cnpj", "agencia", "conta", "periodo.inicio", "periodo.fim", "data.consulta", "arquivo") %in% colnames(result$extita_l)))
  expect_true(all(c("descricao", "valor") %in% colnames(result$extita_c)))
  # Types
  expect_true(inherits(result$extita_l$data, "Date"))
  expect_type(result$extita_l$valor, "double")
  expect_type(result$extita_l$descricao, "character")
  # Non-empty
  expect_gt(nrow(result$extita_l), 0)
  expect_gt(nrow(result$extita_c), 0)
  # No NA in required columns
  expect_false(any(is.na(result$extita_l$data)))
  expect_false(any(is.na(result$extita_l$valor)))
  expect_false(any(is.na(result$extita_l$descricao)))
  expect_false(any(is.na(result$extita_l$empresa)))
  expect_false(any(is.na(result$extita_l$cnpj)))
  expect_false(any(is.na(result$extita_l$agencia)))
  expect_false(any(is.na(result$extita_l$conta)))
  expect_false(any(is.na(result$extita_l$periodo.inicio)))
  expect_false(any(is.na(result$extita_l$periodo.fim)))
  expect_false(any(is.na(result$extita_l$data.consulta)))
  expect_false(any(is.na(result$extita_l$arquivo)))
  # File path is correct
  expect_true(all(result$extita_l$arquivo == example_file))
  # Saldos must include saldo.disponivel.conta
  expect_true("saldo.disponivel.conta" %in% result$extita_c$descricao)
  # Limites presentes
  expect_true(all(c("limite.contratado", "limite.utilizado", "limite.disponivel") %in% result$extita_c$descricao))
  # All saldos are numeric
  expect_type(result$extita_c$valor, "double")
  # No duplicated rows in saldos
  expect_equal(nrow(result$extita_c), length(unique(result$extita_c$descricao)))
  # Descrição não deve começar com data
  expect_false(any(grepl("^\\d{1,2}\\s*/\\s*[a-zA-Z]{3}", result$extita_l$descricao)))
  # Datas dentro do período
  expect_true(all(result$extita_l$data >= min(result$extita_l$periodo.inicio)))
  expect_true(all(result$extita_l$data <= max(result$extita_l$periodo.fim)))
  # Datas de consulta coerentes
  expect_true(all(!is.na(result$extita_l$data.consulta)))
  # CNPJ formato válido
  expect_true(all(grepl("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}", result$extita_l$cnpj)))
  # Agência e conta numéricas
  expect_true(all(grepl("^\\d+$", result$extita_l$agencia)))
  expect_true(all(grepl("^\\d+$", result$extita_l$conta)))
})
