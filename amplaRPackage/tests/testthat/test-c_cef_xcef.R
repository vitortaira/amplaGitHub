test_that("falha em um arquivo CEF nao interrompe a classificacao do lote", {
  caminhoInvalido <- tempfile(fileext = ".pdf")
  caminhoValido <- test_path("data", "xcef3.pdf")

  expect_warning(
    resultado <- c_cef_xcef(c(caminhoInvalido, caminhoValido)),
    "Falha ao classificar extrato CEF"
  )

  expect_identical(resultado, c(NA_character_, "xcef3"))
})