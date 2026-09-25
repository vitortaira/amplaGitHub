# Test file for e_ik_fc

test_that("e_ik_fc lança erro para arquivo inexistente", {
  expect_error(e_ik_fc("nao_existe.xlsx"), regexp = "não encontrado")
})

test_that("e_ik_fc extrai o relatório FC de exemplo", {
  skip_if_not_installed("readxl")
  arquivo_c <- testthat::test_path("data", "fc-2026_08.xlsx")
  skip_if_not(file.exists(arquivo_c))

  fc_t <- e_ik_fc(arquivo_c)

  # Estrutura
  expect_s3_class(fc_t, "tbl")
  expect_named(fc_t, c(
    "empresa", "centro.negocio", "data", "n.mov", "historico", "conta",
    "n.conta", "entrada", "saida", "valor", "arquivo", "arquivo.tipo",
    "arquivo.fonte"
  ))

  # Tipos
  expect_true(inherits(fc_t$data, "Date"))
  expect_type(fc_t$entrada, "double")
  expect_type(fc_t$saida, "double")
  expect_type(fc_t$valor, "double")
  expect_type(fc_t$n.mov, "character")

  # Conteúdo
  expect_gt(nrow(fc_t), 0)
  expect_false(any(is.na(fc_t$data)))
  expect_false(any(is.na(fc_t$empresa)))
  expect_false(any(is.na(fc_t$centro.negocio)))
  expect_false(any(is.na(fc_t$valor)))

  # valor = entrada - saida
  expect_equal(fc_t$valor, round(fc_t$entrada - fc_t$saida, 2))

  # Metadados
  expect_true(all(fc_t$arquivo.tipo == "fc"))
  expect_true(all(fc_t$arquivo.fonte == "ik"))
})
