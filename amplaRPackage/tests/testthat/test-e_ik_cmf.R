# Test file for e_ik_cmf

test_that("e_ik_cmf lança erro para arquivo inexistente", {
  expect_error(e_ik_cmf("nao_existe.xlsx"), regexp = "não encontrado")
})

test_that("e_ik_cmf extrai o relatório CMF de exemplo", {
  skip_if_not_installed("readxl")
  arquivo_c <- testthat::test_path("data", "cmf-2026_08.xlsx")
  skip_if_not(file.exists(arquivo_c))

  cmf_t <- e_ik_cmf(arquivo_c)

  # Estrutura
  expect_s3_class(cmf_t, "tbl")
  expect_named(cmf_t, c(
    "agente", "agente.codigo", "n.conta", "n.mov", "registro", "data.razao",
    "conciliacao", "c", "natureza.mov", "origem", "valor", "d.c",
    "saldo.razao", "historico", "arquivo", "arquivo.tipo", "arquivo.fonte"
  ))

  # Tipos
  expect_true(inherits(cmf_t$registro, "Date"))
  expect_true(inherits(cmf_t$data.razao, "Date"))
  expect_true(inherits(cmf_t$conciliacao, "Date"))
  expect_type(cmf_t$valor, "double")
  expect_type(cmf_t$saldo.razao, "double")
  expect_type(cmf_t$n.mov, "character")

  # Conteúdo
  expect_gt(nrow(cmf_t), 0)
  expect_false(any(is.na(cmf_t$n.mov)))
  expect_false(any(is.na(cmf_t$data.razao)))
  expect_false(any(is.na(cmf_t$valor)))
  expect_false(any(is.na(cmf_t$agente)))
  expect_false(any(is.na(cmf_t$n.conta)))

  # Sinal: débitos negativos, créditos positivos
  expect_true(all(cmf_t$valor[cmf_t$d.c == "D"] <= 0))
  expect_true(all(cmf_t$valor[cmf_t$d.c == "C"] >= 0))

  # Metadados
  expect_true(all(cmf_t$arquivo.tipo == "cmf"))
  expect_true(all(cmf_t$arquivo.fonte == "ik"))
})
