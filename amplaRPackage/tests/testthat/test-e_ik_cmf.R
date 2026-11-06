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
    "empresa", "agente", "agente.codigo", "n.conta", "n.mov", "registro", "data.razao",
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

test_that("CMF plano preserva Emp-Filial e ambas as empresas no cruzamento", {
  caminho <- tempfile(fileext = ".xlsx")
  on.exit(fs::file_delete(caminho))
  dados <- tibble::tibble(
    `N Mov` = c("1", "2"), Data = c(46083, 46083),
    AgenteFinanceiro = c("CEF", "CEF"), `Conta N` = c("123-4", "123-4"),
    Valor = c(100, 25), `D/C` = c("C", "D"),
    `Emp-Filial` = c("GRA.01", "VRG.01")
  )
  openxlsx::write.xlsx(dados, caminho)
  cmf <- e_ik_cmf(caminho)
  expect_equal(cmf$empresa, c("GRA.01", "VRG.01"))
  expect_equal(cmf$valor, c(100, -25))
  fc <- tibble::tibble(n.mov = c("1", "3"), empresa = c("GRA", "AVS"))
  cruzado <- dplyr::full_join(cmf, fc, by = "n.mov",
    suffix = c(".cmf", ".fc"))
  expect_true(all(c("empresa.cmf", "empresa.fc") %in% names(cruzado)))
  expect_equal(cruzado$empresa.cmf, c("GRA.01", "VRG.01", NA_character_))
  expect_equal(cruzado$empresa.fc, c("GRA", NA_character_, "AVS"))
  dados[["Emp-Filial"]] <- NULL
  openxlsx::write.xlsx(dados, caminho, overwrite = TRUE)
  expect_equal(e_ik_cmf(caminho)$empresa, rep(NA_character_, 2))
})

test_that("CMF em blocos preserva agrupamentos de empresa", {
  caminho <- tempfile(fileext = ".xlsx")
  on.exit(fs::file_delete(caminho))
  dados <- as.data.frame(matrix(NA_character_, nrow = 6, ncol = 12))
  dados[1, 1] <- "Empresa: GRA.01"
  dados[2, 1] <- "Agente Conta: CEF - 1 - 123-4"
  dados[3, c(2, 3, 4, 9, 10)] <- c("1", "46083", "46083", "100", "C")
  dados[4, 1] <- "Empresa: VRG.01"
  dados[5, 1] <- "Agente Conta: CEF - 2 - 456-7"
  dados[6, c(2, 3, 4, 9, 10, 12)] <-
    c("2", "46083", "46083", "25", "D", "Pagamento")
  openxlsx::write.xlsx(dados, caminho, colNames = FALSE)
  cmf <- e_ik_cmf(caminho)
  expect_equal(cmf$empresa, c("GRA.01", "VRG.01"))
  expect_equal(cmf$n.mov, c("1", "2"))
  expect_equal(cmf$valor, c(100, -25))
})
