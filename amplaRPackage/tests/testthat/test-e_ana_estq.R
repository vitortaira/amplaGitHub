test_that("estoque identifica CBL com variacoes no nome", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("openxlsx2")

  pasta <- withr::local_tempdir()
  colunasNumericas <- c(
    "Área total", "Área privativa", "Data da venda / reserva",
    "Data de recebimento", "Data do pedido", "Fração ideal",
    "Preço Tab.", "Preço Tab. com Desconto", "Quartos",
    "Valor m2", "Valor m2 com Desconto"
  )
  estoque <- tibble::tibble(
    Empreendimento = c(
      "Up Stay Congonhas", "UP STAY   CONGONHAS",
      "up staycongonhas", "Projeto desconhecido", "Total"
    ),
    Unidade = c("U101", "U102", "U103", "U104", NA_character_),
    `Preço de venda` = "100000",
    Status = "Disponível"
  ) %>%
    dplyr::bind_cols(tibble::as_tibble(stats::setNames(
      rep(list(rep("1", 5)), length(colunasNumericas)),
      colunasNumericas
    )))
  openxlsx2::write_xlsx(
    estoque, fs::path(pasta, "estq-2026_09_30.xlsx")
  )

  resultado <- e_ana_estq(pasta)

  expect_equal(resultado$empresa, c("CBL", "CBL", "CBL", NA_character_))
  expect_equal(
    resultado$id,
    c("CBL-Apartamento-101", "CBL-Apartamento-102",
      "CBL-Apartamento-103", NA_character_)
  )
})

test_that("fechamento cruza CBL identificado no estoque", {
  ambiente <- new.env(parent = environment(r_fechamento))
  ambiente$inputs_l <- list(estq = tibble::tibble(
    id = "CBL-Apartamento-101", empresa = "CBL",
    especie = "Apartamento", unidade = 101L,
    Empreendimento = "Up Stay Congonhas", Status = "Disponível"
  ))
  ambiente$.cache_unis <- tibble::tibble(
    empreendimento = "CBL.01.0001 - Up Stay Congonhas",
    numero = "101", especie = "Apartamento",
    data = as.Date(NA), situacao = "Disponível"
  )
  expressoes <- Filter(function(expressao) {
    is.call(expressao) &&
      identical(expressao[[1]], as.name("<-")) &&
      as.character(expressao[[2]]) %in%
        c("in.estq", "in.unis", "in.unis.cruzado")
  }, as.list(body(r_fechamento)))
  expect_length(expressoes, 3)
  invisible(lapply(expressoes, eval, envir = ambiente))
  resultado <- ambiente$in.unis.cruzado

  expect_equal(nrow(resultado), 1L)
  expect_equal(resultado$id, "CBL-Apartamento-101")
  expect_equal(resultado$empresa.ik, "CBL")
  expect_equal(resultado$empresa.ana, "CBL")
  expect_equal(resultado$status.ana, "Disponível")
  expect_identical(ambiente$in.estq, ambiente$inputs_l$estq)
})