test_that("falhas e vazios sao separados sem perder extratos validos", {
  caminhos <- vapply(seq_len(3), function(indice) {
    caminho <- tempfile(fileext = ".pdf")
    fs::file_create(caminho)
    caminho
  }, character(1))
  caminhos <- as.character(fs::path_abs(caminhos))
  on.exit(fs::file_delete(caminhos))
  modelo <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = character(), contratosPj = character(), mapaContas = NULL
  ))
  extrato <- tibble::as_tibble(lapply(modelo, function(coluna) {
    if (inherits(coluna, "Date")) return(as.Date("2026-08-01"))
    if (inherits(coluna, "POSIXct")) return(as.POSIXct("2026-08-01"))
    if (is.double(coluna)) return(100)
    "teste"
  }))
  extrato$descricao <- "CR DESBLOQ"
  extrato$documento <- "123456"
  extrato$arquivo.subtipo <- "xcef2"
  extrato$cpf.cnpj <- "12345678901"
  extrato$nome.razao <- "Mutuario de teste"
  local_mocked_bindings(e_cef_xcef = function(caminho) {
    if (caminho == caminhos[2]) stop("Falha simulada no PDF")
    if (caminho == caminhos[3]) return(tibble::tibble())
    warning("Aviso simulado")
    extrato
  })
  expect_warning(resultado <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = c(caminhos, caminhos[1]), contratosPj = "123456",
    mapaContas = NULL
  )), "Extracao CEF")
  expect_equal(nrow(resultado), 1L)
  expect_identical(names(resultado), names(modelo))
  expect_equal(resultado$natureza, "entrada.pj")
  expect_equal(resultado$cpf.cnpj, "12345678901")
  expect_equal(resultado$nome.razao, "Mutuario de teste")
  diagnostico <- attr(resultado, "diagnostico")
  expect_equal(diagnostico$estado, c("erro", "sucesso", "vazio"))
  expect_equal(names(diagnostico)[1], "arquivo")
  diagnostico <- diagnostico[match(caminhos, diagnostico$caminho), ]
  expect_equal(diagnostico$estado, c("sucesso", "erro", "vazio"))
  expect_match(diagnostico$erro[2], "Falha simulada")
  expect_match(diagnostico$avisos[1], "Aviso simulado")
})

test_that("arquivo inexistente retorna esquema estavel e diagnostico", {
  expect_warning(resultado <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = tempfile(fileext = ".pdf"),
    contratosPj = character(), mapaContas = NULL
  )), "Extracao CEF")
  expect_equal(nrow(resultado), 0L)
  expect_length(names(resultado), 25L)
  expect_match(attr(resultado, "diagnostico")$erro, "nao encontrado")
})

test_that("esquema invalido e registrado por arquivo", {
  caminho <- tempfile(fileext = ".pdf")
  fs::file_create(caminho)
  on.exit(fs::file_delete(caminho))
  local_mocked_bindings(e_cef_xcef = function(...) {
    tibble::tibble(valor = 100)
  })
  expect_warning(resultado <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = caminho, contratosPj = character(), mapaContas = NULL
  )), "Extracao CEF")
  expect_match(attr(resultado, "diagnostico")$erro, "Colunas obrigatorias")
  expect_equal(nrow(resultado), 0L)
})

test_that("contratos PJ sao carregados uma vez e falhas nao sao ocultadas", {
  caminho <- tempfile(fileext = ".pdf")
  fs::file_create(caminho)
  on.exit(fs::file_delete(caminho))
  chamadas <- 0L
  local_mocked_bindings(
    e_cef_nplpjs = function() {
      chamadas <<- chamadas + 1L
      tibble::tibble(contrato.6.ultimo = "123456",
        contrato.6.penultimo = NA_character_)
    },
    e_cef_xcef = function(...) tibble::tibble()
  )
  suppressMessages(e_cef_xcefs0(caminhosArquivos = caminho, mapaContas = NULL))
  expect_equal(chamadas, 1L)
  local_mocked_bindings(e_cef_nplpjs = function() stop("indisponivel"))
  expect_error(e_cef_xcefs0(caminhosArquivos = caminho, mapaContas = NULL),
    "Falha ao carregar contratos PJ: indisponivel")
})

test_that("continuidade remove so a conta antiga no mesmo mes conhecido", {
  caminho <- tempfile(fileext = ".pdf")
  fs::file_create(caminho)
  on.exit(fs::file_delete(caminho))
  modelo <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = character(), contratosPj = character(), mapaContas = NULL
  ))
  extrato <- modelo[rep(NA_integer_, 4), ]
  extrato$empresa <- "AMP"
  extrato$conta.interno <- c("6000", "2270", "6000", "6000")
  extrato$data.movimentacao <- as.Date(c(
    "2026-08-01", "2026-08-02", "2026-07-01", NA_character_
  ))
  extrato$descricao <- "DESB CR CX"
  extrato$documento <- "123456"
  extrato$arquivo.subtipo <- "xcef6"
  mapa <- tibble::tibble(id.antigo = "AMP-CEF_6000",
    id.atual = "AMP-CEF_2270", id.continuo = "AMP-CEF_2270")
  local_mocked_bindings(e_cef_xcef = function(...) extrato)
  resultado <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = caminho, contratosPj = character(), mapaContas = mapa
  ))
  expect_equal(nrow(resultado), 3L)
  expect_equal(resultado$conta.interno, c("2270", "6000", "6000"))
  expect_true(anyNA(resultado$data.movimentacao))
  expect_equal(resultado$contrato.5, rep("12345", 3))
  expect_equal(resultado$natureza, rep("repasse.cef", 3))
})

test_that("pasta explicita restringe a selecao automatica", {
  pasta <- tempfile()
  fs::dir_create(pasta)
  on.exit(fs::dir_delete(pasta))
  selecionado <- as.character(fs::path(pasta, "extrato.pdf"))
  externo <- tempfile(fileext = ".pdf")
  fs::file_create(selecionado)
  local_mocked_bindings(
    c_extratos = function(...) tibble::tibble(caminho = c(selecionado, externo)),
    e_cef_xcef = function(...) tibble::tibble()
  )
  resultado <- suppressMessages(e_cef_xcefs0(
    f_caminho.pasta.extratos_c = pasta, contratosPj = character(),
    mapaContas = NULL
  ))
  expect_equal(nrow(attr(resultado, "diagnostico")), 1L)
  expect_equal(attr(resultado, "diagnostico")$caminho,
    as.character(fs::path_abs(selecionado)))
})

test_that("mensagens de formato pendente nao escondem o erro original", {
  caminho <- tempfile(fileext = ".pdf")
  fs::file_create(caminho)
  on.exit(fs::file_delete(caminho))
  local_mocked_bindings(e_cef_xcef = function(...) {
    message("Formato pendente para o arquivo")
    stop("could not find function pdf_data")
  })
  expect_warning(resultado <- suppressMessages(e_cef_xcefs0(
    caminhosArquivos = caminho, contratosPj = character(), mapaContas = NULL
  )), "Extracao CEF")
  diagnostico <- attr(resultado, "diagnostico")
  expect_equal(diagnostico$estado, "erro")
  expect_match(diagnostico$erro, "pdf_data")
  expect_match(diagnostico$mensagens, "Formato pendente")
})

test_that("console resume causas e preserva mensagens do catalogo", {
  caminhos <- as.character(fs::path_abs(c(
    tempfile(pattern = "falha-", fileext = ".pdf"),
    tempfile(pattern = "vazio-", fileext = ".pdf")
  )))
  fs::file_create(caminhos)
  on.exit(fs::file_delete(caminhos))
  local_mocked_bindings(
    c_extratos = function(...) {
      message("Amostras dos arquivos: ruido do catalogo")
      tibble::tibble(caminho = caminhos)
    },
    e_cef_xcef = function(caminho) {
      if (caminho == caminhos[1]) stop("Falha de leitura simulada")
      tibble::tibble()
    }
  )
  mensagens <- character()
  expect_warning(resultado <- withCallingHandlers(e_cef_xcefs0(
    contratosPj = character(), mapaContas = NULL
  ), message = function(mensagem) {
    mensagens <<- c(mensagens, conditionMessage(mensagem))
    invokeRestart("muffleMessage")
  }), "1 arquivo\\(s\\) com erro")
  console <- paste(mensagens, collapse = "\n")
  expect_false(grepl("ruido do catalogo", console))
  expect_match(console, "Selecionando extratos")
  expect_match(console, "Falhas por causa")
  expect_match(console, "Falha de leitura simulada")
  expect_match(console, fs::path_file(caminhos[1]), fixed = TRUE)
  expect_match(console, "nao comprova ausencia")
  expect_match(attr(resultado, "mensagensSelecao"), "ruido do catalogo")
  expect_equal(attr(resultado, "diagnostico")$arquivo,
    fs::path_file(caminhos))
})

test_that("advertencias do catalogo continuam visiveis", {
  local_mocked_bindings(c_extratos = function(...) {
    warning("Catalogo incompleto")
    tibble::tibble(caminho = character())
  })
  expect_warning(resultado <- suppressMessages(e_cef_xcefs0(
    contratosPj = character(), mapaContas = NULL
  )), "Catalogo incompleto")
  expect_equal(nrow(resultado), 0L)
  expect_named(attr(resultado, "diagnostico"), c(
    "arquivo", "estado", "registros", "erro", "avisos", "caminho",
    "mensagens"
  ))
})

test_that("progresso e exemplos permanecem limitados em lotes grandes", {
  pasta <- tempfile()
  fs::dir_create(pasta)
  on.exit(fs::dir_delete(pasta))
  caminhos <- as.character(fs::path(pasta, paste0(seq_len(24), ".pdf")))
  fs::file_create(caminhos)
  local_mocked_bindings(e_cef_xcef = function(...) tibble::tibble())
  mensagens <- character()
  resultado <- withCallingHandlers(e_cef_xcefs0(
    caminhosArquivos = caminhos, contratosPj = character(), mapaContas = NULL
  ), message = function(mensagem) {
    mensagens <<- c(mensagens, conditionMessage(mensagem))
    invokeRestart("muffleMessage")
  })
  expect_equal(sum(grepl("Extraidos [0-9]+/24", mensagens)), 4L)
  expect_true(any(grepl("24/24 arquivos \\(100%\\)", mensagens)))
  expect_match(paste(mensagens, collapse = "\n"), "; \\+21")
  expect_equal(nrow(attr(resultado, "diagnostico")), 24L)
})