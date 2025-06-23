test_that("e_cef_extcef extrai dados corretamente de um arquivo de amostra", {
  # Criar um arquivo de texto de amostra para o teste
  caminho_amostra <- test_path("data", "extrato_cef_amostra.txt")

  # Esperado: um tibble com uma linha de dados extraída
  resultado <- e_cef_extcef(caminho_do_arquivo = caminho_amostra)

  # Verificar se o resultado é um tibble
  expect_s3_class(resultado, "tbl_df")

  # Verificar se o tibble tem as colunas esperadas
  expect_named(resultado, c("data", "agencia", "conta", "documento", "descricao", "valor", "tipo_lancamento"))

  # Verificar o número de linhas
  expect_equal(nrow(resultado), 1)

  # Verificar os valores da linha extraída
  expect_equal(resultado$data, as.Date("2023-10-26"))
  expect_equal(resultado$agencia, "1234")
  expect_equal(resultado$conta, "56789-0")
  expect_equal(resultado$documento, "000001")
  expect_equal(resultado$descricao, "DEPOSITO EM CHEQUE")
  expect_equal(resultado$valor, 123.45)
  expect_equal(resultado$tipo_lancamento, "credito")
})

test_that("e_cef_extcef retorna um tibble vazio para entrada nula ou vazia", {
  # Testar com caminho de arquivo nulo
  resultado_nulo <- e_cef_extcef(caminho_do_arquivo = NULL)
  expect_s3_class(resultado_nulo, "tbl_df")
  expect_equal(nrow(resultado_nulo), 0)
  expect_named(resultado_nulo, c("data", "agencia", "conta", "documento", "descricao", "valor", "tipo_lancamento"))

  # Criar um arquivo temporário vazio
  caminho_vazio <- tempfile(fileext = ".txt")
  file.create(caminho_vazio)

  # Testar com um arquivo vazio
  resultado_vazio <- e_cef_extcef(caminho_do_arquivo = caminho_vazio)
  expect_s3_class(resultado_vazio, "tbl_df")
  expect_equal(nrow(resultado_vazio), 0)
  expect_named(resultado_vazio, c("data", "agencia", "conta", "documento", "descricao", "valor", "tipo_lancamento"))

  # Limpar arquivo temporário
  unlink(caminho_vazio)
})
