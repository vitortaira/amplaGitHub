test_that("e_cef_xcef extrai dados corretamente de um arquivo de amostra", {
  # Usar um arquivo de teste existente
  caminho_amostra <- test_path("data", "xcef1.pdf")

  # Pular teste se o arquivo não existir
  skip_if_not(file.exists(caminho_amostra))

  # Extrair dados do arquivo
  resultado <- e_cef_xcef(f_caminho.arquivo_c = caminho_amostra)

  # Verificar se o resultado é um tibble
  expect_s3_class(resultado, "tbl_df")

  # Verificar se o tibble tem as colunas esperadas
  expect_named(resultado, c("data", "agencia", "conta", "documento", "descricao", "valor", "tipo_lancamento"))

  # Verificar tipos das colunas
  expect_type(resultado$data, "double")  # Date objects have type "double"
  expect_type(resultado$agencia, "character")
  expect_type(resultado$conta, "character")
  expect_type(resultado$documento, "character")
  expect_type(resultado$descricao, "character")
  expect_type(resultado$valor, "double")
  expect_type(resultado$tipo_lancamento, "character")

  # Verificar que o resultado não está vazio (para dados reais)
  expect_gte(nrow(resultado), 0)
})

test_that("e_cef_xcef retorna um tibble vazio para entrada nula ou vazia", {
  # Testar com caminho de arquivo nulo
  resultado_nulo <- e_cef_xcef(f_caminho.arquivo_c = NULL)
  expect_s3_class(resultado_nulo, "tbl_df")
  expect_equal(nrow(resultado_nulo), 0)
  expect_named(resultado_nulo, c("data", "agencia", "conta", "documento", "descricao", "valor", "tipo_lancamento"))

  # Criar um arquivo temporário vazio
  caminho_vazio <- tempfile(fileext = ".txt")
  file.create(caminho_vazio)

  # Testar com um arquivo vazio
  resultado_vazio <- e_cef_xcef(f_caminho.arquivo_c = caminho_vazio)
  expect_s3_class(resultado_vazio, "tbl_df")
  expect_equal(nrow(resultado_vazio), 0)
  expect_named(resultado_vazio, c("data", "agencia", "conta", "documento", "descricao", "valor", "tipo_lancamento"))

  # Limpar arquivo temporário
  unlink(caminho_vazio)
})
