#' @title Extrair dados de unidades da Informakon
#'
#' @description
#' Extrai e organiza dados de unidades dos arquivos da Informakon.
#'
#' @param caminho.pasta String do caminho da pasta "Inputs".
#'   Valor padrão: \code{caminhos_pastas("fechamento_in")}.
#'
#' @return
#' Retorna uma tibble com os dados das unidades organizados.
#'
#' @examples
#' \dontrun{
#' unidades <- e_ik_unis()
#' print(unidades)
#' }
#'
#' @export
e_ik_unis <- function() {
  normalizar.cabecalho <- function(texto_c) {
    texto_c %>%
      iconv(from = "", to = "ASCII//TRANSLIT") %>%
      tolower() %>%
      gsub("[^a-z0-9]+", " ", .) %>%
      trimws()
  }

  localizar.coluna <- function(nomes.colunas_c, sinonimos.c) {
    nomes.normalizados_c <- normalizar.cabecalho(nomes.colunas_c)
    sinonimos.normalizados_c <- normalizar.cabecalho(sinonimos.c)
    indice_i <- match(sinonimos.normalizados_c, nomes.normalizados_c)
    indice_i <- indice_i[!is.na(indice_i)]

    if (length(indice_i) == 0) {
      return(NA_character_)
    }

    nomes.colunas_c[indice_i[[1]]]
  }

  caminho.pasta <- caminhos_pastas("fechamento_in")

  if (!dir.exists(caminho.pasta)) {
    stop("A pasta 'Inputs' não foi encontrada: ", caminho.pasta)
  }

  # Busca todos os arquivos unis recursivamente
  caminhos.unis_c <- fs::dir_ls(
    caminho.pasta,
    recurse = TRUE,
    type = "file",
    regexp = "unis-.*\\.xlsx$"
  )

  if (length(caminhos.unis_c) == 0) {
    stop("Nenhum arquivo unis encontrado na pasta Inputs.")
  }

  # Extrai as datas dos nomes dos arquivos (formato: unis-YYYY_MM_DD.xlsx)
  datas.por.arquivo_d <- sapply(caminhos.unis_c, function(caminho_c) {
    basename(caminho_c) %>%
      stringr::str_extract("\\d{4}_\\d{2}_\\d{2}") %>%
      as.Date(format = "%Y_%m_%d")
  })

  # Encontra o índice do arquivo mais recente
  indice.recente_i <- which.max(datas.por.arquivo_d)

  # Seleciona o caminho do arquivo mais recente
  caminho.arquivo.unis_c <- caminhos.unis_c[indice.recente_i]

  # Mensagem informativa
  message("Extraindo arquivo: ", basename(caminho.arquivo.unis_c))

  # Lê o arquivo Excel
  unis_t <- readxl::read_excel(caminho.arquivo.unis_c)

  nomes.colunas_c <- names(unis_t)

  mapa.colunas_l <- list(
    empreendimento = c("Empreendimento", "Nome do empreendimento"),
    setor = c("Setor"),
    edificacao = c("Edificação", "Edificacao", "Torre", "Bloco"),
    numero = c("Número", "Numero", "Unidade", "Apto"),
    p.c = c("P/C", "PC", "P C"),
    especie = c("Espécie", "Especie", "Tipo"),
    pavimento = c("Pavimento", "Andar"),
    situacao = c("Situação", "Situacao", "Status"),
    obs.situacao = c("Obs da Situação", "Obs da Situacao", "Obs Situação"),
    adm.terceiro = c("Adm. Terceiro", "Adm Terceiro"),
    inativo = c("Inativo"),
    tab.preco = c(
      "Tab. Preço", "Tab. Preco", "Preço Tab.", "Preco Tab.",
      "Tab. Preço Atual", "Tab. Preco Atual",
      "Tab. Preço Evento", "Tab. Preco Evento"
    ),
    area = c("Área", "Area"),
    area.lote = c("Área Lote", "Area Lote"),
    fracao.ideal = c("Fração Ideal", "Fracao Ideal"),
    suites = c("Suites", "Suítes"),
    d.simples = c("D.Simples", "D Simples"),
    imobiliaria = c("Imobiliaria", "Imobiliária"),
    corretor = c("Corretor"),
    cliente = c("Cliente"),
    cotista = c("Cotista ?", "Cotista?", "Cotista"),
    data = c("Data"),
    mes.ano = c("Mes/Ano", "Mês/Ano", "Mes Ano"),
    valor.imovel = c("Valor do Imóvel", "Valor do Imovel"),
    comissao = c("Comissão", "Comissao"),
    valor.c.d = c("Valor C.D.", "Valor CD"),
    valor.comissao = c("Valor Comissão", "Valor Comissao"),
    valor.venda = c("Valor de Venda", "Preço de Venda", "Preco de Venda"),
    disponivel.locacao = c(
      "Disponivel para Locação?",
      "Disponível para Locação?",
      "Disponivel para Locacao?"
    ),
    escriturado = c("Escriturado?", "Escriturado"),
    transferido.iptu = c("Transferido IPTU?", "Transferido IPTU"),
    data.escritura = c("Data Escritura", "Data da Escritura"),
    cartorio = c("Cartório", "Cartorio"),
    cartorio.nome = c("Cartório Nome", "Cartorio Nome"),
    matricula = c("Matrícula", "Matricula")
  )

  renomeacoes_c <- c()
  for (nome.canonico_c in names(mapa.colunas_l)) {
    nome.atual_c <- localizar.coluna(
      nomes.colunas_c = nomes.colunas_c,
      sinonimos.c = mapa.colunas_l[[nome.canonico_c]]
    )

    if (!is.na(nome.atual_c) && !identical(nome.atual_c, nome.canonico_c)) {
      renomeacoes_c[nome.canonico_c] <- nome.atual_c
    }
  }

  if (length(renomeacoes_c) > 0) {
    names(unis_t)[match(renomeacoes_c, names(unis_t))] <- names(renomeacoes_c)
  }

  colunas.faltantes_c <- setdiff(names(mapa.colunas_l), names(unis_t))
  if (length(colunas.faltantes_c) > 0) {
    for (coluna_c in colunas.faltantes_c) {
      unis_t[[coluna_c]] <- NA_character_
    }
  }

  # Compatibilidade com layouts novos: se tab.preco nao foi mapeado,
  # tenta preencher a partir das colunas de tabela de preco disponiveis.
  if (all(is.na(unis_t$tab.preco))) {
    colunas.tab.preco_c <- intersect(
      c(
        "Tab. Preço Atual", "Tab. Preco Atual",
        "Tab. Preço Evento", "Tab. Preco Evento",
        "Tab. Preço", "Tab. Preco",
        "Preço Tab.", "Preco Tab."
      ),
      names(unis_t)
    )

    if (length(colunas.tab.preco_c) > 0) {
      unis_t$tab.preco <- suppressWarnings(
        as.numeric(unis_t[[colunas.tab.preco_c[[1]]]])
      )
    }
  }

  # Mantem coluna legada esperada por fluxos antigos.
  if (!"Tab. Preço" %in% names(unis_t)) {
    unis_t[["Tab. Preço"]] <- unis_t$tab.preco
  }

  unis_t <- unis_t %>%
    mutate(
      arquivo = caminho.arquivo.unis_c,
      arquivo.tipo = "unis",
      arquivo.tabela.tipo = "unis",
      arquivo.fonte = "ik"
    ) %>%
    dplyr::filter(!is.na(empreendimento))

  return(unis_t)
}
