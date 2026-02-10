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
  unis_t <- readxl::read_excel(caminho.arquivo.unis_c) %>%
    rename(
      empreendimento = Empreendimento,
      setor = Setor,
      edificacao = Edificação,
      numero = Número,
      p.c = "P/C",
      especie = Espécie,
      pavimento = Pavimento,
      situacao = Situação,
      obs.situacao = "Obs da Situação",
      adm.terceiro = "Adm. Terceiro",
      inativo = Inativo,
      tab.preco = "Tab. Preço",
      area = Área,
      area.lote = "Área Lote",
      fracao.ideal = "Fração Ideal",
      suites = Suites,
      d.simples = "D.Simples",
      imobiliaria = Imobiliaria,
      corretor = Corretor,
      cliente = Cliente,
      cotista = "Cotista ?",
      data = Data,
      mes.ano = "Mes/Ano",
      valor.imovel = "Valor do Imóvel",
      comissao = Comissão,
      valor.c.d = "Valor C.D.",
      valor.comissao = "Valor Comissão",
      valor.venda = "Valor de Venda",
      disponivel.locacao = "Disponivel para Locação?",
      escriturado = "Escriturado?",
      transferido.iptu = "Transferido IPTU?",
      data.escritura = "Data Escritura",
      cartorio = Cartório,
      cartorio.nome = "Cartório Nome",
      matricula = "Matrícula"
    ) %>%
    mutate(
      arquivo = caminho.arquivo.unis_c,
      arquivo.tipo = "unis",
      arquivo.tabela.tipo = "unis",
      arquivo.fonte = "ik"
    ) %>%
    dplyr::filter(!is.na(empreendimento))

  return(unis_t)
}
