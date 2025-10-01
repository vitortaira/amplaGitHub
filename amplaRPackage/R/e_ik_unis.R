#' @title Extrair dados de unidades da Informakon
#'
#' @description
#' Extrai e organiza dados de unidades dos arquivos da Informakon.
#'
#' @param f_caminho.arquivo_c Caminho para o arquivo de unidades (.xlsx).
#'   Por padrão, usa o arquivo mais recente na pasta dados/Informakon.
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
e_ik_unis <- function(f_caminho.arquivo_c = NULL) {
  # Definir caminho padrão se não fornecido
  if (is.null(f_caminho.arquivo_c)) {
    f_caminho.arquivo_c <- "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/dados/Informakon/uni-2025_09_26.xlsx"
  }

  # Verificar se o arquivo existe
  if (!file.exists(f_caminho.arquivo_c)) {
    stop(sprintf("Arquivo não encontrado: %s", f_caminho.arquivo_c))
  }

  # Ler o arquivo Excel
  dados_brutos <- readxl::read_excel(f_caminho.arquivo_c)

  # Processar os dados (estrutura básica - será refinada conforme necessário)
  dados_processados <- dados_brutos %>%
    rename(
      empreendimento = Empreendimento,
      setor = Setor,
      edificacao = Edificação,
      numero = Número,
      p.c = `P/C`,
      especie = Espécie,
      pavimento = Pavimento,
      situacao = Situação,
      obs.situacao = `Obs da Situação`,
      adm.terceiro = `Adm. Terceiro`,
      inativo = Inativo,
      tab.preco = `Tab. Preço`,
      area = Área,
      area.lote = `Área Lote`,
      fracao.ideal = `Fração Ideal`,
      suites = Suites,
      d.simples = `D.Simples`,
      imobiliaria = Imobiliaria,
      corretor = Corretor,
      cliente = Cliente,
      cotista = `Cotista ?`,
      data = Data,
      mes.ano = `Mes/Ano`,
      valor.imovel = `Valor do Imóvel`,
      comissao = Comissão,
      valor.c.d = `Valor C.D.`,
      valor.comissao = `Valor Comissão`,
      valor.venda = `Valor de Venda`,
      disponivel.locacao = `Disponivel para Locação?`,
      escriturado = `Escriturado?`,
      transferido.iptu = `Transferido IPTU?`,
      data.escritura = `Data Escritura`,
      cartorio = Cartório,
      cartorio.nome = `Cartório Nome`,
      matricula = `Matrícula`
    ) %>%
    mutate(
      arquivo = basename(f_caminho.arquivo_c),
      arquivo.tipo = "unis",
      arquivo.fonte = "ik"
    )

  return(dados_processados)
}
