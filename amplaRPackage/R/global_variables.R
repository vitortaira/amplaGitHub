# global_variables.R
# This file defines global variables for the amplaRPackage package.
# Place all global constants and mappings here for easy reuse across the package.

# PT-BR to English month abbreviations
# Use this mapping to convert Portuguese month abbreviations to English.
datas.b_pt.en <- c(
  "jan" = "01",
  "fev" = "02",
  "mar" = "03",
  "abr" = "04",
  "mai" = "05",
  "jun" = "06",
  "jul" = "07",
  "ago" = "08",
  "set" = "09",
  "out" = "10",
  "nov" = "11",
  "dez" = "12"
)

# Contratos de empréstimos PJ
# Função para obter contratos PJ atualizada dinamicamente
get_contratos_pj_6_ultimos <- function() {
  tryCatch(
    {
      unique(e_cef_ecns()$ecn_pj$numero) %>%
        str_sub(-6, -1)
    },
    error = function(e) {
      # Se houver erro, retorna lista vazia
      character(0)
    }
  )
}

# Lista padrão de contratos PJ (pode ser atualizada conforme necessário)
contratos.pj.6.ultimos_c <- character(0)

# Contas bancárias
# Mapear continuidade de contas bancárias usando estrutura compatível
contasBancarias <- tibble::tibble(
  empresa = character(), banco = character(),
  contaAntiga = character(), contaAtual = character()
) %>%
  dplyr::bind_rows(
    # AMP
    tibble::tibble(
      empresa = "AMP", banco = "CEF",
      contaAntiga = "600-0", contaAtual = "577243227-0"
    ),
    tibble::tibble(
      empresa = "AMP", banco = "CEF",
      contaAntiga = "2362-4", contaAtual = "579133302-8"
    ),
    tibble::tibble(
      empresa = "AMP", banco = "CEF",
      contaAntiga = "2429-9", contaAtual = "579133332-0"
    ),
    # AVR
    tibble::tibble(
      empresa = "AVR", banco = "CEF",
      contaAntiga = "659-0", contaAtual = "577243229-6"
    ),
    # AVS
    tibble::tibble(
      empresa = "AVS", banco = "CEF",
      contaAntiga = "2399-3", contaAtual = "579133317-6"
    ),
    tibble::tibble(
      empresa = "AVS", banco = "CEF",
      contaAntiga = "2245-8", contaAtual = "577294854-3"
    ),
    # BUT
    tibble::tibble(
      empresa = "BUT", banco = "CEF",
      contaAntiga = "2498-1", contaAtual = "579133369-9"
    ),
    # CHR
    tibble::tibble(
      empresa = "CHR", banco = "CEF",
      contaAntiga = "3701", contaAtual = "579897189-5"
    ),
    # CPE
    tibble::tibble(
      empresa = "CPE", banco = "CEF",
      contaAntiga = "2505-8", contaAtual = "577565836-8"
    ),
    # DPF
    tibble::tibble(
      empresa = "DPF", banco = "CEF",
      contaAntiga = "29548-2", contaAtual = "586376394-5"
    ),
    # ENC
    tibble::tibble(
      empresa = "ENC", banco = "CEF",
      contaAntiga = "793-7", contaAtual = "577243231-8"
    ),
    tibble::tibble(
      empresa = "ENC", banco = "CEF",
      contaAntiga = "2420-5", contaAtual = "578317306-8"
    ),
    # GRA
    tibble::tibble(
      empresa = "GRA", banco = "CEF",
      contaAntiga = "2480-9", contaAtual = "579133363-0"
    ),
    # INC
    tibble::tibble(
      empresa = "INC", banco = "CEF",
      contaAntiga = "3455-6", contaAtual = "578129123-3"
    ),
    # LCG
    tibble::tibble(
      empresa = "LCG", banco = "CEF",
      contaAntiga = "29757-4", contaAtual = "586376423-2"
    ),
    # POM
    tibble::tibble(
      empresa = "POM", banco = "CEF",
      contaAntiga = "2278-4", contaAtual = "577294862-4"
    ),
    # SN2
    tibble::tibble(
      empresa = "SN2", banco = "CEF",
      contaAntiga = "2419-1", contaAtual = "579133324-9"
    )
  ) %>%
  dplyr::mutate(
    # Criar as colunas esperadas pelo código existente
    id.antigo = paste0(
      empresa,
      "-",
      banco,
      "_",
      stringr::str_sub(stringr::str_remove_all(contaAntiga, "-"), -4, -1)
    ),
    id.atual = paste0(
      empresa,
      "-",
      banco,
      "_",
      stringr::str_sub(stringr::str_remove_all(contaAtual, "-"), -4, -1)
    ),
    id.continuo = paste0(
      empresa,
      "-",
      banco,
      "_",
      stringr::str_sub(stringr::str_remove_all(contaAntiga, "-"), -4, -1),
      stringr::str_sub(stringr::str_remove_all(contaAtual, "-"), -4, -1)
    )
  )
