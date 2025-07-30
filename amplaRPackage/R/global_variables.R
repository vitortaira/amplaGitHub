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
# Carrega arquivo Excel com mapeamento das contas bancárias
contasBancarias <- tryCatch(
  readxl::read_excel(
    path = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Dados/Mapeamento dos dados.xlsx",
    sheet = "Contas"
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Banco = stringr::str_replace_all(Banco, "ú", "u"),
      id.antigo = paste0(Empresa,
        "-",
        Banco,
        "_",
        stringr::str_sub(stringr::str_remove_all(`CC (antigo)`, "-"), -4, -1)
      ),
      id.atual = paste0(Empresa,
        "-",
        Banco,
        "_",
        stringr::str_sub(stringr::str_remove_all(`CC (atual)`, "-"), -4, -1)
      )
    ) %>%
    dplyr::mutate(
      id.antigo = dplyr::na_if(id.antigo, paste0(Empresa, "-", Banco, "_")),
      id.atual = dplyr::na_if(id.atual, paste0(Empresa, "-", Banco, "_")),
      id.continuo = dplyr::if_else(
        is.na(id.antigo),
        id.atual,
        stringr::str_c(id.antigo, str_sub(id.atual, -4, -1))
      )
    ),
  error = function(e) {
    warning("Não foi possível carregar o mapeamento de contas bancárias: ",
            e$message)
    tibble::tibble()
  }
)
