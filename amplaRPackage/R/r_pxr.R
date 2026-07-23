#' @title Consolida PXR a partir de viabilidade e fechamento
#'
#' @description
#' A funcao **r_pxr** consolida os dados de viabilidade e fechamento
#' em um unico tibble, realizando apenas um \\code{bind_rows()} entre
#' \\code{e_viabs()$flx} e \\code{r_fechamento()$mes}.
#'
#' @param f_id_pasta_gdrive_c Codigo (ID) da pasta no Google Drive com os
#'   arquivos de viabilidade, repassado para \\code{e_viabs()}.
#' @param xlsx Logico. Argumento repassado para \\code{r_fechamento()}.
#'   Padrao: \\code{FALSE}.
#'
#' @return Tibble com a uniao de \\code{e_viabs()$flx} e
#'   \\code{r_fechamento()$mes}.
#'
#' @examples
#' \\dontrun{
#' pxr <- r_pxr("1AbCdEfGhIjKlMnOpQrStUvWxYz")
#' }
#'
#' @importFrom dplyr bind_rows
#' @export
r_pxr <- function(f_id_pasta_gdrive_c, xlsx = FALSE) {
  viabs_l <- e_viabs(f_id_pasta_gdrive_c)
  fechamento_l <- r_fechamento(xlsx = xlsx)

  dplyr::bind_rows(
    viabs_l$flx,
    fechamento_l$mes
  )
}
