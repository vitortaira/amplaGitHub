#' @title Configuração de Localidade ao Carregar o Pacote
#'
#' @description
#' A função `.onLoad` é executada automaticamente ao carregar o pacote e
#' configura a localidade para `pt_BR.UTF-8`, garantindo que caracteres
#' especiais sejam interpretados corretamente.
#'
#' @param libname O caminho para a biblioteca onde o pacote está instalado.
#' @param pkgname O nome do pacote sendo carregado.
#'
#' @keywords internal
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
}
