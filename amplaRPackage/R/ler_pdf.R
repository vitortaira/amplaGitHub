#' Read and Process Text from a PDF File
#'
#' This function reads all pages from a specified PDF file, processes the text on each page
#' by splitting it into lines, squishing whitespace, and removing empty lines.
#' It returns a list containing the processed lines per page and all lines concatenated.
#'
#' @param arquivo.caminho Character string. The full path to the PDF file to be read.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item{\code{paginas}: A list, where each element is a character vector
#'           representing the processed lines of a single page from the PDF.}
#'     \item{\code{linhas}: A single character vector containing all processed lines
#'           from all pages of the PDF, concatenated together.}
#'   }
#' @export
#' @importFrom pdftools pdf_text
#' @importFrom purrr map discard
#' @importFrom stringr str_split str_squish
#' @examples
#' # Assuming you have a PDF file at "path/to/your/file.pdf"
#' # For a runnable example, let's create a dummy PDF (requires internet & pdftools)
#' # try({
#' #   dummy_pdf_path <- tempfile(fileext = ".pdf")
#' #   # Download a simple, publicly available PDF for the example
#' #   download.file("https://www.w3.org/WAI/ER/tests/xhtml/testfiles/resources/pdf/dummy.pdf",
#' #                 dummy_pdf_path, mode = "wb")
#' #   pdf_data <- ler_pdf(dummy_pdf_path)
#' #   print(paste("Number of pages:", length(pdf_data$paginas)))
#' #   if (length(pdf_data$linhas) > 0) {
#' #     print(paste("First line of the PDF:", pdf_data$linhas[1]))
#' #   }
#' #   unlink(dummy_pdf_path) # Clean up the dummy file
#' # }, silent = TRUE)
ler_pdf <- function(arquivo.caminho) {
  paginas_l <- pdf_text(arquivo.caminho) %>%
    map(function(pagina) {
      linhas <- str_split(pagina, "\n")[[1]] %>%
        str_squish()
      discard(linhas, function(linha) {
        linha == ""
      })
    })
  linhas_c <- unlist(paginas_l, use.names = FALSE)
  return(list(paginas = paginas_l, linhas = linhas_c))
}
