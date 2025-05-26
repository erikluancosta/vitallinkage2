#' Converte todas as variaveis para o tipo character
#'
#' @description
#' A funcao \code{as_char()} transforma todas as colunas de um data frame
#' em variaveis do tipo \code{character}, independentemente de seu tipo original.
#' Util para padronizar tipos antes de operacoes de uniao, join ou analise textual.
#'
#' @param df Um data frame contendo variaveis de qualquer tipo.
#'
#' @return
#' Um data frame com todas as colunas convertidas para o tipo \code{character}.
#'
#' @examples
#' df <- data.frame(id = 1:3, valor = c(10.5, 20.7, 30.1))
#' as_char(df)
#'
#' @export
as_char <- function(df) {
  data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
}
