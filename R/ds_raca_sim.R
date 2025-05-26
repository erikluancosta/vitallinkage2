#' Decodifica a variavel de raca/cor no formato do SIM
#'
#' @description
#' A funcao \code{ds_raca_sim()} converte os codigos numericos da variavel \code{cd_raca}
#' em descricoes padronizadas conforme o formato utilizado nos dados do SIM.
#'
#' Os codigos seguem a convencao:
#' \itemize{
#'   \item "1" = Branca
#'   \item "2" = Preta
#'   \item "3" = Amarela
#'   \item "4" = Parda
#'   \item "5" = Indigena
#'   \item Qualquer outro valor = Ignorada
#' }
#'
#' @param df Um data frame contendo uma coluna \code{cd_raca} com os codigos de raca/cor.
#'
#' @return
#' O mesmo data frame com uma nova coluna \code{ds_raca}, contendo os valores decodificados.
#'
#' @examples
#' df <- data.frame(cd_raca = c("1", "2", "5", "9"))
#' ds_raca_sim(df)
#'
#' @importFrom dplyr mutate case_when
#' @export
ds_raca_sim <- function(df) {
  df <- dplyr::mutate(
    df,
    ds_raca = dplyr::case_when(
      cd_raca == "1" ~ "Branca",
      cd_raca == "2" ~ "Preta",
      cd_raca == "3" ~ "Amarela",
      cd_raca == "4" ~ "Parda",
      cd_raca == "5" ~ "Indigena",
      TRUE ~ "Ignorada"
    )
  )
  return(df)
}
