#' @title Ajuste em colunas com formato de data
#'
#' @description
#' Converte automaticamente colunas que começam com "dt_" ou "DT_" em objetos do tipo \code{Date},
#' usando o formato especificado via parametro.
#'
#' @param df Nome do data frame.
#' @param tipo_data Tipo do formato de data: \code{1} para datas no formato "DD/MM/YYYY"
#'                  ou \code{2} para datas no formato "YYYY-MM-DD".
#'
#' @return
#' O data frame modificado, onde as colunas que comecam com "dt_" ou "DT_" foram convertidas
#' para o formato de data especificado pelo parametro \code{tipo_data}.
#'
#' @importFrom lubridate dmy ymd
#'
#' @examples
#' df <- data.frame(dt_nasc = c("01/01/2020", "15/12/2021"))
#' ajuste_data(df, tipo_data = 1)
#'
#' df2 <- data.frame(DT_OBITO = c("2022-05-10", "2023-01-01"))
#' ajuste_data(df2, tipo_data = 2)
#'
#' @export
ajuste_data <- function(df, tipo_data = 1) {

  colunas_dt <- grep("^dt_|^DT_|^DT", names(df), value = TRUE)

  for (coluna in colunas_dt) {
    if (tipo_data == 1) {
      df[[coluna]] <- lubridate::dmy(df[[coluna]])
    } else if (tipo_data == 2) {
      df[[coluna]] <- lubridate::ymd(df[[coluna]])
    } else {
      stop("Tipo de data invalido. Use 1 para dmy ou 2 para ymd.")
      # ou para total compatibilidade:
      # stop("Tipo de data inv\u00e1lido. Use 1 para dmy ou 2 para ymd.")
    }
  }

  return(df)
}
