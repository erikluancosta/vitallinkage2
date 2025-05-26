#' Cria colunas de ano com base na origem do dado (SIM, SIH ou SINAN)
#'
#' @description
#' Identifica colunas de datas relevantes de acordo com o banco de origem e gera colunas
#' padronizadas: \code{ano} (ano do evento principal) e \code{ano_nasc} (ano de nascimento).
#'
#' @param df Data frame com as colunas de datas.
#' @param banco String com o nome da base de origem, como "SIM", "SIH", "SINAN_VIOL" ou "SINAN_IEXO".
#'
#' @return Data frame com as colunas \code{ano} e/ou \code{ano_nasc} adicionadas.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom lubridate ymd year
#'
#' @examples
#' df <- data.frame(
#'   dt_nasc = as.Date(c("1980-01-01", "1990-02-02")),
#'   dt_obito = as.Date(c("2020-03-01", "2021-04-01")),
#'   dt_internacao = as.Date(c("2019-06-01", "2020-07-01"))
#' )
#' criar_ano(df, "SIM")
#' criar_ano(df, "SINAN_VIOL")
#' criar_ano(df, "SIH")
#'
#' @export
criar_ano <- function(df, banco) {
  if (!is.character(banco)) {
    stop("O argumento 'banco' deve ser uma string.")
  }

  banco <- toupper(banco)

  df <- dplyr::mutate(
    df,
    ano = dplyr::case_when(
      startsWith(banco, "SINAN") & "dt_nasc" %in% names(df) ~ format(df$dt_nasc, "%Y"),
      banco == "SIH" & "dt_internacao" %in% names(df) ~ as.character(lubridate::year(lubridate::ymd(df$dt_internacao))),
      banco == "SIM" & "dt_obito" %in% names(df) ~ format(df$dt_obito, "%Y"),
      TRUE ~ NA_character_
    ),
    ano_nasc = dplyr::case_when(
      "dt_nasc" %in% names(df) ~ format(df$dt_nasc, "%Y"),
      TRUE ~ NA_character_
    )
  )

  return(df)
}
