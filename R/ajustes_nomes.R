#' @title Ajuste e Limpeza Padronizada de Nomes em Data Frames
#'
#' @description
#' A funcao \code{ajustes_nomes()} realiza a limpeza e padronizaçao de colunas textuais
#' em um data frame, transformando expressões padronizadas em \code{NA},
#' removendo acentos, sufixos, conectivos e palavras irrelevantes em nomes proprios.
#'
#' Aplica transformacoes apenas nas colunas que são \code{character},
#' e, no caso de colunas com nomes que contenham "_nome", realiza uma limpeza semantica adicional.
#'
#' @param df Um data frame contendo colunas de nomes de pessoas a serem padronizadas.
#'
#' @return O data frame original com colunas textuais ajustadas.
#'
#' @details
#' A funcao executa:
#' \enumerate{
#'   \item Substituicao de termos como "IGNORADO", "DESCONHECIDO", "NAO DECLARADO" por \code{NA}.
#'   \item Padronização de acentos e caixa alta.
#'   \item Limpeza especifica para colunas com "_nome", incluindo:
#'     \itemize{
#'       \item Remocao de palavras como "RN", "NATIMORTO", "FETO".
#'       \item Remocao de sufixos como "FILHO", "JUNIOR", "NETO".
#'       \item Eliminacao de conectivos como "DA", "DE", "DOS", "MC", "D\\u2019".
#'       \item Substituicao de caracteres nao alfabeticos e multiplas letras repetidas.
#'     }
#' }
#'
#' @importFrom dplyr mutate across case_when na_if where matches
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_remove_all str_replace_all str_trim str_squish
#'
#' @examples
#' df <- data.frame(nome_paciente = c("Joao da Silva", "RN MARIA DE SOUZA", "NATIMORTO", "Ignorado"))
#' ajustes_nomes(df)
#'
#' @export
ajustes_nomes <- function(df) {

  # 1. Vetores de apoio ----
  na_vals <- c(
    "IGNORADO", "IGNORADA", "NAO DECLARADO", "DESCONHECIDO",
    "NAO INFORMADO", "NAO INFORMADA", "NA", "ND", "NAO CONSTA",
    "IGN", "NC", "XXXXX", "-----", "NAO IDENTIFICADO", "NAO IDENTIFICADA",
    "NAO REGISTRADO", "NAO REGISTRADA", "NAO DIVULGADO", "N I",
    "SEM INFORMACOES", "NAO SOUBE INFORMAR"
  )

  remover_rn <- "\\b(RN|RECEM NASCIDO|RECEM|NATIMORTO|NATIMORTE|FETO MORTO|FETO|NASCIDO VIVO|NASCIDO)\\b"
  sufixos    <- "\\b(FILHO|FILHA|NETO|NETA|SOBRINHO|SOBRINHA|JUNIOR|JR|SEGUNDO|TERCEIRO)\\b"
  conectivos <- "\\b(D[AOE]?|DOS|DAS|DDA|DAA|DO|DOO|DDO|DOOS|DDOS|DOSS|DE|DDE|DEE|E|MC|DI|DDI|DII|D\\u2019)\\b"

  # 2. Pipeline de limpeza ----
  df_final <- df |>
    dplyr::mutate(
      # Padroniza textos comuns para NA
      dplyr::across(
        dplyr::where(is.character),
        function(x) {
          x_clean <- stringi::stri_trans_general(toupper(x), "Latin-ASCII")
          dplyr::case_when(
            x_clean %in% na_vals ~ NA_character_,
            TRUE ~ x
          )
        }
      )
    ) |>
    dplyr::mutate(
      # Limpa variaveis que contem "_nome"
      dplyr::across(
        dplyr::where(is.character) & dplyr::matches("_nome", ignore.case = TRUE),
        function(x) {
          x |>
            toupper() |>
            stringi::stri_trans_general("Latin-ASCII") |>
            stringr::str_replace_all("\\s+", " ") |>
            stringr::str_trim() |>
            stringr::str_remove_all(remover_rn) |>
            stringr::str_remove_all(sufixos) |>
            stringr::str_replace_all(conectivos, " ") |>
            stringr::str_replace_all("[^A-Z ]", " ") |>
            stringr::str_replace_all("(.)\\1+", "\\1") |>
            stringr::str_squish() |>
            dplyr::na_if("")
        }
      )
    )

  return(df_final)
}
