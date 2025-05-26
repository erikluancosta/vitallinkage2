#' @title Geração de variáveis SoundexBR a partir de nomes completos
#'
#' @description
#' A função `soundex_vars()` automatiza a separação de componentes de um nome completo
#' (primeiro nome, nomes do meio e último sobrenome) e aplica o algoritmo SoundexBR
#' a cada componente. Os códigos Soundex resultantes podem ser usados em tarefas de
#' pareamento aproximado, deduplicação ou linkage de registros.
#'
#' Esta função é útil em contextos de limpeza de dados, integração de bases e análise de dados
#' que contenham variações ortográficas de nomes próprios.
#'
#' @param df Um `data.frame` contendo a coluna de nomes completos a ser processada.
#' @param col_name Nome da coluna de nomes completos, passado como uma `string` (ex: `"nome_completo"`).
#'
#' @details
#' A função realiza os seguintes passos:
#' \enumerate{
#'   \item Converte os caracteres para ASCII (removendo acentos).
#'   \item Extrai o primeiro nome, os nomes intermediários e o último nome.
#'   \item Aplica o algoritmo SoundexBR para cada uma das três partes.
#'   \item Concatena os códigos SoundexBR das partes em uma nova variável do tipo `col_name_sound`.
#' }
#'
#' A estrutura final do `data.frame` inclui:
#' \itemize{
#'   \item col_name1, col_name2, col_name3 — partes do nome.
#'   \item col_name1_sound, col_name2_sound, col_name3_sound — SoundexBR das partes.
#'   \item col_name_sound — concatenação dos três códigos SoundexBR.
#' }
#'
#' Os valores `"NANANA"` gerados pelo SoundexBR são convertidos para `NA`.
#'
#' @return
#' Retorna o `data.frame` original com colunas adicionais:
#' \describe{
#'   \item{`<col_name>1`, `<col_name>2`, `<col_name>3`}{Partes do nome}
#'   \item{`<col_name>1_sound`, `<col_name>2_sound`, `<col_name>3_sound`}{Soundex de cada parte}
#'   \item{`<col_name>_sound`}{Concatenação dos três Soundex}
#' }
#'
#' @importFrom dplyr relocate mutate across
#' @importFrom stringi stri_extract_first_words stri_extract_last_words
#' @importFrom SoundexBR soundexBR
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(nome_completo = c("João da Silva", "Maria de Souza"))
#'   df_processado <- soundex_vars(df, "nome_completo")
#'   head(df_processado)
#' }
#'
#' @export

soundex_vars <- function(df, col_name) {

  df[[col_name]] <- iconv(df[[col_name]], "UTF-8", "ASCII")

  # df[[col_name]] <- stringi::stri_trans_general(df[[col_name]], "latin-ascii")
  # Separar componentes do nome
  col_name1 <- paste0(col_name, "1")
  col_name2 <- paste0(col_name, "2")
  col_name3 <- paste0(col_name, "3")

  df[[col_name1]] <- stringi::stri_extract_first_words(df[[col_name]])
  df[[col_name2]] <- sub(".+? ", "", df[[col_name]])
  df[[col_name2]] <- sub("\\s*\\w*$", "", df[[col_name2]])
  df[[col_name3]] <- stringi::stri_extract_last_words(df[[col_name]])

  df <- df |>
    dplyr::relocate(c(col_name1, col_name2, col_name3), .after = col_name)

  # SOUNDEXBR
  col_name1_sound <- paste0(col_name, "1_sound")
  col_name2_sound <- paste0(col_name, "2_sound")
  col_name3_sound <- paste0(col_name, "3_sound")

  df[[col_name1_sound]] <- SoundexBR::soundexBR(df[[col_name1]], BR = TRUE, useBytes = FALSE)
  df[[col_name2_sound]] <- SoundexBR::soundexBR(df[[col_name2]], BR = TRUE, useBytes = FALSE)
  df[[col_name3_sound]] <- SoundexBR::soundexBR(df[[col_name3]], BR = TRUE, useBytes = FALSE)

  df <- df |>
    dplyr::relocate(c(col_name1_sound, col_name2_sound, col_name3_sound), .after = col_name3) |>
    dplyr::mutate(!!paste0(col_name, "_sound") := paste0(get(col_name1_sound), get(col_name2_sound), get(col_name3_sound)),
                  across(contains("_sound"), ~ ifelse(. == "NANANA", NA, .)))


  return(df)
}

