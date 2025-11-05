#' Inicia o pareamento determinístico por igualdade exata entre variáveis
#'
#' Esta função aplica uma etapa inicial de pareamento determinístico, identificando registros com valores idênticos em um conjunto de variáveis especificadas.
#' Para cada grupo de registros com valores completamente coincidentes nas variáveis selecionadas, é atribuído um identificador de grupo (`par_1`) e uma coluna auxiliar `par_c1`.
#' A função é útil como passo preliminar para processos de linkage determinístico em bases de dados.
#'
#' As etapas incluem:
#' - Remoção de registros com valores ausentes nas variáveis-chave.
#' - Identificação de grupos com múltiplas ocorrências.
#' - Geração de identificadores únicos (`par_1` e `par_c1`) por grupo.
#' - Junção dos resultados ao `data.frame` original.
#'
#' @param df Um `data.frame` ou `tibble` contendo os dados a serem processados.
#' @param variaveis Vetor de caracteres com os nomes das colunas que serão usadas para identificar registros iguais (chaves do pareamento).
#'
#' @return Um `data.frame` contendo as colunas `par_1` e `par_c1` para os registros que foram agrupados com base nas variáveis especificadas.
#' Os registros sem valores completos nas variáveis-chave permanecem com `NA` nessas colunas.
#'
#' @export
#' @importFrom data.table := .I .N .GRP setDT setorder fcoalesce
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(
#'   nome = c("Ana", "Ana", "Bruno", "Carlos", "Carlos", NA),
#'   nascimento = c("2000-01-01", "2000-01-01", "1995-05-05", "1980-10-10", "1980-10-10", "2001-12-12")
#' )
#'
#' resultado <- iniciar_linkage(df, variaveis = c("nome", "nascimento"))
#' print(resultado)
iniciar_linkage <- function(df, variaveis) {
  tictoc::tic("Executado em")
  setDT(df)

  # Adiciona índice auxiliar
  if (!"original_index" %in% names(df)) {
    df[, original_index := .I]
  }

  # Inicializa as colunas par_1 e par_c1 como NA
  if (!"par_1" %in% names(df)) df[, par_1 := NA_integer_]
  if (!"par_c1" %in% names(df)) df[, par_c1 := NA_integer_]

  # Identifica linhas completas nas variáveis-chave
  idx_complete <- df[, complete.cases(.SD), .SDcols = variaveis]

  if (any(idx_complete)) {
    # Conta ocorrências
    df[idx_complete, N_par := .N, by = variaveis]

    # Define grupos apenas para os que têm mais de uma ocorrência
    df[idx_complete & N_par > 1, par_1 := .GRP, by = variaveis]

    # Normaliza sequência dos IDs
    unique_groups <- unique(na.omit(df$par_1))
    if (length(unique_groups)) {
      df[!is.na(par_1), par_1 := match(par_1, unique_groups)]
    }

    # Copia par_1 em par_c1
    df[, par_c1 := par_1]

    # Remove auxiliar
    df[, N_par := NULL]
  }

  tictoc::toc()
  return(df[])
}
