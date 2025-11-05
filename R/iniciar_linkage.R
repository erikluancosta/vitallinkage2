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
  # Início da contagem do tempo
  tictoc::tic("Tempo de execução")

  # Adicionando uma coluna de índice para manter o rastreamento das linhas originais
  df <- dplyr::mutate(df, original_index = dplyr::row_number())

  # Filtrando para registros que não possuem valores NA nas variáveis especificadas
  df_filtered <- dplyr::filter(df,
                               stats::complete.cases(
                                 dplyr::select(df, dplyr::all_of(variables))
                               )
  )

  # Transformando em data.table para melhor performance
  df_filtered <- data.table::as.data.table(df_filtered)

  # Criando uma nova coluna 'N_par' onde é contado o número de registros iguais baseado nas variáveis
  df_filtered[, N_par := .N, by = variables]

  # Filtrando os grupos com mais de uma ocorrência
  df_filtered <- df_filtered[N_par > 1]

  # Calculando 'par_1' que é um ID do grupo único para as combinações de variáveis
  df_filtered[, par_1 := .GRP, by = variables]

  # Ajustando par_1 para começar de 1 em diante
  unique_groups <- unique(df_filtered$par_1)
  df_filtered[, par_1 := match(par_1, unique_groups)]

  # Criando 'par_c1' que é simplesmente uma cópia de 'par_1' para manter consistência com a função R
  df_filtered[, par_c1 := par_1]

  # Convertendo de volta para data.frame e removendo duplicatas
  df_filtered <- dplyr::distinct(as.data.frame(df_filtered))

  # Mantendo apenas as colunas necessárias para o join, incluindo o índice original
  df_filtered <- dplyr::select(df_filtered, original_index, par_1, par_c1)

  # Reintegrando os registros filtrados ao DataFrame original usando o índice
  df <- dplyr::left_join(df, df_filtered, by = "original_index")

  # Removendo a coluna de índice original
  df <- dplyr::select(df, -original_index, par_1, dplyr::everything())

  # Fim da contagem do tempo
  tictoc::toc()

  return(df)
}
