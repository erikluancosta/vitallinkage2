#' Aplica regras de linkage determinístico por similaridade completa entre variáveis
#'
#' Esta função identifica grupos de registros em um `data.table` que possuem valores idênticos em um conjunto de variáveis (`variaveis`), e atribui um identificador de linkage (`par_1_new`) comum para esses grupos.
#' É utilizada em processos de pareamento determinístico, atribuindo o mesmo código para registros com as mesmas informações em variáveis-chave.
#'
#' A função:
#' - Identifica combinações completas de variáveis escolhidas.
#' - Filtra apenas grupos com pelo menos dois registros idênticos.
#' - Atribui códigos reaproveitando o maior valor já presente na coluna `par_1`, ou criando novos códigos sequenciais.
#' - Preenche colunas `par_cX` e `par_2` com os novos códigos atribuídos.
#' - Aplica fechamento transitivo com a função `meio_de_campo_dt`.
#'
#' @param df Um `data.table` contendo os dados a serem pareados. Deve conter (ou ser possível adicionar) colunas `par_1`, `par_2` e `par_cX`.
#' @param variaveis Vetor de nomes de colunas (caracteres) a serem utilizadas para definir igualdade completa entre registros.
#' @param num_regra Número inteiro indicando o número da regra atual. Usado para nomear dinamicamente a coluna `par_cX` (ex: `par_c1`, `par_c2`, etc).
#'
#' @return O mesmo `data.table` com os pares de linkage atualizados: colunas `par_1`, `par_2`, e a nova `par_cX` adicionada/modificada.
#' A ordenação original dos registros é preservada.
#'
#' @export
#'
#' @importFrom data.table := .I .N .GRP setDT setorder fcoalesce
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   nome = c("Ana", "Ana", "Bruno", "Carlos", "Carlos"),
#'   nascimento = c("2000-01-01", "2000-01-01", "1995-05-05", "1980-10-10", "1980-10-10"),
#'   par_1 = c(NA_integer_, 1, 2, 3, NA_integer_)
#' )
#' regra_pareamento(dt, variaveis = c("nome", "nascimento"), num_regra = 2)
#' print(dt)
regra_pareamento <-  function(df, variables, num_regra) {
  tictoc::tic("Tempo de processamento")
  setDT(df)
  if (!".rowid" %in% names(df)) df[, .rowid := .I]   # preserva ordem
  vars <- variables

  ## 1. linhas COMPLETAS p/ a regra ------------------------------------------
  idx_complete <- df[, which(complete.cases(.SD)), .SDcols = vars]
  if (!length(idx_complete)) return(df[order(.rowid)])

  ## 2. grupos com ≥ 2 ocorrências -------------------------------------------
  grupos <- df[idx_complete, .N, by = vars][N > 1L]
  if (!nrow(grupos)) return(df[order(.rowid)])

  ## 3. marca participantes ---------------------------------------------------
  df[, flag := FALSE]
  df[grupos, on = vars, flag := TRUE]

  ## 4. calcula par_1_new -----------------------------------------------------
  max_code <- ifelse(any(!is.na(df$par_1)), max(df$par_1, na.rm = TRUE), 0L)

  df[flag == TRUE,
     par_1_new := {
       cur <- par_1[!is.na(par_1)]
       if (length(cur)) max(cur) else NA_integer_
     },
     by = vars]

  df[flag == TRUE & is.na(par_1_new),
     par_1_new := max_code + .GRP,
     by = vars]

  ## 5. escreve par_cX e par_2  (AJUSTE AQUI)
  par_c <- paste0("par_c", num_regra)
  if (!par_c %in% names(df)) df[, (par_c) := NA_integer_]

  cols_to_set <- c(par_c, "par_2")
  df[flag == TRUE, (cols_to_set) := .(par_1_new, par_1_new)]

  ## 6. fechamento transitivo completo ---------------------------------------
  meio_de_campo(df, max_iter = 3L)

  ## 7. limpeza --------------------------------------------------------------
  df[, c("flag", "par_1_new") := NULL]
  setorder(df, .rowid)[, .rowid := NULL]

  ## 8. checagem final -------------------------------------------------------
  if (all(is.na(df[[par_c]]))) message("Nenhum registro foi pareado")

  tictoc::toc()
  return(df[])
}
