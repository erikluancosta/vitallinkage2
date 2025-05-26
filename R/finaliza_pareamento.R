#' Montar a função de finalizar pareamento
#'
#' @param df ainda a definir, mas deve ser um data frame ou número que representa o último id de pareamento.
#'
#' @return numero adicionado + 1
#' @export
finaliza_pareamento <- function(df = 1){

  # Remover quem começar com "par_c",
  # Remover "par_2"
  # Remover qualquer outra variável criada durante o processo do linkage
  # id_pessoa = coalece para quem tem par_1 e quem não tiver, colocar um número sequencial baseado no último par_1
  # Mudar "par_1" para "id_pareamento"

  return(df + 1)

}
