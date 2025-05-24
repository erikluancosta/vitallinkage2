#' Resolve grupos de pares determinísticos transitivos
#'
#' Esta função recebe um `data.table` com colunas `par_1` e `par_2` representando pares determinísticos,
#' e aplica lógica de fechamento transitivo para identificar e atribuir um identificador final comum a cada grupo conectado.
#'
#' O objetivo é garantir que todos os registros pertencentes ao mesmo grupo de ligação (linkage) compartilhem um identificador unificado,
#' mesmo que estejam indiretamente ligados via transitividade.
#'
#' @param dt Um `data.table` contendo, no mínimo, as colunas `par_1` e `par_2`, com pares de identificadores para linkage determinístico.
#'
#' @return O próprio `data.table` modificado com a coluna `par_1` atualizada para refletir o identificador final consolidado para cada grupo.
#' O retorno é invisível (`invisible(dt)`), mas o objeto original `dt` é modificado por referência.
#'
#' @export
#'
#' @importFrom data.table := .I .N .GRP setDT setorder fcoalesce
meio_de_campo <- function(dt) {
  data.table::setDT(dt)

  ## mapa par_2 -> maior par_1 (par_final_temp) -------------------------------
  map_p2 <- dt[!is.na(par_1) & !is.na(par_2),
               .(par_final_temp = max(par_1)),
               by = par_2]

  ## junta sem reordenar
  dt[map_p2, on = "par_2", par_final_temp := i.par_final_temp]

  ## coalesce e 1.º fechamento -------------------------------------------------
  dt[, par_final := fcoalesce(par_final_temp, par_1, par_2)]

  ## fecha transitividade pelo par_1 ------------------------------------------
  dt[!is.na(par_1),
     par_final_final := max(par_final),           # usa MAX como original
     by = par_1]

  dt[, par_1 := fcoalesce(par_final_final, par_final)]

  ## limpa temporários
  dt[, c("par_final_temp", "par_final", "par_final_final") := NULL]
  invisible(dt)
}
