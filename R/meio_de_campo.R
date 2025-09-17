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
meio_de_campo <- function(dt, max_iter = 3L) {
  setDT(dt)

  for (k in seq_len(max_iter)) {

    snap <- dt$par_1                               # 1.  estado antes da volta

    ## A. par_2 ▸ maior par_1 já conhecido ------------------------------------
    dt[!is.na(par_1) & !is.na(par_2),
       par1_max := max(par_1),
       by = par_2]

    ## B. par_final = coalesce( par1_max , par_1 , par_2 ) --------------------
    dt[, par_final := fcoalesce(par1_max, par_1, par_2)]

    ## C. coloca par_final onde par_1 ainda é NA ------------------------------
    dt[is.na(par_1) & !is.na(par_final), par_1 := par_final]

    ## D. fecha transitividade (SEM perder códigos) --------------------------
    dt[!is.na(par_1),
       par_1 := {
         m <- max(par_final, na.rm = TRUE)
         if (is.infinite(m)) .BY$par_1 else m      # se só NA, mantém o antigo
       },
       by = .(par_1)]

    ## limpa temporários desta volta
    dt[, c("par1_max", "par_final") := NULL]

    ## convergiu?
    if (identical(snap, dt$par_1)) break
  }

  ## Safety-check final: se ainda restou NA em par_1, usa par_2 (se houver)
  dt[is.na(par_1) & !is.na(par_2), par_1 := par_2]

  invisible(dt)
}
