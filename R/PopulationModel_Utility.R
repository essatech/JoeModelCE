#' Utility Function 1
#'
#' @param expr A number
#' @param sublist A number
#' @keywords internal
esub <- function(expr, sublist) {
  do.call("substitute", list(expr, sublist))
}
#' Utility Function 2
#'
#' @param e An expression
#' @param env parent frame
#' @keywords internal
proc <- function(e, env = parent.frame()) {
  for(nm in all.vars(e)) {
    if (exists(nm, env) && is.language(g <- get(nm, env))) {
      if (is.expression(g)) g <- g[[1]]
      g <- Recall(g, env)
      L <- list(g)
      names(L) <- nm
      e <- esub(e, L)
    }
  }
  e
}
#' Project Matrix Forward in Time
#'
#' @param mx A argument
#' @param vars A argument
#' @param byrow A argument
#' @keywords internal
pmx_eval <- function(mx, vars, byrow = TRUE) {
  matrix(sapply(mx, eval, vars),
         sqrt(length(mx)),
         sqrt(length(mx)), byrow = byrow)
}
