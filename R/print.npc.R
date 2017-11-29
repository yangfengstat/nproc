#' Print the npc object.
#' @export
#' @method print npc
#' @param x fitted npc object using \code{npc}.
#' @param ... additional arguments.
#' @seealso \code{\link{npc}}, \code{\link{nproc}}
#' @examples
#' n = 1000
#' x = matrix(rnorm(n*2),n,2)
#' c = 1+3*x[,1]
#' y = rbinom(n,1,1/(1+exp(-c)))
#' fit = npc(x, y, method = 'lda')
#' print(fit)

print.npc <- function(x, ...) {
  x$fits = NULL
  attr(x, "class") = NULL
  print(x)
}
