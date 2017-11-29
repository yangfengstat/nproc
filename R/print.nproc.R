#' Print the nproc object.
#' @export
#' @method print nproc
#' @param x fitted nproc object using \code{nproc}.
#' @param ... additional arguments.
#' @seealso \code{\link{npc}}, \code{\link{nproc}}
#' @examples
#' n = 1000
#' x = matrix(rnorm(n*2),n,2)
#' c = 1+3*x[,1]
#' y = rbinom(n,1,1/(1+exp(-c)))
#' fit = nproc(x, y, method = 'lda')
#' print(fit)

print.nproc <- function(x, ...) {
  x$v = NULL
  attr(x, "class") = NULL
  print(x)
}
