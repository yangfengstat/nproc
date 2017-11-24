#' Add NP-ROC curves to the current plot object.
#' @export
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @param x fitted NP-ROC object using \code{nproc}.
#' @param ... additional arguments.
#' @seealso \code{\link{npc}}, \code{\link{nproc}} and \code{\link{plot.nproc}}.
#' @examples
#' n = 1000
#' x = matrix(rnorm(n*2),n,2)
#' c = 1+3*x[,1]
#' y = rbinom(n,1,1/(1+exp(-c)))
#' fit = nproc(x, y, method = 'nb')
#' plot(fit)
#' fit2 = nproc(x, y, method = 'lda')
#' lines(fit2, col = 2)
lines.nproc <- function(x, ...) {
    lines(x$typeI.u, 1 - x$typeII.u, type = "s", ...)
    lines(x$typeI.u, 1 - x$typeII.l, type = "s", ...)
}

