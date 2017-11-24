#' Compare two NP classification methods at different type I error upper bounds.
#'
#' \code{compare} compares NP classification methods and provides the regions where one method is better than the other.
#' @export
#' @param roc1 the first nproc object.
#' @param roc2 the second nproc object.
#' @param plot whether to generate the two NP-ROC plots and mark the area of significant difference. Default = 'TRUE'.
#' @param col1 the color of the region where roc1 is significantly better than roc2. Default = 'black'.
#' @param col2 the color of the region where roc2 is significantly better than roc1. Default = 'red'.
#' @return A list with the following items.
#' \item{alpha1}{the alpha values where roc1 is significantly better than roc2. }
#' \item{alpha2}{the alpha values where roc2 is significantly better than roc1. }
#' \item{alpha3}{the alpha values where roc1 and roc2 are not significantly different.}
#' @seealso \code{\link{npc}}, \code{\link{nproc}}, \code{\link{predict.npc}} and \code{\link{plot.nproc}}
#' @references
#' Xin Tong, Yang Feng, and Jingyi Jessica Li (2016), Neyman-Pearson (NP) classification algorithms and NP receiver operating characteristic (NP-ROC) curves, manuscript, http://arxiv.org/abs/1608.03109.
#' @examples
#' n = 1000
#' set.seed(1)
#' x1 = c(rnorm(n), rnorm(n) + 1)
#' x2 = c(rnorm(n), rnorm(n)*sqrt(6) + 1)
#' y = c(rep(0,n), rep(1,n))
#' fit1 = nproc(x1, y, method = 'lda')
#' fit2 = nproc(x2, y, method = 'lda')
#' v = compare(fit1, fit2)
#' legend('topleft',legend=c('x1','x2'),col=1:2,lty=c(1,1))
#'
compare <- function(roc1, roc2, plot = TRUE, col1 = "black", col2 = "red") {

    alphalist = roc1$typeI.u
    loc1 = roc1$typeII.u < roc2$typeII.l
    loc2 = roc1$typeII.l > roc2$typeII.u
    alpha1 = alphalist[loc1]
    alpha2 = alphalist[loc2]
    alpha3 = setdiff(alphalist, c(alpha1, alpha2))
    if(roc1$delta != roc2$delta){
      stop("The two nproc objects need to have the same delta.")
    }
    if (plot == TRUE) {
        plot(roc1, col = col1, lwd = 2)
        lines(roc2, col = col2, lwd = 2)
    }

    n.alpha = length(alphalist)
    alphalist.extended = c(0, alphalist, 1)
    for (i in 1:n.alpha) {
        x1 = (alphalist.extended[i] + alphalist.extended[i + 1])/2
        x2 = (alphalist.extended[i + 1] + alphalist.extended[i + 2])/2
        if (plot == TRUE) {
            if ((!is.na(loc1[i])) & loc1[i]) {
                polygon(c(x1, x2, x2, x1), c(0, 0, -0.05, -0.05), border = col1, col = col1)
            } else if ((!is.na(loc2[i])) & loc2[i]) {
                polygon(c(x1, x2, x2, x1), c(0, 0, -0.05, -0.05), border = col2, col = col2)
            }
        }
    }


    return(list(alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3))
}

