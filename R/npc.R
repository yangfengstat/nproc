#' Construct a Neyman-Pearson Classifier from a sample of class 0 and class 1.
#'
#' Given a type I error upper bound alpha and a violation upper bound delta, \code{npc} calculates the Neyman-Pearson Classifier
#' which controls the type I error under alpha with probability at least 1-delta.
#' @export
#' @importFrom e1071 svm
#' @importFrom e1071 naiveBayes
#' @importFrom glmnet cv.glmnet
#' @importFrom MASS lda
#' @importFrom randomForest randomForest
#' @importFrom ada ada
#' @importFrom parallel mcmapply
#' @importFrom parallel mclapply
#' @importFrom graphics plot
#' @importFrom stats approx
#' @importFrom stats glm
#' @importFrom stats predict
#' @importFrom stats pbinom
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats rbinom
#' @importFrom tree tree
#' @importFrom graphics polygon
#' @param x n * p observation matrix. n observations, p covariates.
#' @param y n 0/1 observatons.
#' @param method base classification method.
#' \itemize{
#' \item logistic: Logistic regression. \link{glm} function with family = 'binomial'
#' \item penlog: Penalized logistic regression with LASSO penalty. \code{\link[glmnet]{glmnet}} in \code{glmnet} package
#' \item svm: Support Vector Machines. \code{\link[e1071]{svm}} in \code{e1071} package
#' \item randomforest: Random Forest. \code{\link[randomForest]{randomForest}} in \code{randomForest} package
#' \item lda: Linear Discriminant Analysis. \code{\link[MASS]{lda}} in \code{MASS} package
#' \item slda: Sparse Linear Discriminant Analysis with LASSO penalty.
#' \item nb: Naive Bayes. \code{\link[e1071]{naiveBayes}} in \code{e1071} package
#' \item ada: Ada-Boost. \code{\link[ada]{ada}} in \code{ada} package
#' }
#' @param alpha the desirable upper bound on type I error. Default = 0.05.
#' @param delta the violation rate of the type I error. Default = 0.05.
#' @param split the number of splits for the class 0 sample. Default = 1. For ensemble
#' version, choose split > 1.
#' @param split.ratio the ratio of splits used for the class 0 sample to train the
#' base classifier. The rest are used to estimate the threshold. Can also be set to be "adaptive", which will be determined using a data-driven method implemented in \code{find.optim.split}. Default = 0.5.
#' @param n.cores number of cores used for parallel computing. Default = 1. WARNING:
#' windows machine is not supported.
#' @param band whether to generate both lower and upper bounds of type II error. Default #' = FALSE.
#' @param randSeed the random seed used in the algorithm.
#' @param warning whether to show various warnings in the program. Default = TRUE.
#' @param ... additional arguments.
#' @return An object with S3 class npc.
#'  \item{fits}{a list of length max(1,split), represents the fit during each split.}
#'  \item{method}{the base classification method.}
#'   \item{split}{the number of splits used.}
#' @seealso \code{\link{nproc}} and \code{\link{predict.npc}}
#' @references
#' Xin Tong, Yang Feng, and Jingyi Jessica Li (2016), Neyman-Pearson (NP) classification algorithms and NP receiver operating characteristic (NP-ROC), manuscript, http://arxiv.org/abs/1608.03109.
#' @examples
#' set.seed(1)
#' n = 1000
#' x = matrix(rnorm(n*2),n,2)
#' c = 1+3*x[,1]
#' y = rbinom(n,1,1/(1+exp(-c)))
#' xtest = matrix(rnorm(n*2),n,2)
#' ctest = 1+3*xtest[,1]
#' ytest = rbinom(n,1,1/(1+exp(-ctest)))
#'
#' ##Use lda classifier and the default type I error control with alpha=0.05, delta=0.05
#' fit = npc(x, y, method = 'lda')
#' pred = predict(fit,xtest)
#' fit.score = predict(fit,x)
#' accuracy = mean(pred$pred.label==ytest)
#' cat('Overall Accuracy: ',  accuracy,'\n')
#' ind0 = which(ytest==0)
#' typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
#' cat('Type I error: ', typeI, '\n')
#'
#' \dontrun{
#' ##Ensembled lda classifier with split = 11,  alpha=0.05, delta=0.05
#' fit = npc(x, y, method = 'lda', split = 11)
#' pred = predict(fit,xtest)
#' accuracy = mean(pred$pred.label==ytest)
#' cat('Overall Accuracy: ',  accuracy,'\n')
#' ind0 = which(ytest==0)
#' typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
#' cat('Type I error: ', typeI, '\n')
#'
#' ##Now, change the method to logistic regression and change alpha to 0.1
#' fit = npc(x, y, method = 'logistic', alpha = 0.1)
#' pred = predict(fit,xtest)
#' accuracy = mean(pred$pred.label==ytest)
#' cat('Overall Accuracy: ',  accuracy,'\n')
#' ind0 = which(ytest==0)
#' typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
#' cat('Type I error: ', typeI, '\n')
#'
#' ##Now, change the method to adaboost
#' fit = npc(x, y, method = 'ada', alpha = 0.1)
#' pred = predict(fit,xtest)
#' accuracy = mean(pred$pred.label==ytest)
#' cat('Overall Accuracy: ',  accuracy,'\n')
#' ind0 = which(ytest==0)
#' typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
#' cat('Type I error: ', typeI, '\n')
#'
#' ##Now, try the adaptive splitting ratio
#' fit = npc(x, y, method = 'ada', alpha = 0.1, split.ratio = 'adaptive')
#' pred = predict(fit,xtest)
#' accuracy = mean(pred$pred.label==ytest)
#' cat('Overall Accuracy: ',  accuracy,'\n')
#' ind0 = which(ytest==0)
#' typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
#' cat('Type I error: ', typeI, '\n')
#' cat('Splitting ratio:', fit$split.ratio)
#' }

npc <- function(x = NULL, y, method = c("logistic", "penlog", "svm", "randomforest", "lda", "slda", "nb", "ada", "tree"), alpha = 0.05, delta = 0.05, split = 1, split.ratio = 0.5, n.cores = 1, band  = FALSE, randSeed = 0, warning = TRUE, ...) {
    if (!is.null(x)) {
        x = as.matrix(x)
    }
    method = match.arg(method)
    if(split.ratio == 'adaptive'){
      obj = find.optim.split(x = x, y = y, method = method, alpha = alpha, delta = delta, split = split, split.ratio.seq = seq(from=0.1,to=0.9,by=0.1), band  = band, randSeed = randSeed, warning = FALSE, ...)
      split.ratio = obj$split.ratio.min
    }

    set.seed(randSeed)
      object = NULL
        p = ncol(x)
        if (p == 1 & method == "penlog") {
            stop("glmnet does not support the one predictor case. ")
        }
        ind0 = which(y == 0)  ##indices for class 0
        ind1 = which(y == 1)  ##indices for class 1
        n0 = length(ind0)
        n1 = length(ind1)
        if (split == 0) {
            ## no split, use all class 0 obs for training and for calculating the cutoff
            fits = npc.split(x, y, p, alpha, delta, ind0, ind0, ind1, ind1, method,
                n.cores = n.cores, warning = warning, ...)
        } else {
            ## with split
            n0.1 = round(n0 * split.ratio)  ##default size for calculating the classifier
            n1.1 = round(n1 * split.ratio)

            ind01.mat = sapply(1:split, f <- function(i) {
                sample(ind0, n0.1)
            })
            ind11.mat = sapply(1:split, f <- function(i) {
                sample(ind1, n1.1)
            })
            ind02.mat = sapply(1:split, f <- function(i) {
                setdiff(ind0, ind01.mat[, i])
            })
            ind12.mat = sapply(1:split, f <- function(i) {
                setdiff(ind1, ind11.mat[, i])
            })
            n0.cores = max(1, floor(n.cores/split))
            if (band == TRUE) {
                fits = mclapply(1:split, f <- function(i) {
                  set.seed(i + randSeed)
                  npc.split(x, y, p, alpha, delta, ind01.mat[, i], ind02.mat[,
                    i], ind11.mat[, i], ind12.mat[, i], method, n.cores = n0.cores, warning = warning, ...)
                }, mc.cores = n.cores)
            } else {
                fits = mclapply(1:split, f <- function(i) {
                  set.seed(i + randSeed)
                  npc.split(x, y, p, alpha, delta, ind01.mat[, i], ind02.mat[,
                    i], ind1, ind1, method, n.cores = n0.cores, warning = warning, ...)
                }, mc.cores = n.cores)
            }

        }
        object$fits = fits
        object$split = split
        object$method = method
        object$nsmall = fits[[1]]$nsmall
        object$split.ratio = split.ratio
        class(object) = "npc"
        rm(.Random.seed, envir=.GlobalEnv)
        return(object)
}
