#' Calculate the Receiver Operating Characteristics with Cross-validation or Subsampling
#'
#' \code{rocCV} calculates the receiver operating characterisitc with cross-validation
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#' @export
#' @param x n * p observation matrix. n observations, p covariates.
#' @param y n 0/1 observatons.
#' @param method classification method(s).
#' \itemize{
#' \item logistic: Logistic regression. \link{glm} function with family = 'binomial'
#' \item penlog: Penalized logistic regression with LASSO penalty. \code{\link[glmnet]{glmnet}} in \code{glmnet} package
#' \item svm: Support Vector Machines. \code{\link[e1071]{svm}} in \code{e1071} package
#' \item randomforest: Random Forest. \code{\link[randomForest]{randomForest}} in \code{randomForest} package
#' \item Linear Discriminant Analysis. lda: \code{\link[MASS]{lda}} in \code{MASS} package
#' \item nb: Naive Bayes. \code{\link[e1071]{naiveBayes}} in \code{e1071} package
#' \item ada: Ada-Boost. \code{\link[ada]{ada}} in \code{ada} package
#' }
#' @param metric metric used for averging performance. Includes 'CV' and 'SS' as options. Default = 'CV'.
#' @param n.folds number of folds used for cross-validation or the number of splits in the subsampling. Default = 5.
#' @param train.frac fraction of training data in the subsampling process. Default = 0.5.
#' @param n.cores number of cores used for parallel computing. Default = 1.
#' @param randSeed the random seed used in the algorithm. Default = 0.
#' @param ... additional arguments.
#' @return A list.
#' \item{fpr}{sequence of false positive rate.}
#' \item{tpr}{sequence of true positive rate.}
#' @seealso \code{\link{nproc}}
#' @references
#' Xin Tong, Yang Feng, and Jingyi Jessica Li (2016), Neyman-Pearson (NP) classification algorithms and NP receiver operating characteristic (NP-ROC), manuscript, http://arxiv.org/abs/1608.03109
#' @examples
#' n = 200
#' x = matrix(rnorm(n*2),n,2)
#' c = 1 - 3*x[,1]
#' y = rbinom(n,1,1/(1+exp(-c)))
#' fit = rocCV(x, y, method = 'svm')
#' fit2 = rocCV(x, y, method = 'penlog')
#' fit3 = rocCV(x, y, method = 'penlog', metric = 'SS')




rocCV <- function(x = NULL, y, method = c("logistic", "penlog", "svm", "randomforest",
    "lda", "nb", "ada", "tree"), metric = "CV",  n.folds = 5, train.frac = 0.5, n.cores = 1, randSeed = 0, ...) {
    if (!is.null(x)) {
        x = as.matrix(x)
        p = ncol(x)
        if (p == 1 & method == "penlog") {
            stop("glmnet does not support the one predictor case. ")
        }
    }
    method = match.arg(method)
    set.seed(randSeed)


    roc.tpr.mat = NULL
    common.fpr.list = seq(from = 0,to=1,by=0.001)
    foldid = sample(rep(seq(n.folds), length = length(y)))
    n = length(y)
    roc.tpr.mat = mcmapply(f <- function(fold){
      if(metric == 'CV'){
        which = (foldid == fold)
      } else{
        which = as.logical(rbinom(n,1,train.frac))
      }
      train.x = x[!which, , drop = FALSE]
      train.y = y[!which]
      test.x = x[which, , drop = FALSE]
      test.y = y[which]
      colnames(train.x) = paste("x", 1:p, sep = "")
      colnames(test.x) = paste("x", 1:p, sep = "")
      class.obj = classification.core(method, train.x, train.y, test.x, ...)
      test.score = class.obj$test.score

      roc.prediction = ROCR::prediction(test.score , as.factor(test.y))
      perf <- ROCR::performance(roc.prediction,"tpr","fpr")
      fpr = perf@x.values[[1]]
      tpr = perf@y.values[[1]]

      tpr.interpolate = approx(fpr, tpr, common.fpr.list,
                               method = "constant",f = 1,rule = 2)$y
    }, 1:n.folds, mc.cores = n.cores)

    roc.cv.tpr = apply(roc.tpr.mat,1,mean)

    roc.cv.fpr = common.fpr.list

    object = list(fpr = roc.cv.fpr, tpr = roc.cv.tpr)
    rm(.Random.seed, envir=.GlobalEnv)
    return(object)
}
