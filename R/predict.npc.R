#' Predicting the outcome of a set of new observations using the fitted npc
#' object.
#' @export
#' @param object fitted npc object using \code{npc}.
#' @param newx a set of new observations.
#' @param pred.score a vector of scores for the new observations. Used when method = 'custom'.
#' @param ... additional arguments.
#' @return A list containing the predicted label and score.
#' \item{pred.label}{Predicted label vector.}
#' \item{pred.score}{Predicted score vector.}
#' @seealso \code{\link{npc}} and \code{\link{nproc}}
#' @examples
#' n = 1000
#' x = matrix(rnorm(n*2),n,2)
#' c = 1+3*x[,1]
#' y = rbinom(n,1,1/(1+exp(-c)))
#' xtest = matrix(rnorm(n*2),n,2)
#' ctest = 1+3*xtest[,1]
#' ytest = rbinom(n,1,1/(1+exp(-ctest)))
#'
#' ##Use logistic classifier and the default type I error control with alpha=0.05
#' fit = npc(x, y, method = 'logistic')
#' pred = predict(fit,xtest)
#' fit.score = predict(fit,x)
#' accuracy = mean(pred$pred.label==ytest)
#' cat('Overall Accuracy: ',  accuracy,'\n')
#' ind0 = which(ytest==0)
#' ind1 = which(ytest==1)
#' typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
#' cat('Type I error: ', typeI, '\n')
#' typeII = mean(pred$pred.label[ind1]!=ytest[ind1]) #type II error on test set
#' cat('Type II error: ', typeII, '\n')

predict.npc <- function(object, newx = NULL, pred.score = NULL, ...) {
  if (object$method == "custom") {
    if (is.null(pred.score)) {
      stop("pred.score needed for method \"custom\".")
    }
    pred.score = pred.score
  } else {
    colnames(newx) <- paste("x", 1:ncol(newx), sep = "")
    if (object$method == "logistic") {
      pred.score = predict(object$fit, data.frame(newx), type = "response")
    } else if (object$method == "penlog") {
      pred.score = predict(object$fit$glmnet.fit, newx = newx, type = "response",
                           s = object$fit$lambda.min)
    } else if (object$method == "svm") {
      pred.score = attr(predict(object$fit, newx, decision.values = TRUE), "decision.values")[,
                                                                                        1]
    } else if (object$method == "randomforest") {
      pred.score = predict(object$fit, newx, type = "prob")[, 2]
    } else if (object$method == "lda") {
      pred.score = predict(object$fit, data.frame(newx))$posterior[, 2]
    } else if (object$method == "nb") {
      pred.score = predict(object$fit, data.frame(newx), type = "raw")[, 2]
    } else if (object$method == "ada") {
      pred.score = predict(object$fit, data.frame(newx), type = "probs")[, 2]
    }
  }
  if (object$sign == TRUE) {
    pred.label = outer(pred.score, object$cutoff, ">")
  } else {
    pred.label = outer(pred.score, object$cutoff, "<=")
  }
  return(list(pred.label = pred.label, pred.score = pred.score))

}
