find.optim.split <- function(x = NULL, y, method = c("logistic", "penlog", "svm", "randomforest", "lda", "slda", "nb", "ada", "tree"), alpha = 0.05, delta = 0.05, split = 1, split.ratio.seq = seq(from=0.1,to=0.9,by=0.1), nfolds = 10, band  = FALSE, randSeed = 0, ...){
  if (!is.null(x)) {
    x = as.matrix(x)
  }
  method = match.arg(method)
  set.seed(randSeed)
   n = length(y)
   object = NULL
   p = ncol(x)
   ind0 = which(y == 0)  ##indices for class 0
   ind1 = which(y == 1)  ##indices for class 1
   n0 = length(ind0)
   n1 = length(ind1)

   foldid = sample(rep(seq(nfolds), length = n1))

   n.split.ratio = length(split.ratio.seq)

   error = matrix(1.1, n.split.ratio,nfolds)
   for(i in 1:nfolds){
     which = foldid == i

     testind = ind1[which]
     xtrain = x[-testind,]
     ytrain = y[-testind]
     xtest = x[testind,]
     ytest = y[testind]

   for(split.ind in 1:n.split.ratio){
     fit = npc(x = xtrain, y = ytrain, method = method, alpha = alpha, delta = delta, split = split, split.ratio = split.ratio.seq[split.ind], band  = band, randSeed = randSeed, ...)
     if(fit$nsmall==FALSE){
       pred = predict(fit,xtest)
       error[split.ind, i] = mean(pred$pred.label!=ytest)
     }
   }

   }
   errorm = apply(error,1,mean)
   errorse = apply(error,1,sd)/sqrt(nfolds)
   loc.min = which.min(errorm)
   locs = which(errorm<errorm[loc.min]+2*errorse[loc.min])
   loc.1se = locs[which.min(abs(split.ratio.seq[locs]-0.5))]
   #loc.1se = min(which(errorm==max(errorm[locs])))
   split.ratio.min = split.ratio.seq[loc.min]
   split.ratio.1se = split.ratio.seq[loc.1se]
   #optim.split.ratio = split.ratio.seq[which.min(errorm)]
   list(split.ratio.min=split.ratio.min, split.ratio.1se = split.ratio.1se, error = error, errorse = errorse)
}

npc.core <- function(y, score, alpha = NULL, delta = 0.05, n.cores = 1, warning = TRUE) {

  ind0 = which(y == 0)
  ind1 = which(y == 1)
  if (length(ind0) == 0 || length(ind1) == 0) {
    stop("both class 0 and class 1 responses are needed to decide the cutoff.")
  }
  sig = mean(score[ind1]) > mean(score[ind0])  ##whether the class 0 has a larger average score than class 1
  if (sig == FALSE) {
    score = -score
  }
  # loc = bs(1:n0, method = 'bs', alpha, delta, n.cores = n.cores)

  score0 = sort(score[ind0])
  score1 = sort(score[ind1])
  obj = find.order(score0, score1, delta, n.cores = n.cores)

  cutoff.list = obj$scores


  cutoff = NULL
  alpha.n = length(obj$alpha.u)
  min.alpha = sort(obj$alpha.u,partial=2)[2]
  nsmall = FALSE
  if (!is.null(alpha)) {
    if (min.alpha > alpha + 1e-10){
      cutoff = Inf
      loc = length(ind0)
      nsmall = TRUE
      if(warning){
        warning('Sample size is too small for the given alpha. Try a larger alpha.')
      }
    } else{
      loc = min(which(obj$alpha.u <= alpha + 1e-10))
      cutoff = cutoff.list[loc]
    }
  }
  return(list = list(cutoff = cutoff, loc = loc, sign = sig, alpha.l = obj$alpha.l, alpha.u = obj$alpha.u,
                     beta.l = obj$beta.l, beta.u = obj$beta.u, nsmall = nsmall))
}

pred.npc.core <- function(object, newx = NULL) {
  if (object$method == "logistic") {
    pred.score = predict(object$fit, data.frame(newx), type = "response")
  } else if (object$method == "penlog") {
    pred.score = predict(object$fit$glmnet.fit, newx = newx, type = "response", s = object$fit$lambda.min)
  } else if (object$method == "svm") {
    pred.score = attr(predict(object$fit, newx, decision.values = TRUE), "decision.values")[,1]
  } else if (object$method == "randomforest") {
    pred.score = predict(object$fit, newx, type = "prob")[, 2]
  } else if (object$method == "lda") {
    pred.score = predict(object$fit, data.frame(newx))$posterior[, 2]
  } else if (object$method == "slda") {
    pred.score = predict(object$fit$glmnet.fit, newx = newx, s = object$fit$lambda.min)
  } else if (object$method == "nb") {
    pred.score = predict(object$fit, data.frame(newx), type = "raw")[, 2]
  } else if (object$method == "ada") {
    pred.score = predict(object$fit, data.frame(newx), type = "probs")[, 2]
  } else if (object$method == 'tree'){
    pred.score = predict(object$fit, data.frame(newx), type = 'vector')
  }
  pred.score = as.vector(pred.score)
  if (object$sign == FALSE) {
    pred.score = -pred.score
  }
  pred.label = outer(pred.score, object$cutoff, ">")
  list(pred.label = pred.label, pred.score = pred.score)

}

classification.core <- function(method, train.x, train.y, test.x, ...){
  if (method == "logistic") {
    train.data = data.frame(train.x, y = train.y)
    fit = glm(y ~ ., data = train.data, family = "binomial")
    test.score = predict(fit, data.frame(test.x), type = "response")
  } else if (method == "penlog") {
    fit = cv.glmnet(train.x, train.y, family = "binomial", ...)
    test.score = predict(fit$glmnet.fit, newx = test.x, type = "response", s = fit$lambda.min)
    test.score = as.vector(test.score)
  } else if (method == "svm") {
    train.y = as.factor(train.y)
    fit = svm(train.x, train.y, ...)
    test.score = attr(predict(fit, test.x, decision.values = TRUE), "decision.values")[,1]
  } else if (method == "randomforest") {
    train.y = as.factor(train.y)
    fit = randomForest(train.x, train.y, ...)
    test.score = predict(fit, test.x, type = "prob")[, 2]
  } else if (method == "lda") {
    fit = lda(train.x, train.y)
    test.score = predict(fit, data.frame(test.x))$posterior[, 2]
  } else if (method == "slda") {
    n1 = sum(train.y==1)
    n0 = sum(train.y==0)
    n = n1 + n0
    lda.y = train.y
    lda.y[train.y == 0] = -n/n0
    lda.y[train.y == 1] = n/n1
    fit = cv.glmnet(train.x, lda.y, ...)
    test.score = predict(fit$glmnet.fit, newx = test.x, type = "link", s =       fit$lambda.min)
    test.score = as.vector(test.score)
  } else if (method == "nb") {
    fit = naiveBayes(train.x, train.y)
    test.score = predict(fit, data.frame(test.x), type = "raw")[, 2]
  } else if (method == "ada") {
    train.data = data.frame(train.x, y = train.y)
    fit = ada(y ~ ., data = train.data)
    test.score = predict(fit, data.frame(test.x), type = "probs")[, 2]
  } else if (method == 'tree') {
    train.y = as.factor(train.y)
    train.data = data.frame(train.x, y = train.y)
    fit = tree(y~ ., data = train.data)
    test.score = predict(fit, newdata = data.frame(test.x), type = 'vector')
  }
  return(list(fit = fit, test.score = test.score))
}

npc.split <- function(x, y, p, alpha, delta, ind01, ind02, ind11, ind12, method,
                      n.cores = 1, warning = TRUE, ...) {
  train.ind = c(ind01, ind11)
  test.ind = c(ind02, ind12)
  train.x = x[train.ind, , drop = FALSE]
  train.y = y[train.ind]
  test.x = x[test.ind, , drop = FALSE]
  test.y = y[test.ind]
  colnames(train.x) = paste("x", 1:p, sep = "")
  colnames(test.x) = paste("x", 1:p, sep = "")
  class.obj = classification.core(method, train.x, train.y, test.x, ...)
  fit = class.obj$fit
  test.score = class.obj$test.score
  obj = npc.core(test.y, test.score, alpha = alpha, delta = delta, n.cores = n.cores, warning = warning)

  object = list(fit = fit, y = test.y, score = test.score, cutoff = obj$cutoff,
                sign = obj$sign, method = method, beta.l = obj$beta.l, beta.u = obj$beta.u, alpha.l = obj$alpha.l,
                alpha.u = obj$alpha.u, nsmall = obj$nsmall)
  class(object) = "npc"
  return(object)
}
getroc <- function(yhat, y) {
  TPR = apply(yhat, 2, f <- function(x) {
    sum((x == y) & (y == 1))/sum(y == 1)
  })
  FPR = apply(yhat, 2, f <- function(x) {
    sum((x != y) & (y == 0))/sum(y == 0)
  })
  cbind(FPR, TPR)
}






## find the order such that the type-I error bound is satisfied with probability at
## least 1-delta
find.order <- function(score0, score1 = NULL, delta = 0.05, n.cores = 1) {
  ##score0 contains all the candidate cutoffs for the classifier.

  score0 = sort(score0)
  score1 = sort(score1)
  n0 = length(score0)
  n1 = length(score1)
  if(length(unique(score0))<10) { ##add small random noise if too few unique values.
    sd0 = sd(score0)
    sd1 = sd(score1)
    score0 = score0 + rnorm(n0, sd = max(1, sd0)*1e-6)
    score1 = score1 + rnorm(n1, sd = max(1, sd1)*1e-6)
  }
  scores = score0
  alpha.l = alpha.u = beta.l = beta.u = rep(0, n0)
  #alpha.l: type I error lower bound
  #alpha.u: type I error upper bound
  #beta.l: type II error lower bound
  #beta.u: type II error upper bound
  v.list = seq(from = 0, to = 1, by = 0.001)
  #ru0 = mcmapply(function(s){sum(score0 <= s)}, scores, mc.cores = n.cores)
  #rl0 = n0 + 1 - mcmapply(function(s){sum(score0 >= s)}, scores, mc.cores = n.cores)
  ru0 = rank(score0, ties.method = 'max')
  rl0 = rank(score0, ties.method = 'min')
  rl1 = mcmapply(function(s){sum(score1 < s) + s %in% score1 }, scores, mc.cores = n.cores)
  ru1 = mcmapply(function(s){sum(score1 < s) + max(1,sum(score1 == s)) }, scores, mc.cores = n.cores)


  alpha.mat = mcmapply(f <- function(i){
    if (ru0[i] == n0){
      alpha.l = 0
    }  else {
      alpha.l = v.list[max(which(pbinom(n0 - rl0[i], n0, v.list) >= 1 - delta))]
    }
    alpha.u = v.list[min(which(pbinom(n0 - ru0[i], n0, v.list) <= delta))]

    # if (ru0[i] == 0) {
    #   alpha.u = 1
    #   alpha.l = v.list[max(which(pbinom(n0 - rl0[i], n0, v.list) >= 1 - delta))]
    # } else if (rl0[i] == n0) {
    #   alpha.l = 0
    #   alpha.u = v.list[min(which(pbinom(n0 - ru0[i], n0, v.list) <= delta))]
    # } else {
    #   alpha.u = v.list[min(which(pbinom(n0 - ru0[i], n0, v.list) <= delta))]
    #   alpha.l = v.list[max(which(pbinom(n0 - rl0[i], n0, v.list) >= 1 - delta))]
    # }
     if (ru1[i] >= n1) {
      beta.u = 1
      beta.l = v.list[max(which(pbinom(rl1[i]-1, n1, v.list) >= 1 - delta))]
    } else if (rl1[i] == 0) {
      beta.u = v.list[min(which(pbinom(ru1[i]-1, n1, v.list) <= delta))]
      beta.l = 0
    } else {
      beta.u = v.list[min(which(pbinom(ru1[i]-1, n1, v.list) <= delta))]
      beta.l = v.list[max(which(pbinom(rl1[i]-1, n1, v.list) >= 1 - delta))]
    }
    c(alpha.l, alpha.u, beta.l, beta.u)
  }, 1:n0, mc.cores = n.cores)
  alpha.l = alpha.mat[1, ]
  alpha.u = alpha.mat[2, ]
  beta.l = alpha.mat[3, ]
  beta.u = alpha.mat[4, ]


  return(list(scores = scores, beta.l = beta.l, beta.u = beta.u, alpha.l = alpha.l, alpha.u = alpha.u, ru1=ru1,rl1=rl1,ru0=ru0,rl0=rl0))

}



