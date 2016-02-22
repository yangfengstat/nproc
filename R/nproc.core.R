nproc.core <- function(x = NULL, y, method = c("logistic", "penlog", "svm", "randomforest",
                                          "lda", "nb", "ada", "custom"), score = NULL, conf = FALSE,
                  alphalist = seq(from = 0.01, to = 0.99,by = 0.01), delta = 0.05,
                  split = TRUE, cv = FALSE, fold = 5, loc.prob = NULL, n.cores = 1) {
  method = match.arg(method)

    if (!cv) {
      fit = npc(x, y, method, score = score, alpha = alphalist, delta = delta,
                split = split, n.cores = n.cores)
      # if(is.null(loc.prob)) loc.prob = fit$loc.prob obj = npc.core(fit$y, fit$prob,
      # alpha = alphalist, delta = delta, loc.prob = loc.prob)
      v = getroc(fit$pred.y, fit$y)
    } else {
      n = length(y)
      n.fold = ceiling(n/fold)
      ind = sample(1:n)
      rocmat = array(0, c(fold, length(alphalist), 2))
      for (i in 1:fold) {
        cind = (i - 1) * n.fold + 1:n.fold
        cind = intersect(1:n, cind)
        te.ind = ind[cind]
        tr.ind = setdiff(1:n, te.ind)
        fit = npc(x[tr.ind, ], y[tr.ind], method, score = score[tr.ind], alpha = alphalist,
                  delta = delta, split = split, loc.prob = loc.prob, n.cores = n.cores)
        if (is.null(loc.prob))
          loc.prob = fit$loc.prob
        pred = predict(fit, x[te.ind, ], score[te.ind])
        rocmat[i, , ] = getroc(pred$pred.label, y[te.ind])
      }
      v = apply(rocmat, c(2, 3), mean)

    }
  if (is.null(loc.prob))
    loc.prob = fit$loc.prob

  return(list(v = v, loc.prob = loc.prob))
}
