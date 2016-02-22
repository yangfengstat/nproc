## ---- eval=FALSE---------------------------------------------------------
#  install.packages("nproc", repos = "http://cran.us.r-project.org")

## ------------------------------------------------------------------------
library(nproc)

## ------------------------------------------------------------------------
n = 1000
set.seed(0)
x = matrix(rnorm(n*2),n,2)
c = 1+3*x[,1]
y = rbinom(n,1,1/(1+exp(-c)))

## ------------------------------------------------------------------------
fit = npc(x, y, method = "svm", alpha = 0.05)

## ------------------------------------------------------------------------
xtest = matrix(rnorm(n*2),n,2)
ctest = 1+3*xtest[,1]
ytest = rbinom(n,1,1/(1+exp(-ctest)))

## ------------------------------------------------------------------------
pred = predict(fit,xtest)
fit.score = predict(fit,x)
accuracy = mean(pred$pred.label==ytest)
cat("Overall Accuracy: ",  accuracy,'\n')
ind0 = which(ytest==0)
typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
cat('Type I error: ', typeI, '\n')

## ------------------------------------------------------------------------
fit = npc(x, y, method = "logistic", alpha = 0.1)
pred = predict(fit,xtest)
accuracy = mean(pred$pred.label==ytest)
cat("Overall Accuracy: ",  accuracy,'\n')
ind0 = which(ytest==0)
typeI = mean(pred$pred.label[ind0]!=ytest[ind0]) #type I error on test set
cat('Type I error: ', typeI, '\n')

## ------------------------------------------------------------------------
fit2 = npc(y = y, score = fit.score$pred.score,
pred.score = pred$pred.score, method = 'custom')

