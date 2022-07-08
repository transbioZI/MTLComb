rm(list=ls())
source('MTComb_L21.R')

#load data
load('simulated_data.rda')

#z-score normalization
#Y[1:10] <- lapply(Y[1:10],
#                  function(x) scale(x, center = TRUE, scale = TRUE))
#tY[1:10] <- lapply(tY[1:10],
#                  function(x) scale(x, center = TRUE, scale = TRUE))
#X <- lapply(X, function(x)
#    apply(x, 2, function(xx) scale(xx, center = TRUE, scale = TRUE)))
#tX <- lapply(tX, function(x)
#    apply(x, 2, function(xx) scale(xx, center = TRUE, scale = TRUE)))

#parameters
str(Y)
opts <- list(init=0, tol=10^-6, maxIter=10000, ter=2)
ctasks=ctasks


####################
#test solver
####################
s1 = solve_L21 (X, Y, lam=1, C=0, ctasks=ctasks, opts=opts)
sum(sqrt(rowSums(s1$W^2))!=0)
#20
s2 = solve_L21 (X, Y, lam=0.1, C=0, ctasks=ctasks, opts=opts)
sum(sqrt(rowSums(s2$W^2))!=0)
#163
s3 = solve_L21 (X, Y, lam=1, C=1, ctasks=ctasks, opts=opts)
sum(sqrt(rowSums(s3$W^2))!=0)
#109
plot(s1$Obj, xlab="interations", ylab="objective value")
plot(s2$Obj, xlab="interations", ylab="objective value")
plot(s3$Obj, xlab="interations", ylab="objective value")

####################
#test training
####################
#use case 1
fit1=MTLComb_Train(X=X, Y=Y, nlambda=50, lam_ratio=0.001, ctasks=ctasks, opts=opts)
#plot regularization tree
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x^2)))), type="l", xlab="lambda sequence", ylab="importance of features")
#use case 2
fit2=MTLComb_Train(X=X, Y=Y, lambda=0.1, ctasks=ctasks, opts=opts)
plot(fit2$Obj, xlab="interations", ylab="objective value")
image(fit2$ws[[1]], xlab="features", ylab="tasks")
#use case 3
fit3=MTLComb_Train(X=X, Y=Y, lambda=c(1, 0.1, 0.01, 0.001), ctasks=ctasks, opts=opts)
matplot(t(sapply(fit3$ws, function(x)sqrt(rowSums(x^2)))), type="l", xlab="lambda sequence", ylab="importance of features")

#intercept model
fit4=MTLComb_Train(X=X, Y=Y, nlambda=50, lam_ratio=0.001, ctasks=ctasks, opts=opts, intercept = T)
dim(fit4$ws[[40]])
#intercepts
fit4$ws[[40]][1,]



#####################
#test cross-validation
#####################
cvResult=MTLComb_CV(X=X, Y=Y, nfolds=10, lam_ratio=0.01, nlambda=20, ctasks=ctasks, opts=opts)
plot_MTLComb_CV(cvResult)
cvResult$lambda.regress.min
#0.1801166
cvResult$lambda.classify.min
#0.1413483
cvResult$lambda.weighted.min
#0.1607325
cvResult$lam_seq[15]
#0.1801166
cvResult$lam_seq[10]
#0.6051592

fit5=MTLComb_Train(X=X, Y=Y, nlambda=50, lambda=cvResult$lam_seq[10], ctasks=ctasks, opts=opts)
fit6=MTLComb_Train(X=X, Y=Y, nlambda=50, lambda=cvResult$lambda.weighted.min, ctasks=ctasks, opts=opts)
par(mfrow=c(2,1))
image(fit5$ws[[1]], xlab="features", ylab="tasks", main="lambda selected mannuelly")
image(fit6$ws[[1]], xlab="features", ylab="tasks", main="lambda selected automatically")
par(mfrow=c(1,1))



#####################
#test prediction
#####################
str(tY)
predScores=predict_MTLComb(fit5, newx=tX[[5]], type="regress")
plot(rowMeans(predScores[[1]]), tY[[5]], xlab="averaged prediction scores", ylab="true outcome")
predScores=predict_MTLComb(fit5, newx=tX[[15]], type="classify")
plot(as.factor(tY[[15]]), rowMeans(predScores[[1]]), xlab="classes", ylab="averaged prediction probability")


