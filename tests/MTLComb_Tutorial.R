
################################################################################
#                                                                              #  
#           This script contained the codes for demonstrating the              # 
#           functions of MTLComb as the tutorial                               #
#                                                                              #
################################################################################





rm(list=ls())
gc()


library("MTLComb")



################################################################################
# 0, generate classification and regression simulation data
################################################################################

# Create 20 tasks, First 10 are regression tasks, and the second 10 is classification tasks.
# Each task contains 200 features, 100 subjects. The first 10 features are true predictive features

#generate classification tasks
t <- 10 # number of classification tasks
p <- 200 # number of features
n <- 100 # number of subjects
W <- matrix(data=rnorm(t*p),ncol=t, nrow = p) # create coefficient matrix
W[1:p*0.9,] <- 0 # create sparsity
X <- list(); Y <- list(); tX <- list(); tY <- list()
for(i in 1:t){
  X[[i]] <- matrix(data=rnorm(n*p),ncol=p, nrow = n) # feature matrix
  Y[[i]] <- sign(X[[i]] %*% W[,i] + 0.5 * rnorm(n)) # outcome
  tX[[i]] <- matrix(rnorm(p*n),nrow=n) #test data
  tY[[i]] <- sign(tX[[i]] %*% W[, i] + rnorm(n) * 0.5) #test data
}
cX <- X; cY <- Y; cW <- W; ctX <- tX; ctY <- tY;

#generate regression tasks
t <- 10 # number of regression tasks
p <- 200 # number of features
n <- 100 # number of subjects
W <- matrix(data=rnorm(t*p),ncol=t, nrow = p) # create coefficient matrix
W[1:p*0.9,] <- 0 # create sparsity
X <- list(); Y <- list(); tX <- list(); tY <- list()
for(i in 1:t){
  X[[i]] <- matrix(data=rnorm(n*p),ncol=p, nrow = n) # feature matrix
  Y[[i]] <- X[[i]] %*% W[,i] + 0.5 * rnorm(n) # outcome
  tX[[i]] <- matrix(rnorm(p*n),nrow=n) #test data
  tY[[i]] <- tX[[i]] %*% W[, i] + rnorm(n) * 0.5 #test data
}

X <- c(X,cX); Y <- c(Y,cY); W <- cbind(W,cW);
tX <- c(tX, ctX); tY <- c(tY, ctY)




#parameters
opts <- list(init=0, tol=10^-6, maxIter=10000, ter=2)
ctasks <- 11:20





################################################################################
# 1, test solver
################################################################################
s1 = solve_L21 (X, Y, lam=1, C=0, C2=0, ctasks=ctasks, opts=opts)
# the number of non-zero coefficients
sum(sqrt(rowSums(s1$W^2))!=0) 
# 20

# change to a smaller lam -> more number of selected features
s2 = solve_L21 (X, Y, lam=0.1, C=0, C2=0, ctasks=ctasks, opts=opts)
sum(sqrt(rowSums(s2$W^2))!=0)
# 158

# enable the parameter C
s3 = solve_L21 (X, Y, lam=1, C=0,  C2=1, ctasks=ctasks, opts=opts)
sum(sqrt(rowSums(s3$W^2))!=0)
#80

# demonstrate the objective
plot(s1$Obj, xlab="interations", ylab="objective value")
plot(s2$Obj, xlab="interations", ylab="objective value")
plot(s3$Obj, xlab="interations", ylab="objective value")
################################################################################








################################################################################
# 2, test training procedure
################################################################################
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
################################################################################


################################################################################
# 3, test cross-validation
################################################################################
cvResult=MTLComb_CV(X=X, Y=Y, nfolds=10, lam_ratio=0.01, nlambda=20, ctasks=ctasks, opts=opts)
plot_MTLComb_CV(cvResult)
#demonstrated the automatically selected lambda
cvResult$lambda.regress.min
#0.1415105
cvResult$lambda.classify.min
#0.1803233
cvResult$lambda.weighted.min
#0.1609169
cvResult$lam_seq[15]
#0.1803233 
cvResult$lam_seq[10]
#0.6058536 

# training with different selected lambda
fit5=MTLComb_Train(X=X, Y=Y, nlambda=50, lambda=cvResult$lam_seq[10], ctasks=ctasks, opts=opts)
fit6=MTLComb_Train(X=X, Y=Y, nlambda=50, lambda=cvResult$lambda.weighted.min, ctasks=ctasks, opts=opts)
#plot the feature selection result
par(mfrow=c(2,1))
image(fit5$ws[[1]], xlab="features", ylab="tasks", main="lambda selected mannuelly")
image(fit6$ws[[1]], xlab="features", ylab="tasks", main="lambda selected automatically")
par(mfrow=c(1,1))
################################################################################









################################################################################
# 4, test prediction
################################################################################
str(tY)
predScores=predict_MTLComb(fit5, newx=tX[[5]], type="regress")[[1]][,5]
plot(predScores, tY[[5]], xlab="prediction scores", ylab="true outcome")
predScores=predict_MTLComb(fit5, newx=tX[[15]], type="classify")[[1]][,5]
plot(as.factor(tY[[15]]), predScores, xlab="classes", ylab="prediction probability")
################################################################################

