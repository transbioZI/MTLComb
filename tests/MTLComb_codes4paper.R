



################################################################################
#                                                                              #  
#           This script contained the codes for replicating                    #
#                the results shown in the MTLComb paper                        #
#                                                                              #
################################################################################




rm(list=ls())
gc()


library("MTLComb")



################################################################################
# 0, generate classification and regression simulation data
################################################################################
source ("tests/generate_simulation_data.R")









################################################################################
# 1, demonstrate regularization path for both regression and classification tasks
################################################################################
#parameters
opts <- list(init=0, tol=10^-6, maxIter=10000, ter=2)
fit1=MTLComb_Train(X=X, Y=Y, nlambda=200, lam_ratio=0.001, ctasks=ctasks, opts=opts)
#plot regularization path for regression tasks
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x[,1:10]^2)))), type="l", 
        xlab="lambda sequence", ylab="importance of features")
#plot regularization path for classification tasks
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x[,11:20]^2)))), type="l", 
        xlab="lambda sequence", ylab="importance of features")
################################################################################


################################################################################
# 2, demonstrate lambda selection using cv
################################################################################
# 10 folds cv
cvResult=MTLComb_CV(X=X, Y=Y, nfolds=10, lam_ratio=0.001, nlambda=200, ctasks=ctasks, opts=opts)
plot_MTLComb_CV(cvResult)
################################################################################







################################################################################
# 3, demonstrate the joint feature selection
################################################################################
fit2=MTLComb_Train(X=X, Y=Y, nlambda=200, lambda=cvResult$lam_seq[50], ctasks=ctasks, opts=opts)
par(mfrow=c(2,1))
image(W!=0, xlab="features", ylab="tasks", main="ground truth")
image(fit2$ws[[1]]!=0, xlab="features", ylab="tasks", main="lambda selected from cv", )
par(mfrow=c(1,1))




################################################################################
# 4, demonstrate the prediction performance on the independent data cohorts
################################################################################
ev_reg=vector()
for (i in 1:10){
  predScores=predict_MTLComb(fit2, newx=tX[[i]], type="regress")[[1]][,i]
  
  ev_reg=c(ev_reg, summary(lm(tY[[i]]~predScores))$adj.r.squared)
} 
mean(ev_reg) #ave=0.9464027

ev_cla=vector()
library(fmsb)
for (i in 11:20){
  predScores=predict_MTLComb(fit2, newx=tX[[i]], type="classify")[[1]][,i-10]
  
  ev_cla=c(ev_cla, NagelkerkeR2(glm(as.factor(tY[[i]])~predScores, family = "binomial"))$R2)
} 
mean(ev_cla)#ave=0.8198122

