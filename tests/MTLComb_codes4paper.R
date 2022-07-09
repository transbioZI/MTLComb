
################################################################################
#
# Package Name: MTLComb
# Description: multi-task learning combining calssification and regressin tasks with joint feature selection
#
# Copyright (C) 2022  Dr. Han Cao (hank9cao@gmail.com)
# All rights reserved.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
################################################################################












rm(list=ls())
gc()
source('MTLComb.R')
load('simulated_data.rda')


#parameters
str(Y)
opts <- list(init=0, tol=10^-6, maxIter=10000, ter=2)
ctasks=ctasks



####################
#demonstrate regularization path for both regression and classification tasks
####################
#use case 1
fit1=MTLComb_Train(X=X, Y=Y, nlambda=200, lam_ratio=0.001, ctasks=ctasks, opts=opts)
#plot regularization tree
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x[,1:10]^2)))), type="l", xlab="lambda sequence", ylab="importance of features")
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x[,11:20]^2)))), type="l", xlab="lambda sequence", ylab="importance of features")



#####################
#demonstrate lambda selection using cv
#####################
cvResult=MTLComb_CV(X=X, Y=Y, nfolds=10, lam_ratio=0.001, nlambda=200, ctasks=ctasks, opts=opts)
plot_MTLComb_CV(cvResult)
cvResult$lambda.regress.min
#0.1689682
cvResult$lambda.classify.min
#0.2309316
cvResult$lambda.weighted.min
#0.1999499
cvResult$lam_seq[50]
#0.8636699

fit2=MTLComb_Train(X=X, Y=Y, nlambda=200, lambda=cvResult$lam_seq[50], ctasks=ctasks, opts=opts)
par(mfrow=c(2,1))
image(W!=0, xlab="features", ylab="tasks", main="ground truth")
image(fit2$ws[[1]]!=0, xlab="features", ylab="tasks", main="lambda selected from cv", )
par(mfrow=c(1,1))

#explained variance
ev_reg=vector()
for (i in 1:10){
  predScores=predict_MTLComb(fit2, newx=tX[[i]], type="regress")[[1]][,i]
  
  ev_reg=c(ev_reg, summary(lm(tY[[i]]~predScores))$adj.r.squared)
} #ave=0.9464027

ev_cla=vector()
library(fmsb)
for (i in 11:20){
  predScores=predict_MTLComb(fit2, newx=tX[[i]], type="classify")[[1]][,i-10]
  
  ev_reg=c(ev_reg, NagelkerkeR2(glm(as.factor(tY[[i]])~predScores, family = "binomial"))$R2)
} #ave=0.8198122

