
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



















################################################################################
#' @title The solver of MTLComb 
#'
#' @description Solve the objective of MTLComb
#' 
#' @param X A set of feature matrices (the mixed tasks with classification and regression types)
#' @param Y A set of responses, could be binary (classification problem) or continues (regression problem). The valid
#'   value of binary outcome \eqn{\in\{1, -1\}}
#' @param lam A positive constant \eqn{\lambda} to control the number of jointly selected features
#' @param C A non-negative constant to improve the generalization performance
#' @param ctasks the index of the classification tasks. For example \code{ctasks=c(2,3)} refered to 2th and 3th tasks 
#'   are classification
#' @param opts Options of the optimization procedure. One can set the initial search point, the tolerance and the 
#'   maximized number of iterations and the termination rule using this parameter. For example 
#'   \code{list(init=0,  tol=10^-3, maxIter=1000, ter=2)} 
#' @return The coefficients of the model and the historical objective values
#' 
#' @export
#' @author Dr. Han Cao
################################################################################
solve_L21 <- function (X, Y, lam, C, ctasks,opts){
  proximal_l21 <- function (W, lambda ){
    Fnorm <- sqrt(rowSums(W^2))
    zeros <- which(Fnorm==0)              
    prox_f_coef <- 1 - lambda/Fnorm
    prox_f_coef[zeros] <- 0
    prox_f_coef <- prox_f_coef *( prox_f_coef>0 )
    Wp = matrix(rep(prox_f_coef, ncol(W)), nrow=length(prox_f_coef))*W
    return(Wp)
  }
  
  nonsmooth_eval <- function (W){
    return(sum(sqrt(rowSums(W^2)))*lam)
  }
  
  LS_eval_gradient <- function(W, X, Y){
    grad_Ws=sapply(1:length(Y), function(k){(XtXr[[k]] %*% W[,k]  - XtYr[[k]])/nSubs[k]}) + 2* C * W
    funcVals=LS_eval_funVal(W, X, Y)
    return(list(grad_Ws=grad_Ws, funcVals=funcVals))
  }
  
  LS_eval_funVal <- function (W, X, Y){
    funcVal <- sum(sapply(1:length(Y), function(k){mean(0.5 * (Y[[k]] - X[[k]] %*% W[,k])^2)})) + C * norm(W, 'f')^2
    return(funcVal)
  }
  
  LR_eval_gradient <- function(W, X, Y){
    rData=lapply(1:length(X), function(k){
      x=X[[k]]; y=Y[[k]];w=W[,k]
      weight <- 1/length(y)
      l <- -y*(x %*% w) 
      lp <- l
      lp[lp<0] <- 0
      funcVal <- sum(weight * ( log( exp(-lp) +  exp(l-lp) ) + lp ))
      b <- (-weight*y)*(1 - 1/ (1+exp(l)))
      grad_w <- (t(x) %*% b)
      return(list(grad_w=grad_w, funcVal=funcVal))
    })
    grad_Ws=2*sapply(rData, function(x)x$grad_w) + 2* C * W
    funcVals=2*sum(sapply(rData, function(x)x$funcVal)) + C * norm(W, 'f')^2
    return(list(grad_Ws=grad_Ws, funcVals=funcVals))
  }
  
  LR_eval_funVal <- function (W, X, Y){
    funcVal <- sum(sapply(1:length(Y), function(k){
      x=X[[k]]; y=Y[[k]];w=W[,k]
      weight <- 1/length(y)
      l <- - y*(x %*% w)
      lp <- l
      lp[lp<0] <- 0
      return(sum(weight * ( log( exp(-lp) +  exp(l-lp) ) + lp )))
    }))*2 + C * norm(W, 'f')^2
    return(funcVal)
  }
  
  eval_gradient = function(W){
    Wc=W[,ctasks]; Wr=W[,rtasks]
    rc = LR_eval_gradient(W=Wc, X=Xc, Y=Yc)
    rr = LS_eval_gradient(W=Wr, X=Xr, Y=Yr)
    Wnew=W;
    Wnew[,ctasks]=rc$grad_Ws
    Wnew[,rtasks]=rr$grad_Ws
    funcVals=rc$funcVals + rr$funcVals
    return(list(grad_Ws=Wnew, funcVals=funcVals))
  }
  
  eval_funVal <- function (W){
    Wc=W[,ctasks]; Wr=W[,rtasks]
    rr = LS_eval_funVal(W=Wr, X=Xr, Y=Yr)
    rc = LR_eval_funVal(W=Wc, X=Xc, Y=Yc)
    return(rc+rr)
  }
  
  #################################  
  # Main algorithm
  #################################  
  Obj <- vector(); 
  nFeats=ncol(X[[1]])
  ntasks=length(X)
  nSubs=sapply(X, nrow)
  rtasks=setdiff(1:ntasks, ctasks)
  Xr=X[rtasks]; Yr=Y[rtasks]
  Xc=X[ctasks]; Yc=Y[ctasks]
  
  XtXr=lapply(1:length(Xr), function(k)t(Xr[[k]])%*%Xr[[k]])
  XtYr=lapply(1:length(Xr), function(k)t(Xr[[k]])%*%Yr[[k]])

  #initialize a starting point
  if(opts$init==0){
    w0=matrix(0,nrow=nFeats, ncol=length(X))
  }else if(opts$init==1){
    w0 <- opts$w0
  }    
  
  bFlag <- 0; 
  wz <- w0;
  wz_old <- w0;

  t <- 1;
  t_old <- 0;
  iter <- 0;
  gamma <- 1;
  gamma_inc <- 2;

  while (iter < opts$maxIter){
    alpha <- (t_old - 1) /t;
    ws <- (1 + alpha) * wz - alpha * wz_old;

    iter_update=eval_gradient(ws)  
    Gws <- iter_update[[1]]
    Fs <- iter_update[[2]]

    # the Armijo Goldstein line search scheme
    while (TRUE){
      wzp <- proximal_l21(ws - Gws/gamma, lam / gamma);
      Fzp=eval_funVal(wzp)


      delta_wzp <- wzp - ws;
      r_sum <- norm(delta_wzp, 'f')^2

      #second order approximation
      Fzp_gamma = Fs + sum(delta_wzp * Gws) + gamma * r_sum/2
    
      if (r_sum <=1e-20){
        bFlag=1; 
        break;
      }
      if (Fzp <= Fzp_gamma) break else {gamma = gamma * gamma_inc}
    }
  
    wz_old = wz; wz = wzp; Obj = c(Obj, Fzp + nonsmooth_eval(wz));
  
    #test stop condition.
    if (bFlag) break;
    if (iter>=1){
      if (opts$ter==1 & abs( Obj[length(Obj)] - Obj[length(Obj)-1] ) <= opts$tol){
        break
      } else if(opts$ter==2 & abs( Obj[length(Obj)] - Obj[length(Obj)-1] ) <= opts$tol*Obj[length(Obj)-1]){
        break
      } else if(opts$ter==3 & iter==opts$maxIter){
        break
      }
    }
  
    iter = iter + 1;
    t_old = t;
    t = 0.5 * (1 + (1+ 4 * t^2)^0.5);
  }
  return(list(W=wzp, Obj=Obj))
}

















################################################################################
#' @title Regularization path estimation 
#'
#' @description Train a series of MTLComb models indexed by a sequence of lambda
#' 
#' @param X A set of feature matrices (the mixed tasks with classification and regression types)
#' @param Y A set of responses, could be binary (classification problem) or continues (regression problem). The valid
#'   value of binary outcome \eqn{\in\{1, -1\}}
#' @param nlambda The number of lambda in the sequence. The default value is 10
#' @param lam_ratio The number (between 0 and 1) to determine the smallest lambda according to lam_max. For example 
#'   lam_ratio * lam_max. The default value is 0.01
#' @param lambda A positive constant \eqn{\lambda} to control the number of jointly selected features
#' @param C A non-negative constant to improve the generalization performance. The default value is 0 
#' @param ctasks The index of the classification tasks. For example \code{ctasks=c(2,3)} refers that 2th and 3th tasks 
#'   are classification
#' @param intercept True/False to indicator whether to use intercept or intercept-free model
#' @param opts Options of the optimization procedure. One can set the initial search point, the tolerance and the 
#'   maximized number of iterations and the termination rule using this parameter. The default value is 
#'   \code{list(init=0,  tol=10^-2, maxIter=100, ter=2)} 
#' @return The regularization path
#' 
#' @export
#' @author Dr. Han Cao
################################################################################
MTLComb_Train = function(X=NULL, Y=NULL, nlambda=10, lam_ratio=0.01, lambda=NULL, C=0, ctasks=NULL, intercept=F, 
                        opts=list(init=0, maxIter=20, tol=0.01, ter=2)){
  #intercept model
  if (intercept){
    X=lapply(X, function(x)cbind(1, x))
  }
  
  #initialize final result
  fit=list();fit$ws=list();fit$Obj=vector(); fit$C=C; fit$ctasks=ctasks; fit$intercept=intercept
  ntasks=length(X)
  rtasks=setdiff(1:ntasks, ctasks)
  xys=sapply(1:ntasks, function(k)t(X[[k]])%*%Y[[k]]/nrow(X[[k]]))
  xy_norm=sqrt(rowSums(xys^2))
  
  #determine the lambda sequence
  if(length(lambda)>1){
      lam_seq=lambda
  } else if(length(lambda)==1){
      lam_max=max(xy_norm)
      lam_min=lambda
      lam_seq=exp(seq(log(lam_max),log(lam_min),length.out = nlambda))
  } else if(is.null(lambda)){
      lam_max=max(xy_norm)
      lam_min=lam_ratio*lam_max
      lam_seq=exp(seq(log(lam_max),log(lam_min),length.out = nlambda))
  }

    #warm-start training procedure
    optsTrain=opts
    for(i in 1:length(lam_seq)){
      m=solve_L21(X=X, Y=Y, lam=lam_seq[i], C=C, opts=optsTrain, ctasks = ctasks)
      optsTrain$w0=m$W; optsTrain$init=1
      fit$ws[[i]]=m$W
      fit$Obj=c(fit$Obj, m$Obj)
      fit$Logs=rbind(fit$Logs, m$Logs)
      fit$gamma=c(fit$gamma, m$gamma)
    }
    fit$lam_seq=lam_seq
    names(fit$ws)=paste0("Lam=", round(lam_seq, 2))

  fit$ws=lapply(fit$ws, function(x){rownames(x)=colnames(X[[1]]); return(x)})
  if(length(lambda)==1){fit$ws=fit$ws[length(fit$ws)]}
  return(fit)
}













################################################################################
#' @title Get the data splits for CV 
#'
#' @description The subjects of each task will be randomly divided into k folds
#' 
#' @param Y A set of responses, could be binary (classification problem) or continues (regression problem). The valid
#'   value of binary outcome \eqn{\in\{1, -1\}}
#' @param cv_fold The number of folds for CV
#' @return The data splits of all folds
#' 
#' @author Dr. Han Cao
################################################################################
getCVPartition <- function(Y, cv_fold){
  task_num = length(Y);
  randIdx <- lapply(Y, function(x) sample(1:length(x), length(x), replace = FALSE))   
  
  cvPar = {};
  for (cv_idx in 1: cv_fold){
    # build cross validation data splittings for each task.
    cvTrain = {};
    cvTest = {};
    
    for (t in 1: task_num){
      task_sample_size <- length(Y[[t]]);
      te_idx <- seq(cv_idx, task_sample_size, by=cv_fold)
      tr_idx <- seq(1,task_sample_size)[!is.element(1:task_sample_size, te_idx)]
      cvTrain[[t]] = randIdx[[t]][tr_idx]
      cvTest[t] = list(randIdx[[t]][te_idx])
    }
    cvPar[[cv_idx]]=list(cvTrain=cvTrain, cvTest=cvTest);
  }
  return(cvPar)
}





################################################################################
#' @title k-fold CV
#'
#' @description Split the subjects of each task into k folds, training a MTLComb model on k-1 folds of data and test on the holding out fold. 
#' Repeat this procedure over all folds
#' 
#' @param X A set of feature matrices (the mixed tasks with classification and regression types)
#' @param Y A set of responses, could be binary (classification problem) or continues (regression problem). The valid
#'   value of binary outcome \eqn{\in\{1, -1\}}
#' @param nfolds The number of folds in CV. The default value is 20
#' @param nlambda The number of lambda in the sequence. The default value is 10
#' @param lam_ratio The number (between 0 and 1) to determine the smallest lambda according to lam_max. For example 
#'   lam_ratio * lam_max. The default value is 0.01
#' @param lambda A positive constant \eqn{\lambda} to control the number of jointly selected features
#' @param C A non-negative constant to improve the generalization performance. The default value is 0 
#' @param ctasks The index of the classification tasks. For example \code{ctasks=c(2,3)} refers that 2th and 3th tasks 
#'   are classification
#' @param intercept True/False indicator to tell whether intercept or intercept-free model
#' @param opts Options of the optimization procedure. One can set the initial search point, the tolerance and the 
#'   maximized number of iterations and the termination rule using this parameter. The default value is 
#'   \code{list(init=0,  tol=10^-4, maxIter=50, ter=2)} 
#' @return The CV results
#' 
#' @export
#' @author Dr. Han Cao
################################################################################
MTLComb_CV = function(X=NULL, Y=NULL, nfolds=10, lam_ratio=0.01, nlambda=10, lambda=NULL, ctasks=NULL, intercept=F, 
                             opts=list(init=0, maxIter=50, tol=0.001, ter=2), C=0){
  library("pROC")
  
  ntasks=length(X)
  rtasks=setdiff(1:ntasks, ctasks)
  cvResult=list(); cvResult$C=C; 
  cvPar <- getCVPartition(Y, nfolds)
  
  mse_fold=vector()
  auc_fold=vector()
  lam_seq=vector()
  for (i in 1:length(cvPar)){
    Xtrain=lapply(c(1:ntasks), function(k) X[[k]][cvPar[[i]][[1]][[k]], ])
    Ytrain=lapply(c(1:ntasks), function(k) Y[[k]][cvPar[[i]][[1]][[k]]])
    Xtest=lapply(c(1:ntasks), function(k) X[[k]][cvPar[[i]][[2]][[k]], ])
    Ytest=lapply(c(1:ntasks), function(k) Y[[k]][cvPar[[i]][[2]][[k]]])
      
    fit=MTLComb_Train(X=Xtrain, Y=Ytrain, nlambda=nlambda, lam_ratio=lam_ratio, ctasks=ctasks, opts=opts, C=C, lambda=lambda, intercept=intercept)
    
    #intercept model
    if (intercept){
      Xtest=lapply(Xtest, function(x)cbind(1, x))
    }
    
    yhatR=lapply(fit$ws, function(w)lapply(rtasks, function(x) Xtest[[x]]%*%w[,x]))
    mse_fold=rbind(mse_fold, sapply(yhatR, function(x)mean(sapply(1:length(rtasks), function(xx)mean((x[[xx]]-Ytest[[rtasks[xx]]])^2)))))
    
    yhatC=lapply(fit$ws, function(w)lapply(ctasks, function(x) {score=exp(Xtest[[x]]%*%w[,x]); return(score/(1+score))}))
    auc_fold=rbind(auc_fold, sapply(yhatC, function(x)mean(sapply(1:length(ctasks), 
      function(xx)as.numeric(pROC::auc(as.factor(Ytest[[ctasks[xx]]]), x[[xx]]))))))
    
    lam_seq=rbind(lam_seq, fit$lam_seq)
    
  }
  lambda.regress.min=colMeans(lam_seq)[order(colMeans(mse_fold))[1]]
  lambda.classify.min=colMeans(lam_seq)[order(colMeans(auc_fold), decreasing = T)[1]]
  lambda.weighted.min=length(rtasks)/ntasks*lambda.regress.min+length(ctasks)/ntasks*lambda.classify.min
    
  colnames(mse_fold)=paste0("Lam",1:ncol(mse_fold))
  colnames(auc_fold)=paste0("Lam",1:ncol(auc_fold))
  colnames(lam_seq)=paste0("Lam",1:ncol(mse_fold))
  
  cvResult$lam_seq=colMeans(lam_seq); cvResult$mse_fold=mse_fold; cvResult$auc_fold=auc_fold; cvResult$lam_seq_folds=lam_seq
  cvResult$lambda.regress.min=lambda.regress.min; cvResult$lambda.classify.min=lambda.classify.min; cvResult$lambda.weighted.min=lambda.weighted.min
  return(cvResult)
}













################################################################################
#' @title Plot the CV result
#'
#' @description Plot the CV result for both regression and clasification tasks
#' 
#' @param cvResult The result from calling the function \code{MTLComb_CV}
#' 
#' @export
#' @author Dr. Han Cao
################################################################################
plot_MTLComb_CV = function(cvResult){
  par(mfrow=c(1,2))
  plot(colMeans(cvResult$auc_fold), main="auc of classification tasks", xlab="lambda index", ylab="auc")
  plot(colMeans(cvResult$mse_fold), main="error of regression tasks", xlab="lambda index", ylab="mse")
  par(mfrow=c(1,1))
}














################################################################################
#' @title Prediction with MTLComb model
#'
#' @description All regression or classification models would be predicted in the single targeting dataset. The prediction scores 
#' of all models are returned
#' 
#' @param fit A MTLComb model
#' @param newx A new feature matrix
#' @param type True/False indicator to show whether the new dataset is regression or classification
#' @return The prediction scores of all tasks of \code{type} tasks
#' 
#' @export
#' @author Dr. Han Cao
################################################################################
predict_MTLComb=function(fit, newx, type){
  ntasks=ncol(fit$ws[[1]])
  rtasks=setdiff(1:ntasks, fit$ctasks)
  
  if (fit$intercept){
    newx=cbind(1, newx)
  }
  
  if(type=="regress"){
    yhat=lapply(fit$ws, function(x)newx%*%x[,rtasks])
    
  }else if(type=="classify"){
    yhat=lapply(fit$ws, function(x)1/(1+exp(-newx%*%x[,fit$ctasks])))
  }
  return(yhat)
}
