


################################################################################
#generate classification tasks
################################################################################
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

################################################################################
#generate regression tasks
################################################################################
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
ctasks <- 11:20