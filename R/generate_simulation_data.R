
#generate classification tasks
t <- 10
p <- 200
n <- 100
W <- matrix(data=rnorm(t*p),ncol=t, nrow = p)
W[1:p*0.9,] <- 0
X <- list(); Y <- list(); tX <- list(); tY <- list()
for(i in 1:t){
  X[[i]] <- matrix(data=rnorm(n*p),ncol=p, nrow = n)
  Y[[i]] <- sign(X[[i]] %*% W[,i] + 0.5 * rnorm(n))
  tX[[i]] <- matrix(rnorm(p*n),nrow=n)
  tY[[i]] <- sign(tX[[i]] %*% W[, i] + rnorm(n) * 0.5)
}
cX <- X; cY <- Y; cW <- W; ctX <- tX; ctY <- tY;

#generate regression tasks
t <- 10
p <- 200
n <- 100
W <- matrix(data=rnorm(t*p),ncol=t, nrow = p)
W[1:p*0.9,] <- 0
X <- list(); Y <- list(); tX <- list(); tY <- list()
for(i in 1:t){
  X[[i]] <- matrix(data=rnorm(n*p),ncol=p, nrow = n)
  Y[[i]] <- X[[i]] %*% W[,i] + 0.5 * rnorm(n)
  tX[[i]] <- matrix(rnorm(p*n),nrow=n)
  tY[[i]] <- tX[[i]] %*% W[, i] + rnorm(n) * 0.5
}

X <- c(X,cX); Y <- c(Y,cY); W <- cbind(W,cW);
tX <- c(tX, ctX); tY <- c(tY, ctY)
ctasks <- 11:20

save(file='simulated_data.rda', X,Y,tX,tY,W,ctasks)
