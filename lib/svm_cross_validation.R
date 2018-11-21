cv.function <- function(X.train, y.train, par, K){
  n_tokens <- length(y.train)
  n.fold <- floor(n_tokens/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n_tokens-(K-1)*n.fold)))  
  cv.f1 <- rep(NA, K)
  
  for (k in 1:K){
    train.data <- X.train[s != i, ]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i, ]
    test.label <- y.train[s == i]
    
    fit <- mcSVM(train.data, train.label, gamma = par$gamma, lambda = par$lambda, KERNEL = "GAUSS_RBF")
    pred <- predict(fit, test.data)
    p <- sum(pred == "0" & test.label == "0")/sum(pred == "0")
    r <- sum(pred == "0" & test.label == "0")/sum(test.label == "0")
    cv.f1[k] <- 2*p*r/(p+r)  
    
  }			
  return(mean(cv.f1))
}