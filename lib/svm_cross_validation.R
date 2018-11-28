cv.function <- function(X.train, y.train, par, K){
  n_tokens <- length(y.train)
  n.fold <- floor(n_tokens/K)
  set.seed(1)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n_tokens-(K-1)*n.fold)))  
  cv.f1 <- rep(NA, K)
  
  for (k in 1:K){
    train.data <- X.train[s != k, ]
    train.label <- y.train[s != k]
    test.data <- X.train[s == k, ]
    test.label <- y.train[s == k]
    
    fit <- ksvm(train.data, train.label, kernel = "rbfdot", type = "C-svc", kpar = list(sigma = par$sigma))
    pred <- predict(fit, test.data)
    pred <- ifelse(pred==2,TRUE,FALSE)
    p <- sum(!pred & !test.label)/sum(!pred)
    r <- sum(!pred & !test.label)/sum(!test.label)
    cv.f1[k] <- 2*p*r/(p+r)
    
  }
  return(mean(cv.f1))
}

perform_cv <- function(input, labels){
  set.seed(1)
  K<-5
  cv_result <- rep(NA, 3)
  sigmas <- c(0.01, 0.1, 1)
  cv_time <- matrix(NA, ncol = 3, nrow = 3)
  for(i in 1:length(sigmas)){
    pars = list("sigma" = sigmas[i])
    cv_result[i] <- cv.function(input, labels,pars,K)
  }
  best_sigma <- sigmas[which.max(cv_result)]
  return(list("best_sigma" = best_sigma, "cv_result" = cv_result))
}
# 
# cv.function <- function(X.train, y.train, par, K){
#   n_tokens <- length(y.train)
#   n.fold <- floor(n_tokens/K)
#   set.seed(1)
#   s <- sample(rep(1:K, c(rep(n.fold, K-1), n_tokens-(K-1)*n.fold)))  
#   cv.f1 <- rep(NA, K)
#   
#   for (k in 1:K){
#     train.data <- X.train[s != k, ]
#     train.label <- y.train[s != k]
#     test.data <- X.train[s == k, ]
#     test.label <- y.train[s == k]
#     
#     fit <- mcSVM(train.data, train.label, gamma = par$gamma, lambda = par$lambda, KERNEL = "GAUSS_RBF")
#     pred <- as.logical(predict(fit, test.data))
#     p <- sum(!pred & !test.label)/sum(!pred)
#     r <- sum(!pred & !test.label)/sum(!test.label)
#     
#     cv.f1[k] <- 2*p*r/(p+r)  
#     
#   }
#   print(cv.f1)
#   return(mean(cv.f1))
# }