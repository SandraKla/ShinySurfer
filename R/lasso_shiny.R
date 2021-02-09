#' Lasso Regression von Prof. Klawonn
#' 
#' @param dat
#' @param target.column
#' @param lambda_seq
#' @param alpha
#' @param normalise
lasso_training_results <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),
                                   alpha=1,normalise=T){
  taco <- target.column
  if (!is.numeric(taco)){
    taco <- grep(target.column,colnames(dat))
  }
  x_vars <- as.matrix(dat[,-taco])
  y_var <- dat[[taco]]
  y.mean <- 0
  y.sd <- 1
  if (normalise){
    x_vars <- scale(x_vars)
    y.mean <- mean(y_var,na.rm=T)
    y.sd <- sd(y_var,na.rm=T)
    y_var <- (y_var - y.mean)/y.sd
  }
  
  cv_output <- cv.glmnet(x_vars,y_var,alpha=alpha,lambda=lambda_seq)
  best_lam <- cv_output$lambda.min
  lasso_best <- glmnet(x_vars,y_var,alpha=alpha,lambda=best_lam)
  predictions <- predict(lasso_best,s=best_lam,newx=x_vars)
  if (normalise){
    predictions <- y.mean + y.sd*predictions
  }
  return(list(lambda=best_lam,coefficients=coef(lasso_best)[,1],predictions=predictions))
}

#' Lasso Regression von Prof. Klawonn
#' 
#' @param dat
#' @param target.column
#' @param lambda_seq
#' @param alpha
#' @param normalise
#' @param n.bootstrap
lasso_bootstrap <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),
                            alpha=1,normalise=T,n.bootstrap=10){
  
  lambda <- rep(0,n.bootstrap)
  coefficient.matrix <- matrix(0,nrow=n.bootstrap,ncol=ncol(dat)+1)
  colnames(coefficient.matrix) <- c("lambda","intercept",colnames(dat)[!names(dat) %in% target.column,drop=F])
  withProgress(message = 'Calculation in progress',detail = 'Wait a moment',value = 0,expr = {
    for (i in 1:n.bootstrap){
      inds <- sample(nrow(dat),nrow(dat),replace=T)
      lassi <- lasso_training_results(dat[inds,],target.column,lambda_seq=lambda_seq,alpha=alpha,normalise=normalise)
      coefficient.matrix[i,] <- c(lassi$lambda,lassi$coefficients)
      incProgress(1/n.bootstrap)
    }})
  return(coefficient.matrix)
}

#' 
#' 
#' @param x
prop.nonzero <- function(x){
  return(sum(!is.na(x) & x!=0)/sum(!is.na(x)))
}

#' 
#' 
#' @param x
get.proportion.of.nonzero.coeffcients <- function(coefficient.matrix){
  return(apply(coefficient.matrix,2,prop.nonzero))
}

#' 
#' 
#' @param x
sign.consistency <- function(x){
  return(sum(!is.na(x) & x>=0)==sum(!is.na(x)) | sum(!is.na(x) & x<=0)==sum(!is.na(x)))
}

#' 
#' 
#' @param x
get.sign.consistency <- function(coefficient.matrix){
  return(apply(coefficient.matrix,2,sign.consistency))
}