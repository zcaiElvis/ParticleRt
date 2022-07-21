source("functions/get_true_rt.R")

one_day_pred <- function(rt, y, shape, scale){
  lambda <- get_lambda(y, shape, scale)
  y_pred <- rep(0, length(y))
  
  for(k in 2:length(y_pred)){
    y_pred[k] <- lambda[k]*rt[k]
  }
  return(y_pred)
}