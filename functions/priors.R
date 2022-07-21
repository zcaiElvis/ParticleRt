library("pomp")



rprior_lnormal_sd <- function(sigma1, sigma2, sdlog, x0, g_shape, g_scale){
  c(
    sigma1 = sigma1,
    sigma2 = sigma2,
    sdlog = runif(1, 0.01, 0.1),
    x0 = x0,
    g_shape = g_shape,
    g_scale = g_scale
  )
}

dprior_lnormal_sd <- function(sdlog, log){
  dunif(sdlog, 0.01, 0.1, log = log)
}


