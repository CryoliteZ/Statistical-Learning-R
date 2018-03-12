mle_update <- function(mu_prev, cov_prev, n_prev, xn){
    n = n_prev + 1
    mu = mu_prev + (xn - mu_prev)/n
    s = (cov_prev  + (1/n) * ((xn - mu_prev) %*%  t(xn - mu_prev))) * (n_prev/n)
    ret = list(mu = mu, s = s, n = n)
    return(ret)
}
