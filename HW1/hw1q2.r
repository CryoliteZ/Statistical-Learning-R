mle_update <- function(mu_prev, cov_prev, n_prev, xn){
    n = n_prev + 1
    mu = mu_prev + (xn - mu_prev)/n
    s = (cov_prev * (n-1) + ((n-1)/n) * (xn - mu_prev) %*%  t(xn - mu_prev)) / n
    return(list(n = n, mu = mu, s = s))
}
