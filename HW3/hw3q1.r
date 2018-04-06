lm_evmax <- function(y, xmat){
    N = nrow(y)

    lambda = 0.001 * N
    M = dim(xmat)[2]
    I = diag(M)

    w = solve(lambda * I + t(xmat) %*% xmat) %*% t(xmat) %*% y
    e0 = y - xmat %*% w
    beta = drop(N / (t(e0) %*% e0))
    alpha = drop(lambda %*% beta)
    A = alpha * I + beta * t(xmat) %*% xmat
    mN = beta * solve(A) %*% t(xmat) %*% y
    lambda = eigen(beta * t(xmat) %*% xmat)$values
    
    # compute new alpha, beta, gamma
    gamma = 0
    for(i in 1:M){
        gamma = gamma + (lambda[i] / (alpha + lambda[i]))
    }
    alpha_new = drop(gamma / (t(mN) %*% mN))
    e1 = y - xmat %*% mN
    beta_new = drop((N - gamma) / (t(e1) %*% e1))

    # iterate
    while(abs(alpha - alpha_new) + abs(beta - beta_new) >= 10 ** (-5)){
        alpha = alpha_new
        beta = beta_new
        A = alpha * I + beta * t(xmat) %*% xmat
        mN = beta * solve(A) %*% t(xmat) %*% y
        lambda = eigen(beta * t(xmat) %*% xmat)$values
        gamma = 0
        for(i in 1:M){
            gamma = gamma + (lambda[i] / (alpha + lambda[i]))
        }
        alpha_new = drop(gamma / (t(mN) %*% (mN)))
        e1 = y - xmat %*% mN
        beta_new = drop((N - gamma) / (t(e1) %*% e1))

    }
    result = list(mN = mN, mNsd = diag(sqrt(solve(A))), alpha = alpha, beta = beta)
    return(result)
}
