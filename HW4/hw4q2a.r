sigmoid <- function(x){
    return((exp(x) / (exp(x) + 1)))
}

logicreg_l2_train <- function(tall, xmat, debuglevel=0){
    lambda_rate =  0.0005
    param_tol = 10 ** (-5)
    granditertol = 2
    outitermax = 50
    inneritermax = 20
    N = nrow(tall)
    M = dim(xmat)[2]
    I = diag(M)
    # compute init lambda
    lambda = lambda_rate * N
    # compute init w
    w = solve(lambda * I + t(xmat) %*% (xmat) ) %*% t(xmat) %*% tall
    R = NULL
    for(outer_loop in 1:outitermax){
        w_count = 0
        for(inner_loop in 1:inneritermax){
            y = sigmoid(  t(w) %*% t(xmat))
            y = t(y)
            R = y %*% t(1-y) * diag(nrow(y)) 

            E = lambda * w + t(xmat) %*% ( y - tall)
            H = t(xmat) %*% R %*% xmat + lambda * I
            w_new = w - solve(H) %*% E
            w_diff = w_new - w
            mean_diff = mad(w_diff,center= mean(w_diff))
            w = w_new
            w_count = w_count + 1
            if(mean_diff < param_tol){
                # converge
                # print(w_new)
                break
            }
            
        }
        
        
        for(inner_loop in 1:inneritermax){
             ev = eigen(t(xmat) %*% R %*%  xmat)
            ev_values = ev$values
            gamma = 0
            for(i in 1:length(ev_values)){
                gamma  = gamma + (ev_values[i] / (ev_values[i] + lambda) )
            }
            lambda_new = gamma / (t(w) %*% w)
            lambda_diff = lambda_new - lambda
            lambda = drop(lambda_new)
            if(abs(lambda_diff) < param_tol){
                # converge
                # print(lambda)
                break
            }
        }
        
        if(w_count <= granditertol){
            break;
        }
        
    }
    y = sigmoid(  t(w) %*% t(xmat))
    y = t(y)
    R = y %*% t(1-y) * diag(nrow(y)) 
    Sn = solve(lambda * I + t(xmat) %*% R %*%  xmat)
    Sn = sqrt(diag(Sn))
    result = list(w = w, w_sd = Sn, lambda = lambda, M = M, N = N)
    return(result)
}

