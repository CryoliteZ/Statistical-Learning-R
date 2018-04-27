sigmoid <- function(x){
    return((exp(x) / (exp(x) + 1)))
}

logicreg_l2_predict <- function(model1, xmattest1){
    w = (model1)$w
    prob = sigmoid( t(w) %*% t(xmattest1))
    class = apply(prob, 1, function(x) x > 0.5)
    class = as.integer(as.logical(class))
    prob = t(prob)
    result = list(prob = prob, class = class)
    return(result)
}
