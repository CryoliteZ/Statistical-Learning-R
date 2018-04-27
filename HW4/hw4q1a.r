pgm_train <- function(outclass, train_data){
    result = list()
    for(class in outclass) {
        train = train_data[[class]] 
        mu1 = colMeans(train)
        sigma1 = cov(train, train)
        prec1 = solve(sigma1)
        detsig_log = log(det(sigma1))
        N1 =  nrow(train)
        l = list(mu1=mu1, sigma1=sigma1, prec1=prec1, detsig_log=detsig_log, N1=N1)
        result[[class]] = l
    }
    return(result)
}
