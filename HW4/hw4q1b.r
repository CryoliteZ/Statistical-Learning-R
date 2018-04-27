pgm_predict <- function(model, testds1_feature){
    if(ncol(testds1_feature) != 3){
        return(NULL)
    }
    vec = c()
    for(row_idx in 1:nrow(testds1_feature)){
        x = testds1_feature[row_idx,]
        x = as.matrix(x)
        p_max = -1
        target_idx = 1
        for(i in 1:length(model)){
            # model
            m = model[[i]]

            # mu, sigma
            sigma = m$sigma1
            mu = m$mu1
            sigma_inv = solve(sigma)

            p = 1
            p = det(sigma)** (-1/2) 
            v = exp((-1/2) * ((x - mu ) %*% sigma_inv  %*% t(x - mu)))
            p = p * v
            if(p > p_max){
                p_max = p
                target_idx = i
            }
            # library compute answer (cheating)
            # answer <- dmvnorm(x, m$mu1, m$sigma1)
            # print(answer)
        }
        vec[row_idx] = target_idx
    }
    return(vec)
}
