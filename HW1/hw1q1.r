gpredict = function(df_train, df_test = NULL){
    
    rep.row <- function(x,n){
       matrix(rep(x,each = n),nrow = n)
    }
    
    myColMeans<-function(i,x){
        mean(x[,i])
    }
    
    if(!is.null(df_test) && ncol(df_train)-1 != ncol(df_test)){
        return(NULL)
    }
    xa = data.matrix(df_train[,1])
    xb = data.matrix(df_train[,2:ncol(df_train)])
     
    mua = mean(xa)
    mub = colMeans(xb)
    cov_all =  cov(df_train, df_train)
    n_features = ncol(cov_all)
    cov_ab = cov_all[2:n_features,1]
    cov_bb = cov_all[2:n_features,2:n_features]
    cov_bb_inv = solve(cov_bb)
    if(is.null(df_test)){
        x_pred = NULL
    }
    else{
        # convert df_test to matrix
        df_test = data.matrix(df_test)
        ntest = nrow(df_test)
        x_pred = rep(0, ntest)
        for (i in 1:ntest){
            x_pred[i] = mua + (cov_ab %*% cov_bb_inv) %*% (df_test[i,] - mub)
        } 
        #  x_pred = mua + (cov_ab %*% cov_bb_inv) %*% t(df_test - mub_matrix)
    }
    result = list(mua = mua, 
                    mub = mub,
                    s_ab = cov_ab,
                    s_bb = cov_bb,
                    predict = x_pred)
    return(result)
    
}
