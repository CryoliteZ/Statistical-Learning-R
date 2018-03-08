gpredict <- function(df_train, df_test){
    
    rep.row <- function(x,n){
       matrix(rep(x,each = n),nrow = n)
    }
    
    myColMeans<-function(i,x){
        mean(x[,i])
    }
    
    xa = data.matrix(df_train[,1])
    xb = data.matrix(df_train[,2:ncol(df_train)])
     
    mua = mean(xa)
#     mub = colMeans(xb)
    mub = sapply(1:ncol(xb),myColMeans,xb)
  
    mub_matrix = rep.row(mub, nrow(df_test))
    
    cov_ab = cov(xa, xb)
    cov_bb = cov(xb, xb)
    cov_bb_inv = solve(cov_bb)
    if(is.null(df_test)){
        x_pred = NULL
    }
    else{
        # convert df1_test to matrix
        df_test = data.matrix(df_test)
        x_pred = mua + (cov_ab %*% cov_bb_inv) %*% t(df_test - mub_matrix)
    }
    result = list(mua = mua, 
                    mub = mub,
                    s_ab = cov_ab,
                    s_bb = cov_bb,
                    predict = x_pred)
    return(result)
    
}
