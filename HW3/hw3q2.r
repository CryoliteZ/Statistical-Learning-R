load(file="data/msong_slhw.rdata")
x_train = msong_train[1:5000,2:ncol(msong_train)]
y_train = msong_train$year


x_test = msong_test[,2:ncol(msong_test)]
y_test = msong_test$year
dim(x_test)

org_year_mean =  mean(y_train)
# shift to mean
y_train = y_train - mean(y_train)

# x_train <- scale(x_train) fast way to standarize
# standarize x_train
x_train_mean = t(apply(x_train, 2, function(x) mean(x, na.rm=TRUE)))                  
x_train_sd = apply(x_train, 2, function(x) sd(x, na.rm=TRUE))
x_train = sweep(x_train, 2, x_train_mean)
x_train = sweep(x_train, MARGIN=2, x_train_sd,`/`)
                   
#  standarize x_test
x_test = sweep(x_test, 2, x_train_mean)
x_test = sweep(x_test, MARGIN=2, x_train_sd,`/`)                   
y_test = y_test - org_year_mean


nfeat = ncol(x_train)
w = rep(0, nfeat)
lr = 0.0001
lambda = 0.01
w_lr = rep(0, nfeat)

epochs = 10

for(it in 1:epochs){
    w_grad = rep(0.0, nfeat)
    for(n in 1:nrow(x_train)){
       
       w_grad =  w_grad - ( y_train[n]-  drop(w %*% (x_train[n,])))* x_train[n,] + lambda * w
    }
    w = w - lr * w_grad
   
   
    
}