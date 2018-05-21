rf_carton <- function(dsall, folds, testfold, ypos = "pos",  chi_threshold = 0.1, grid_length = 20,  grid_type = "loglinear", rfntree = 500 ,debuglevel=0){
    
    mytest <-function (confusion){
        precision = (confusion[2,2])/(confusion[1,2] + confusion[2,2])
        recall = (confusion[2,2])/(confusion[2,1] + confusion[2,2])
        f1 = 2 * (precision * recall ) / (precision + recall)
        return(list(precision=precision, recall=recall, f1 = f1))
    }

    ycol=1
    #this is the training dataset
    dstrain_all = dsall[-folds[[testfold]],]
    dstrain_all_y = as.factor(dstrain_all[,ycol] == ypos)
    #testing dataset
    dstest = dsall[folds[[testfold]],]
    dstest_y = as.factor(dstest[,ycol] == ypos)
    if(testfold==1) {
     tunefold=10
    } else {1
     tunefold=testfold-1
    }
    #tuning dataset
    dstune = dsall[folds[[tunefold]],]
    #this is the sub-train dataset
    dstrain = dsall[-c(folds[[tunefold]], folds[[testfold]]),]
    dstrain_y = as.factor(dstrain[,ycol] == ypos)
    dstune_y = as.factor(dstune[,ycol] == ypos)

    features = filter_chisq(dstrain, ypos, chi_threshold = chi_threshold)
    dstrain_new = dstrain[features$colpos]
    dstune_new = dstune[features$colpos]
    ncol = dim(dstrain_new)[2]

    m_min = 2
    m_max = ncol
    grids = NULL
    if(grid_type == "loglinear"){
        grids = unique(round(exp(seq(log(m_min), log(m_max),length=grid_length))))
    }
    else if(grid_type == 'equal'){
        grids = unique(round(seq(m_min, m_max, length=grid_length )))
    }
    
    f1_all = c()
    best_mtry = 2
    best_f1 = -1
    best_test_result = NULL
    for(mtry in grids){
        result = randomForest(x = dstrain_new, y = dstrain_y, xtest=dstune_new,ytest= dstune_y, ntree = rfntree ,mtry = mtry)
        test_result = mytest(result$test$confusion) 
        if(test_result$f1  > best_f1){
            best_f1 = test_result$f1
            best_mtry = mtry
            best_test_result = test_result
        }
        f1_all = c(f1_all, test_result$f1)
    }
    new_features = filter_chisq(dstrain_all, ypos, chi_threshold = chi_threshold)
    dstrain_all = dstrain_all[new_features$colpos]
    dstest = dstest[new_features$colpos]
    final_rf_result = randomForest(x = dstrain_all, y = dstrain_all_y, xtest=dstest,ytest= dstest_y, ntree = rfntree ,mtry = best_mtry)
    result = list(mgrids= grids, f1_all=f1_all, best_m=best_mtry, test= mytest(final_rf_result$test$confusion), fselect=new_features)
    return(result)
}
