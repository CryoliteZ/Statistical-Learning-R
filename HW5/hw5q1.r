filter_chisq <- function(dstrain, ypos="pos", min_count=5, chi_threshold=10**(-5)){
    pos_col = names(dstrain1)[1]
    new_cols = colSums(dstrain1[,2:dim(dstrain1)[2]] != 0)
    features = c()
    for(name in names(new_cols)){
       if(new_cols[name] > min_count){
            features <- c(features, name)
       }
    }
    new_df = dstrain1[c(pos_col,features)]
    N = nrow(new_df)
    chi_df<-data.frame(feat = character(), chisq_value = double())
    chi_sq_values = c()
    col_idx_array = c()
    for(feat in features){
        col_idx = grep(feat, colnames(new_df))
        col_idx_array = c(col_idx_array, grep(feat, colnames(dstrain1)))
        gs = c(0, 1)
        cs = c( "%notpositivetag%", ypos)
        present_or_not = 0
        on_topic_or_not = 0 
        chi_sq_sum = 0
        for(g in gs){
            for(c in cs){
                obsv_freq = 0
                if(g == 0){
                    present_or_not = nrow(new_df[new_df[, col_idx] == 0, ][feat])
                    if(c == ypos){
                       obsv_freq =  nrow(new_df[new_df[, 1] == ypos & new_df[, col_idx] == 0, ][feat])
                    }
                    else{
                       obsv_freq =  nrow(new_df[new_df[, 1] != ypos & new_df[, col_idx] == 0, ][feat])
                    }
                }
                else{
                    present_or_not = nrow(new_df[new_df[, col_idx] > 0, ][feat])
                    if(c == ypos){
                       obsv_freq =  nrow(new_df[new_df[, 1] == ypos & new_df[, col_idx] > 0, ][feat])
                    }
                    else{
                       obsv_freq =  nrow(new_df[new_df[, 1] != ypos & new_df[, col_idx] > 0, ][feat])

                    }
                }
                if(c == ypos){
                    on_topic_or_not =  nrow(new_df[new_df[, 1] == ypos, ][feat])
                }
                else{
                    on_topic_or_not =  nrow(new_df[new_df[, 1] !=ypos, ][feat])
                }
                E = present_or_not * on_topic_or_not / N
                chi_sq = (obsv_freq - E) **2 / E
                chi_sq_sum = chi_sq_sum + chi_sq
            }
        }
        chi_sq_values = c(chi_sq_values, chi_sq_sum )


    }
    chi_df <- data.frame(features, chi_sq_values, col_idx = col_idx_array)
    chi_df = chi_df[with(chi_df, order(-1 * chi_sq_values)), ]
    chi_df = chi_df[chi_df$chi_sq_values > chi_threshold,]
    result = list(colpos = chi_df$col_idx, colname = as.character((chi_df$features)),chistat = chi_df$chi_sq_values )
    if(nrow(chi_df) == 0){
        result = list(colpos = NULL, colname = NULL,chistat = NULL)
    }
    return(result)
}
