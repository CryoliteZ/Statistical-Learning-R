entropy <- function(p) { 
    if(p == 1 || p  < 10 ** (-6)) return(0)
    return(-1 * p * log2(p) - (1-p) * log2(1-p)) 
}

filter_ig <- function(dstrain, ypos="pos", min_count=5, ig_threshold=10**(-5)){
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
    N_POS = nrow(new_df[new_df[, 1] == ypos, ][1])
    H_Y = entropy(N_POS / N)
    ig_values_array = c()
    col_idx_array = c()
    for(feat in features){
        col_idx = grep(feat, colnames(new_df))
        col_idx_array = c(col_idx_array, grep(feat, colnames(dstrain1)))
        pos_present = nrow(new_df[new_df[, 1] == ypos & new_df[, col_idx] > 0, ][feat])
        pos_absent = nrow(new_df[new_df[, 1] == ypos & new_df[, col_idx] == 0, ][feat])
        neg_present = nrow(new_df[new_df[, 1] != ypos & new_df[, col_idx] > 0, ][feat])
        neg_absent = nrow(new_df[new_df[, 1]  != ypos & new_df[, col_idx] == 0, ][feat])
        p_hasfeat = (pos_present + neg_present) /N
        p_nofeat = (pos_absent + neg_absent)/ N
        p_has_feat_pos = pos_present / (pos_present + neg_present)
        p_no_feat_pos = pos_absent / (pos_absent + neg_absent)
        H_YX = p_hasfeat * entropy(p_has_feat_pos)  + p_nofeat* entropy(p_no_feat_pos)
        ig = H_Y - H_YX
        ig_values_array = c(ig_values_array, ig)
    }
    ig_df = data.frame(fname = features, ig_value = ig_values_array, col_idx = col_idx_array)
    ig_df = ig_df[with(ig_df, order(-1 * ig_value)), ]
    ig_df = ig_df[ig_df$ig_value > ig_threshold,]
    result = list(colpos = ig_df$col_idx, colname = as.character((ig_df$fname)),igvalue = ig_df$ig_value)
    if(nrow(ig_df) == 0){
        result = list(colpos = NULL, colname = NULL,igvalue = NULL)
    }
    return(result)
} 
