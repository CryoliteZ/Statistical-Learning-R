gen_utagmat <- function(data_user_tags, data_paying_price){
    reg_tvalue = function(y, x) {
        if(length(y) != length(x)) {
            stop("Inconsistent length of y and x")
        }
        y=matrix(y, ncol=1)
        xmat=matrix(1, ncol=2, nrow=length(y))
        xmat[,2] = x    
        bhead = solve(t(xmat)%*%xmat, t(xmat)%*%y)
        yhead = xmat %*% bhead
        e1 = y - yhead
        var1 = sum(e1 * e1) / (length(e1)-2)    
        sigma2 = solve(t(xmat)%*%xmat) * var1
        t1=bhead[2]/sqrt(sigma2[2,2])    
        return(t1)
    }

    myfunction <- function(tag_list_string, tag){
        user_tags_split <- strsplit(data_user_tags, split = ",")
        has_tag = matrix(rep(0, length(tag_list_string)), ncol=1)
        idx = 1
#         print((has_tag))
        for(ss in user_tags_split){
            for(t in ss){
                if(t == tag && !is.na(t)&& !is.na(tag)){
                    has_tag[idx] = 1
                    break
                }
            }
            idx = idx + 1
        }
        return(has_tag) 
#         return(grepl(tag, tag_list_string))
    }

    user_tags_split <- strsplit(data_user_tags, split = ",")
    uesr_tags_count = c()
    for(ss in user_tags_split){
        for(t in ss){
            if(t  %in% names(uesr_tags_count)){
                uesr_tags_count[t] =  uesr_tags_count[t] + 1
            }
            else{
                uesr_tags_count[t] = 1
            }
        }
    }


    y = data_paying_price
    t_value_list = rep(NA, length(uesr_tags_count))
    for(user_tag in names(uesr_tags_count)){
        if(uesr_tags_count[user_tag] >= 5 && !is.na(uesr_tags_count[user_tag])){
            x = as.numeric(myfunction(data_user_tags, user_tag))
            t_value_list[user_tag] = reg_tvalue(y,x)
        }
    }


#     uesr_tags_count = sort(uesr_tags_count, decreasing = TRUE)
    t_value_list = sort(abs(t_value_list), decreasing = TRUE)
    t_value_list_idx = t_value_list >= 1
    t_value_list = t_value_list[t_value_list_idx]
#     names(t_value_list)

    new_df = data.frame(constant = rep(1, length(data_user_tags)))
    nextcol = ncol(new_df)+1
    tnames = names(t_value_list)
    orgfeature = data_user_tags
    newfeatname = tnames
    for(user_tag in tnames){
        new_df[[nextcol]] = as.numeric(myfunction(orgfeature, user_tag))
        names(new_df)[nextcol] = paste("user", user_tag, sep="_")
        newfeatname[nextcol] = paste("user", user_tag, sep="_")
        nextcol= nextcol+1
    }
#     head(new_df) 
    new_df = data.matrix(new_df)
#     y = data_paying_price
#     w = solve(t(new_df) %*% new_df, t(new_df) %*% y)
    return(new_df)
}
