gen_uagentmat <- function(user_agents, data_paying_price){
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

    myfunction <- function(agents_result, agent_name){
        has_tag = matrix(rep(0, data_len), ncol=1)
        idx = 1
        for(ags in agents_result){
            for(ag in ags){
                if(ag == agent_name && !is.na(agent_name)&& !is.na(ag)){
                    has_tag[idx] = 1
                    break
                }
            }
            idx = idx + 1
        }
        return(has_tag) 
    }
    
    data_len = length(data_paying_price)
    pattern <- "([A-Za-z][A-Za-z0-9]{1,})"
    agents_result = regmatches(user_agents, gregexpr(pattern, user_agents))
    agents_result =lapply(agents_result, unique)
    user_agents_count = c()
    for(ags in agents_result){
        for(ag in ags){
            if(ag  %in% names(user_agents_count)){
                user_agents_count[ag] =  user_agents_count[ag] + 1
            }
            else{
                user_agents_count[ag] = 1
            }
        }
    }
    doc_freq_upperbound = floor(0.5 * data_len)
    doc_freq_lowerbound = 10
    t_value_list = rep(NA, length(user_agents_count))
#     print(agents_result)
    for(user_agent in names(user_agents_count)){
        if(!is.na(user_agents_count[user_agent]) && user_agents_count[user_agent] >= doc_freq_lowerbound && user_agents_count[user_agent] <=  doc_freq_upperbound){
            x = as.numeric(myfunction(agents_result, user_agent))
            t_value_list[user_agent] = reg_tvalue(data_paying_price,x)
        }
    }
    reverse_alpha_idx = sort(names(t_value_list), decreasing = TRUE)
    t_value_list = t_value_list[reverse_alpha_idx]
    t_value_list = sort(abs(t_value_list), decreasing = TRUE)
    t_value_list_idx = t_value_list >= 1
    t_value_list = t_value_list[t_value_list_idx]
  
 
    new_df = data.frame(constant = rep(1, data_len))
    nextcol = ncol(new_df)+1
    tnames = names(t_value_list)
    for(user_agent in tnames){
        new_df[[nextcol]] = as.numeric(myfunction(agents_result, user_agent))
        names(new_df)[nextcol] = paste("agent", user_agent, sep="_")
        nextcol= nextcol+1
    }
    new_df = data.matrix(new_df)
    return(new_df)
}
