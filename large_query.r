df_variableIDs <- get_variableIDs()
variables_list = as.list(df_variableIDs$variableId)
variables_list<-split(variables_list, ceiling(seq_along(variables_list)/2000))
years_list = list(62)
parties_list = list(36)
query_results <- flex_query(parties_list,years_list,variables_list[[1]])
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[2]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[3]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[4]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[5]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[6]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[7]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[8]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[9]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[10]]))
query_results <- rbind(query_results,flex_query(parties_list,years_list,variables_list[[11]]))



#If, for some reason, you want to grab all the years, then try this:
#All years:
df_yearIDs <- get_years()
years_list = as.list(df_yearIDs$yearId)
