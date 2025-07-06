

# ALK by level
list_lvl <- list()
for(l in levels[1]:levels[2]){
  eval(parse(text=paste0('list_lvl$lv',l,'<- f_levels_ALK(pre_key = aglg5a_4,
                           lvl = ',l,', 
                           ages = ages, 
                           tab_levels = tab_levels, 
                           vars_r = vars_r, 
                           list_text = list_text, 
                           levels = levels, 
                           space_first = space_first,
                           uncertainty = uncertainty,
                           min_sfish = min_sfish)')))
}



if (length(stratif) == 0){
  ALK <- f_ALK_nostrat(ages = ages, 
                       levels = levels, 
                       years = years, 
                       list_lvl =list_lvl, 
                       specie = specie, 
                       length_pred = length_pred, 
                       vars_r = vars_r,
                       tab_levels = tab_levels, 
                       tab_space = tab_space, 
                       tab_time = tab_time, 
                       space_first= space_first, 
                       time_first = time_first, 
                       aglg5 = aglg5)
  
  if(return_graph == "Yes"){
    f_graph_nostrat(path_graph = path_graph, 
                    data_graph = ALK, 
                    years = years, 
                    ages = ages, 
                    tab_time = tab_time, 
                    tab_levels = tab_levels,
                    time_first = time_first)
  }
}else{
  ALK <- f_ALK_strat(ages = ages, 
                     levels = levels, 
                     years = years, 
                     list_lvl =list_lvl, 
                     specie = specie, 
                     length_pred = length_pred, 
                     vars_r = vars_r,
                     tab_levels = tab_levels, 
                     tab_space = tab_space, 
                     tab_time = tab_time, 
                     stratif = stratif, 
                     space_first= space_first, 
                     time_first = time_first, 
                     aglg5 = aglg5)
  
  
  if(return_graph == "Yes"){
    f_graph_strat(path_graph = path_graph, 
                  data_graph = ALK, 
                  years = years, 
                  ages = ages, 
                  tab_time = tab_time, 
                  tab_levels = tab_levels, 
                  time_first = time_first)
  }
  
}



return(ALK)