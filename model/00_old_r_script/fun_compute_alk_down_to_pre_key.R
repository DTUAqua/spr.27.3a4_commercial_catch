

#-----------------------------------------------------------------------------------------------------#
#--------------------------------------- F_COMPUTE_ALK -----------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_compute_ALK <- function(path_data, data_rca, data_rhh, tab_levels, tab_space, tab_time, space_first, time_first,
                          specie, ages, years, uncertainty, min_sfish, recruit_month, type_length, length_pred, length_obs, stratif,
                          return_graph, path_graph){
  
  # Attributing values according to table given
  
  # If we use scm we'll need to multiply by 2 later
  if(type_length == 'scm') {
    length_pred[3] <- 0.5
    length_pred[4] <- 2
  }
  
  # Length range when need to repeat length
  length_pred[5] <- (length_pred[2]-length_pred[1])*length_pred[4]+1
  
  # Difference ages
  ages[3] <- ages[2] - ages[1] + 1
  
  # Levels : first level and last level numbers
  levels <- c(tab_levels$level[1], tail(tab_levels$level,1)) 
  
  # Iitialisation
  vars_r <- list()
  
  # Needed later for aglg4 list :
  list_text <- list()
  
  # Creating variables that will include variables names and stratification variables names
  if(length(stratif) != 0){ #stratification
    tab_levels$vars <- tab_levels$strat1
    for(l in levels[1]:levels[2]){
      vars_r[[l]] <- tab_levels$strat1[l] #if stratification, we select the first stratification variable
      eval(parse(text=paste0('list_text[[l]] <- "aglg4$',tab_levels$strat1[[l]],'"')))
      if(length(stratif) > 1){  # if we have more than one stratification variable
        for(n in 2:length(stratif)){ #insert stratif variables
          if(eval(parse(text=paste0('tab_levels$strat',n,'[l] != ""')))){ # Only for levels with stratification
            eval(parse(text=paste0('tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels$strat',n,'[l], sep = ", ")'))) #new column in tab_levels
            eval(parse(text=paste0('vars_r[[l]] <- paste(c(vars_r[[l]], tab_levels$strat',n,'[l]))'))) #vector of names
            eval(parse(text=paste0('list_text[[l]] <- paste(c(list_text[[l]] , paste("aglg4$", tab_levels$strat',n,'[[l]], sep ="")))')))
          }
        }
        for(n in 1:tab_levels$nb_var[l]){ #insert level variables
          eval(parse(text=paste0('tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels$var',n,'[l], sep = ", ")'))) #new column in tab_levels
          eval(parse(text=paste0('vars_r[[l]] <- paste(c(vars_r[[l]], tab_levels$var',n,'[l]))'))) #vector of names
          eval(parse(text=paste0('list_text[[l]] <- paste(c(list_text[[l]] , paste("aglg4$", tab_levels$var',n,'[[l]], sep ="")))')))
        }
      }else{ # if only one stratification variable
        for(n in 1:tab_levels$nb_var[l]){
          eval(parse(text=paste0('tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels$var',n,'[l], sep = ", ")'))) #new column in tab_levels
          eval(parse(text=paste0('vars_r[[l]] <- paste(c(vars_r[[l]], tab_levels$var',n,'[l]))'))) #vector of names
          eval(parse(text=paste0('list_text[[l]] <- paste(c(list_text[[l]] , paste("aglg4$", tab_levels$var',n,'[[l]], sep ="")))')))
        }
      }
    }
    
    # Get rid of commas when the levels had no more stratification
    for(i in 1:levels[2]){if(substr(tab_levels$vars[i], 1, 1) == ","){
      tab_levels$vars[i] <- substring(tab_levels$vars[i],2)
      vars_r[[i]] <- vars_r[[i]][-1]
      list_text[[i]] <- list_text[[i]][-1]}}
  }else { #no stratification
    tab_levels$vars <- tab_levels$var1
    for(l in levels[1]:levels[2]){
      vars_r[[l]] <- tab_levels$var1[l] #no stratification : we select the first variable to initialize
      eval(parse(text=paste0('list_text[[l]] <- "aglg4$',tab_levels$var1[[l]],'"')))
      if(tab_levels$nb_var[l] != 1){ # Loop only when more than 1 variable
        for(n in 2:tab_levels$nb_var[l]){
          eval(parse(text=paste0('tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels$var',n,'[l], sep = ", ")'))) #new column in tab_levels
          eval(parse(text=paste0('vars_r[[l]] <- paste(c(vars_r[[l]], tab_levels$var',n,'[l]))'))) #vector of names
          eval(parse(text=paste0('list_text[[l]] <- paste(c(list_text[[l]] , paste("aglg4$", tab_levels$var',n,'[[l]], sep ="")))')))
        }
      }
    }
  }
  if (length(stratif) == 0){
    # Sandeel preparation data
    pre_key <- f_data_input_nostratif(path_data = path_data, 
                                      data_rca = data_rca, 
                                      data_rhh = data_rhh,
                                      tab_space = tab_space, 
                                      tab_time = tab_time, 
                                      time_first = time_first,
                                      space_first = space_first,
                                      specie = specie, 
                                      type_length = type_length, 
                                      ages = ages, 
                                      years = years, 
                                      recruit_month = recruit_month,
                                      length_pred = length_pred, 
                                      length_obs = length_obs)
  }else{
    pre_key <- f_data_input_stratif(path_data = path_data, 
                                    data_rca = data_rca, 
                                    data_rhh = data_rhh,
                                    tab_space = tab_space, 
                                    tab_time = tab_time, 
                                    time_first = time_first,
                                    space_first = space_first,
                                    specie = specie, 
                                    type_length = type_length, 
                                    ages = ages, 
                                    years = years, 
                                    recruit_month = recruit_month,
                                    length_pred = length_pred, 
                                    length_obs = length_obs,
                                    stratif = stratif)
  }
  
return(list(pre_key = pre_key, ages = ages, tab_levels = tab_levels, levels = levels, vars_r = vars_r, list_text = list_text, space_first = space_first, 
            uncertainty = uncertainty, min_sfish = min_sfish, length_pred = length_pred))
  
}
