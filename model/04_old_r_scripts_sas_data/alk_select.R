select_alk <- function(ages, levels, years, areas, list_lvl, specie, length_pred, vars_r, tab_levels, tab_space, tab_time, space_first, time_first,
                          aglg5){
  
  # List to stock ALK of each year
  ALKs <- list()
  
  # Get list each level for each year
  lvls_yrs <- list()
  
  for (l in levels[1]: levels[2]){
    eval(parse(text=paste0('lvls_yrs$lv',l,'_l <- split(list_lvl$lv',l,', list_lvl$lv',l,'$year)'))) 
  }
  
  # Getting one line per rect
  tab_space <- subset(tab_space, rect != "" & is.na(rect) == F)
  if (space_first != "rect"){ # Case where rect is not the smallest area taken into account
    eval(parse(text=paste0('tab_space <- tab_space[!(is.na(tab_space$', space_first, ') == T & tab_space$', space_first, '%in% c("999", "99", "")),]')))
  }
  
  # Get one line for each minimal time interval (if not by month)
  if(!(time_first == "month") ){
    eval(parse(text=paste0('tab_time2 <- distinct(tab_time, ', paste(names(tab_time)[-1], collapse = ", "), ')')))
    eval(parse(text=paste0('tab_time2 <- subset(tab_time2,!(is.na(', time_first, ') == T | ', time_first, '%in% c("", "999", "99")))')))
  }else {tab_time2 <- tab_time}
  
  ## Loop compute ALK for each year
  for(yr in (years[1]: years[2])){
    
    combs <- tab_space
    eval(parse(text=paste0('combs <- combs[rep(1:nrow(combs), each = length(na.omit(tab_time2$', time_first, '))*(ages[2]-ages[1])*length_pred[5]),] '))) # (month) * (ages-1) * (length range)
    combs$year <- yr
    eval(parse(text=paste0('combs$',time_first,' <- rep(na.omit(tab_time2$', time_first , '), each = (ages[2]-ages[1])*length_pred[5])')))
    combs$age <- rep(ages[1]:(ages[2]-1), each = length_pred[5])
    combs$length <- rep(seq(length_pred[1],length_pred[2],length_pred[3]), nrow(combs)/length_pred[5])    
    
    eval(parse(text=paste0('combs <- merge(combs, tab_time2, by = "',time_first,'", all.x = T)')))
    
    #### Combination all levels in one dataset (for each year)
    
    for(l in levels[1]:levels[2]){
      if(l == levels[1]){
        eval(parse(text=paste0('comb',l, ' <- merge(combs, lvls_yrs$lv',l,'_l$`', yr,'`, by = c(vars_r[[l]],"age", "length"), all = TRUE)')))
        eval(parse(text=paste0('comb',l, ' $level <-', l)))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$maxlgth) == FALSE & comb',l, '$maxlgth < 18 & comb',l, '$length > comb',l, '$maxlgth + 2 & comb',l, '$age != 0] <- 0')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$maxlgth) == FALSE & comb',l, '$maxlgth == 19 & comb',l, '$length > comb',l, '$maxlgth + 3] <- 0')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$maxlgth) == FALSE & comb',l, '$maxlgth > 19 & comb',l, '$length  > comb',l, '$maxlgth + 4] <- 0')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$minlgth) == FALSE & comb',l, '$minlgth < 20 & comb',l, '$length  < comb',l, '$minlgth - 2] <- 1')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$minlgth) == FALSE & comb',l, '$minlgth == 22 & comb',l, '$length  < comb',l, '$minlgth - 3] <- 1')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$minlgth) == FALSE & comb',l, '$minlgth >= 24 & comb',l, '$length < comb',l, '$minlgth - 4] <- 1')))
        eval(parse(text=paste0('comb',l, '$level[is.na(comb',l, '$pred) == TRUE] <- NA')))
        eval(parse(text=paste0('comb',l, '$pi <- comb',l, '$pred')))
        eval(parse(text=paste0('comb',l, '$variance <- comb',l, '$prange')))
        eval(parse(text=paste0('comb',l, '$s_fish <- comb',l, '$sums')))
        eval(parse(text=paste0('comb',l, ' <- subset(comb',l, ', select = -c(minlgth, maxlgth, prange, pred, minp, maxp, sums, n, s))')))
      }else {
        eval(parse(text=paste0('comb',l, ' <- merge(comb',l-1, ', lvls_yrs$lv',l,'_l$`',yr,'`,  by = c(vars_r[[l]],"age", "length"), all = TRUE)')))
        eval(parse(text=paste0('comb',l, '$level[is.na(comb',l, '$pi) == TRUE & is.na(comb',l, '$level) == TRUE] <- ',l)))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$maxlgth) == FALSE & comb',l, '$maxlgth < 18 & comb',l, '$length > comb',l, '$maxlgth + 2] <- 0')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$maxlgth) == FALSE & comb',l, '$maxlgth == 19 & comb',l, '$length > comb',l, '$maxlgth + 3] <- 0')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$maxlgth) == FALSE & comb',l, '$maxlgth > 19 & comb',l, '$length  > comb',l, '$maxlgth + 4] <- 0')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$minlgth) == FALSE & comb',l, '$minlgth < 20 & comb',l, '$length  < comb',l, '$minlgth - 2] <- 1')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$minlgth) == FALSE & comb',l, '$minlgth == 22 & comb',l, '$length  < comb',l, '$minlgth - 3] <- 1')))
        eval(parse(text=paste0('comb',l, '$pred[is.na(comb',l, '$pred) == FALSE & is.na(comb',l, '$minlgth) == FALSE & comb',l, '$minlgth >= 24 & comb',l, '$length < comb',l, '$minlgth - 4] <- 1')))
        eval(parse(text=paste0('comb',l, '$pi[is.na(comb',l, '$pi) == TRUE] <- comb',l, '$pred[is.na(comb',l, '$pi) == TRUE]')))
        eval(parse(text=paste0('comb',l, '$level[is.na(comb',l, '$pi) == TRUE] <- NA')))
        eval(parse(text=paste0('comb',l, '$variance[is.na(comb',l, '$level) == F & comb',l, '$level == l] <- comb',l, '$prange[is.na(comb',l, '$level) == F & comb',l, '$level == l]')))
        eval(parse(text=paste0('comb',l, '$s_fish[is.na(comb',l, '$level) == F & comb',l, '$level == l] <- comb',l, '$sums[is.na(comb',l, '$level) == F & comb',l, '$level == l]')))
        eval(parse(text=paste0('comb',l, '<- subset(comb',l, ', select = -c(minlgth, maxlgth, prange, pred, minp, maxp, sums, n, s))')))
        eval(parse(text=paste0('rm(comb',l-1, ')')))
      }
    }
    eval(parse(text=paste0('l11 <- comb',levels[2]))) 
    l11 <- subset(l11, is.na(rect) == F)
    
    # Initialization vectors for sql
    text_pi <- c();text_l <- c();text_var <- c();text_s_fish <- c()
    
    # Creation variables pi,level, variance and n_fish for each age
    for (i in ages[1]: (ages[2]-1)){
      eval(parse(text=paste0('l11$pi',i,'[l11$age == ',i,'] <- l11$pi[l11$age == ',i,']')))
      eval(parse(text=paste0('l11$l',i,'[l11$age == ',i,'] <- l11$level[l11$age == ',i,']')))
      eval(parse(text=paste0('l11$var',i,'[l11$age == ',i,'] <- l11$variance[l11$age == ',i,']')))
      eval(parse(text=paste0('l11$s_fish',i,'[l11$age == ',i,'] <- l11$s_fish[l11$age == ',i,']')))
      
      # Text for max calculation
      eval(parse(text=paste0('text_pi <- c(text_pi, "pi',i,'")'))) #vector pi0, pi1, pi2, pi3
      eval(parse(text=paste0('text_l <- c(text_l, "l',i,'")'))) #vector l0, l1, l2, ...
      eval(parse(text=paste0('text_var <- c(text_var, "var',i,'")'))) #vector l0, l1, l2, ...
      eval(parse(text=paste0('text_s_fish <- c(text_s_fish, "s_fish',i,'")'))) #vector l0, l1, l2, ...
    }
    text_max <- paste(paste(text_pi, collapse = ", "), paste(text_l, collapse = ", "), 
                      paste(text_var, collapse = ", "), paste(text_s_fish, collapse = ", "),sep = ", ")
    
    # Replacing NA by '0' for the group by function
    for (a in ages[1]: (ages[2]-1)){
      eval(parse(text=paste0('l11$pi', a, '[is.na(l11$pi', a, ') == T] <- 0')))
      eval(parse(text=paste0('l11$l', a, '[is.na(l11$l', a, ') == T] <- 0')))
      eval(parse(text=paste0('l11$var', a, '[is.na(l11$var', a, ') == T] <- 0')))
      eval(parse(text=paste0('l11$s_fish', a, '[is.na(l11$s_fish', a, ') == T] <- 0')))
    }
    # Attribute the level and probabilty to each combination
    eval(parse(text=paste0("l12 <- l11 %>% group_by(rect, ", time_first, ", length) %>% 
                           summarize_at(.vars = vars(", text_max, "), .funs = max)")))
    
    # Rep for each age : careful case where age_min = 0
    l12b <- l12[rep(1:nrow(l12), each = (ages[2] - ages[1] + 1)),]
    l12b$age <- rep(seq(ages[1], ages[2]), nrow(l12))
    
    ## l13
    
    l13 <- subset(l12b, is.na(rect) == F & rect != "")
    
    #####  Specificity for species
    if(specie == 'Ammodytes marinus'){ # Sandeel
      l13$pi0[is.na(l13$pi0) == TRUE & l13$l0 %in% c(8,9) & l13$month %in% c(1.5,3,4,4.5,5,5.5,6,6.5)] <- 0
      l13$pi0[is.na(l13$pi0) == TRUE & (is.na(l13$l0) == TRUE | l13$l0 == 10) & l13$month %in% c(1.5,3,4,4.5)] <- 0
      l13$pi0[l13$month < 5] <- 0
      
      if(yr %in% c(1973,1986)){
        l13$pi2 <- 1
        l13$l2 <- 10
      }
      
      if(yr == 1994){
        l13$pi0 <- 0
        l13$l0 <- 10
      }
      
      if(yr == 1996){
        l13$pi0[l13$month %in% c(1.5,3,4,4.5,5,5.5,6,6.5)] <- 0
        l13$l0[l13$month %in% c(1.5,3,4,4.5,5,5.5,6,6.5)] <- 10
      }
    }
    
    ## Unconditional probabilities
    l13$p <- 0
    eval(parse(text=paste0("l13$p[l13$age == ages[1]] <- l13$pi", ages[1], "[l13$age == ages[1]]")))
    
    for(i in (ages[1]+1):ages[2]){
      prod <- 1
      if(i != ages[2]){ # from first age to last age -1
        for(j in ages[1]:(i)){
          eval(parse(text=paste0("p", j, " <- l13$pi", j, "[l13$age == i]"))) # gets pi for specific age
        }
        for(j in ages[1]:(i-1)){
          eval(parse(text=paste0("prod <- prod*(1 - p", j, ")"))) # calculates product (see formula)
        }          
        eval(parse(text=paste0("l13$p[l13$age == i] <- p", i, " * prod"))) # unconditional probability for specific age
        
      }else{ # Last age
        prod <- 1
        for(j in ages[1]:(i-2)){ # loop until the previous age
          eval(parse(text=paste0("p", j, " <- l13$pi", j, "[l13$age == i]")))
          eval(parse(text=paste0("prod <- prod*(1 - p", j, ")")))
        }
        eval(parse(text=paste0("l13$p[l13$age == i] <- prod - (prod * l13$pi", i-1, "[l13$age == i])"))) # unconditional probability for specific age
      }
    }
    
    # Transposition to 1 column for each age
    # + initialisation for text sql
    text_p <- c(); text_l <- c(); text_pi <- c()
    
    for(i in ages[1]:ages[2]){
      eval(parse(text=paste0("l13$p", i, "<- 0")))
      eval(parse(text=paste0("l13$p", i,"[l13$age == ", i, "] <- l13$p[l13$age == ", i, "]")))
      
      # Text for max
      eval(parse(text=paste0("text_p <- c(text_p, 'p",i,"')"))) #vector p0, p1 ...
      
      if(i != ages[2]){
        eval(parse(text=paste0("text_l <- c(text_l, 'l",i,"')"))) #vector l0, l1 ...
        eval(parse(text=paste0("text_pi <- c(text_pi, 'pi",i,"')"))) #vector pi0, pi1 ...
      }
    }
    
    eval(parse(text=paste0("l14 <- l13 %>% group_by(rect, ", time_first, ", length) %>% 
                           summarize_at(.vars = vars(", paste(paste(text_p, collapse = ", "), paste(text_l, collapse = ", "), 
                                                              paste(text_pi, collapse = ", "), paste(text_var, collapse = ", "),
                                                              paste(text_s_fish, collapse = ", "), sep = ", "), "), .funs = max)")))    
    
    eval(parse(text=paste0('ALKs$a',yr,' <- l14 ')))
    
  }
  return(ALKs)
}