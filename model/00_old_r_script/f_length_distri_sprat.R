
#-----------------------------------------------------------------------------------------------------#
#-------------------------------------- F_LENGTH_DISTRI ----------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_length_distri <- function(path_data, data_all, data_ALK, path_graph, tab_levels, tab_time, tab_space, species, specie, years,
                            type_length, ages, space_first, time_first, min_nsamples){
  # Pre-settings  
  # variables used
  vars <- tab_levels$var1[1] #no stratification : we select the first variable to initialize
  if(tab_levels$nb_var[1] != 1){ # Loop only when more than 1 variable
    for(n in 2:tab_levels$nb_var[1]){
      eval(parse(text=paste0('vars <- paste(c(vars, tab_levels$var',n,'[[1]]))'))) #vector of names
    }
  }
  
  # Variables of interest for each level
  vars_lvl <- list()
  for(i in 1:nrow(tab_levels)){
    vars_lvl[[i]] <- tab_levels$var1[i]
    if(tab_levels$nb_var[i] > 1){
      for (j in 2:tab_levels$nb_var[i]){
        eval(parse(text=paste0('vars_lvl[[i]] <- c(vars_lvl[[i]], tab_levels$var', j, '[i])')))
      }
    }
  }
  
  #### Part 1 ####
  setwd(path_data)
  l1 <- read.csv(data_all)
  
  # # Import 
  # # RDB - Danish
  # setwd(path_data)
  # rca <- read.csv(data_rca)
  # rhl <- read.csv(data_rhl)
  # rsl <- read.csv(data_rsl)
  # rhh <- read.csv(data_rhh)
  # 
  # # Specie selection
  # rca <- subset(rca, sppName %in% species)
  # rhl <- subset(rhl, sppName %in% species)
  # rsl <- subset(rsl, sppName %in% species)
  # 
  # # Join with rHH to get the dates
  # rca <- merge(rca, rhh[,c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "date")], all.x = TRUE, by = c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum"))
  # rca$day <- substr(rca$date, 9, 10)
  # rca$day <- as.numeric(rca$day)
  # 
  # # Length
  # rca$length <- rca$lenCls
  # if(type_length == "scm"){
  #   rca$length[rca$lenCode == 'scm'] <- (floor(rca$lenCls[rca$lenCode == 'scm']*2)/2)/10
  #   rca$length[rca$lenCode == 'mm'] <- 0.5*floor(rca$lenCls[rca$lenCode == 'mm']*0.2)        
  # }
  # 
  # # Group by length
  # grp_rca <- rca %>% group_by (sampType, landCtry, vslFlgCtry, year, month, quarter, day, proj, trpCode, staNum, rect, length) %>%
  #   summarise(number = n(), vgt = sum(indWt)/1000)
  # 
  # # Mean weight
  # grp_rca$m_vgt <- with(grp_rca, vgt/number)
  # 
  # # Add number of fish length measured by sample by year, trip, station
  # grp_rca <- merge(grp_rca, rsl[,c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "subSampNo")], all.x = TRUE, by = c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum"))
  # 
  # # !!!!! Only while Norway doesn't give rdb data !!!!! #  
  # # Merge Nwg and danish
  # if(is.null(nrow(data_nwg)) == F){
  #   l1 <- rbind(grp_rca,data_nwg) 
  # } else {
  #   l1 <- grp_rca
  # }
  
  # Select years and only samples >= 25
  l1 <- subset(l1, year %in% years & subSampNo >= 25 & is.na(rect) == F & rect != "")
  
  # Get areas
  l1 <- merge(l1, tab_space, all.x = TRUE, by = "rect")
  if(space_first != "rect"){
    eval(parse(text=paste0("l1 <- subset(l1, is.na(", space_first, ") == F & ", space_first, " != '999')")))
  }
  
  # Get months for sandeel 
  if(specie == 'Ammodytes'){
    l1$month[l1$month %in% c(1,2)] <- 1.5
    l1$month[(l1$day > 15 & l1$month %in% c(4,5,6))] <- l1$month[(l1$day > 15 & l1$month %in% c(4,5,6))]+ 0.5
    l1$month[l1$month %in% c(9,10)] <- 9.5
    l1$month[l1$month %in% c(11,12)] <- 11.5
    
    l2 <- l1
  }else {l2 <- l1}
  
  l2$m_vgt[l2$m_vgt == 0] <- NA
  
  # Create graph 1
  f_graph1(data_graph = l2, path_graph = path_graph)
  
    # Get the number of samples - wrong
  l2 <- l2[!duplicated(l2[,c("year", time_first, "rect", "day", "staNum")]),]
  
  # Get rid of half months
  l2$month[l2$month %in% c(4.5,5.5,6.5)] <- floor(l2$month[l2$month %in% c(4.5,5.5,6.5)])
  eval(parse(text=paste0("nb_samples <- l2 %>% group_by(year,", time_first, ", rect) %>% summarise(n_samples = n())")))
  
  
  # Group by sample
  eval(parse(text=paste0("l4 <- l1 %>% group_by(year,",time_first, ", rect, sampType, landCtry, vslFlgCtry, proj, trpCode, staNum, subSampNo, length) %>% summarize_at(.vars = vars(vgt, number), .funs = sum)")))
  l4$lnl <- with(l4, log(length))
  l4$m_vgt <- with(l4, vgt/number) 
  
  l4$m_vgt[l4$m_vgt == 0] <- NA
  
  l4 <- data.frame(ungroup(l4))
  
  #### Part 2 ####
  
  # GLM - Eliminate extreme weights
  # Split by month/quarter 
  # 2 cases : m_vgt missing or not :
  l4_na <- subset(l4, is.na(m_vgt) == T)
  if(nrow(l4_na) == 0){
    eval(parse(text=paste0("l5<- split(l4, l4$", time_first, ")")))
    
    # Model 
    m1 <- lapply(l5, function (x) {glm(m_vgt ~ lnl, family = Gamma(link = "log"), data = x)})
    
    #  Get predictions
    preds <- list(m1[[1]]$fitted.values)
    for(i in 2:length(m1)){
      preds[[i]] <- m1[[i]]$fitted.values
    }
    
    # Put l5 back together   
    l5a <- do.call(rbind,l5)
    
    # Get all predictions
    m1a <- unlist(preds) 
    l6 <- as.data.frame(cbind(as.data.frame(l5a), data.frame(pred2 = m1a)))
    
    l6a <- l6[!duplicated(l6[, c(time_first, "length")]),]
    
    # Predictions by time and length
    l6b <- merge(l4, l6a[,c(time_first, "length", "pred2")], all.x = TRUE, by = c(time_first, "length")) 
  }else{
    # Get the obsrvations with unknown weight
    l4miss <- subset(l4, is.na(m_vgt) == T)
    l4nomiss <- subset(l4, is.na(m_vgt) == F)
    
    # GLM - Eliminate extreme weights
    # Split by month/quarter 
    eval(parse(text=paste0("l5nomiss <- split(l4nomiss, l4nomiss$", time_first, ")")))
    eval(parse(text=paste0("l5miss <- split(l4miss, l4miss$", time_first, ")")))
    
    # Model 
    m1 <- lapply(l5nomiss, function (x) {glm(m_vgt ~ lnl, family = Gamma(link = "log"), data = x)})
    
    #  Get predictions
    preds <- list(m1[[1]]$fitted.values)
    for(i in 2:length(m1)){
      preds[[i]] <- m1[[i]]$fitted.values
    }
    
    # Put l5 back together   
    l5a <- do.call(rbind,l5nomiss)
    
    # Get all predictions
    m1a <- unlist(preds) 
    l6 <- as.data.frame(cbind(as.data.frame(l5a), data.frame(pred2 = m1a)))
    
    #-- Get predictions when missing values
    for(i in 1:length(l5miss)){
      l5miss[[i]]$pred2 <- predict(m1[[i]], newdata = l5miss[[i]], type = 'response')
    }
    l5amiss <- as.data.frame(do.call(rbind,l5miss))
    #---#
    
    # Join with and without missing weight
    l6b <- rbind(l6, l5amiss)
    
    l6a <- l6b[!duplicated(l6b[, c(time_first, "length")]),]
    
    # Predictions by time and length
    l6b <- merge(l4, l6a[,c(time_first, "length", "pred2")], all.x = TRUE, by = c(time_first, "length")) 
  }
  
  
  # Delete extreme weights
  l6c <- l6b
  l6c$m_vgt[is.na(l6c$pred2) == FALSE & l6c$m_vgt > 1.5*l6c$pred2] <- NA
  l6c$m_vgt[is.na(l6c$pred2) == FALSE & l6c$m_vgt < 0.5*l6c$pred2] <- NA
  
  # GLM by year & month (or quarter)
  # Get 2 datasets : one with NAs for m_vgt, the other without NAs
  l7 <- split(l6c, list(l6c$year, eval(parse(text=paste0('l6c$', time_first)))))
  l7_a <- lapply(l7, function (x) {subset(x, is.na(m_vgt) == FALSE)}) #withoutNA
  l7_b <- lapply(l7, function (x) {subset(x, is.na(m_vgt) == TRUE)}) #withNA
  
  # GLM
  m2 <- lapply(l7_a, function (x) {glm(m_vgt ~ lnl, family = Gamma(link = "log"), data = x)})
  
  # Get predictions
  preds_a <- list()
  for(i in 1:length(m2)){
    preds_a[[i]] <- m2[[i]]$fitted.values
    l7_b[[i]]$pred <- predict(m2[[i]], newdata = l7_b[[i]], type = 'response')
  }
  
  # Join predictions and datasets with no NAs
  l7_c <- do.call(rbind,l7_a)
  m2a <- unlist(preds_a) 
  l8 <- cbind(l7_c, data.frame(pred = m2a))
  
  # Unlist data with NA and bind them to the rest
  l7_d <- do.call(rbind, l7_b)
  l8 <- rbind.fill(l8, l7_d)
  
  # Get preds by year, month(quarter) and length
  l8a <- l8[!duplicated(l8[,c("year", time_first, "length", "pred")]),]
  
  # Merge it with l6c
  l8b <- merge(l6c, l8a[,c("year", time_first, "length", "pred")], all.x = TRUE, by = c("year", time_first, "length"))
  
  # Correcting weight if out of limits
  l8b[is.na(l8b$m_vgt) == TRUE, "vgt"] <- with(l8b[is.na(l8b$m_vgt) == TRUE, ], number*pred) 
  l8b[is.na(l8b$m_vgt) == TRUE, "m_vgt"] <- l8b[is.na(l8b$m_vgt) == TRUE, "pred"]
  # l8b[l8b$m_vgt > 0.5 * l8b$pred2, "m_vgt"] <- l8b[l8b$m_vgt > 0.5 * l8b$pred2, "pred2"] 
  # l8b[l8b$m_vgt < 0.5 * l8b$pred2, "m_vgt"] <- l8b[l8b$m_vgt < 0.5 * l8b$pred2, "pred2"]
  # l8b[l8b$m_vgt > 0.5 * l8b$pred2, "vgt"] <- with(l8b[l8b$m_vgt > 0.5 * l8b$pred2,] , number*pred2)
  # l8b[l8b$m_vgt < 0.5 * l8b$pred2, "vgt"] <- with(l8b[l8b$m_vgt < 0.5 * l8b$pred2,] , number*pred2)
  
  #### Part 3 ####
  l_distri <- list()
  for(y in years[1]:years[2]){
    # Join with ALK
    eval(parse(text=paste0("l9 <- merge(l8b[l8b$year == y,], data_ALK$a", y, ", all.x = TRUE, by = c(time_first, 'rect', 'length'))")))
    
    if(specie == "Ammodytes"){
      l9 <- subset(l9, length >= 4)
    }
    
    # Special for sprat
    if(specie == "Sprattus sprattus"){
      l9[l9$quarter %in% c(1,2) & is.na(l9$p0) == T, c("p0", "p1") ] <- 0
      l9$p1[l9$quarter %in% c(1,2) & l9$p0 != 0] <- l9$p1[l9$quarter %in% c(1,2)  & l9$p0 != 0] + l9$p0[l9$quarter %in% c(1,2) & l9$p0 != 0]
      l9$p0[l9$quarter %in% c(1,2) & l9$p0 != 0 ] <- 0
    }
    
    # Transpose to get 1 line per age instead of having multiple times the same variable for each age
    l9a <- l9[rep(1:nrow(l9), each = (ages[2]-ages[1]+1)),]
    l9a$age <- rep(ages[1]:ages[2])
    for(a in ages[1]:ages[2]){
      eval(parse(text=paste0("l9a$p[l9a$age == ", a, "] <- l9a$p", a, "[l9a$age == ", a, "]")))
    }
    for(a in ages[1]:ages[2]){
      if(a < ages[2]){
        eval(parse(text=paste0("l9a <- subset(l9a, select = -c(p", a, ", l", a, ", pi", a, "))")))
      }else {
        eval(parse(text=paste0("l9a <- subset(l9a, select = -c(p", a, "))")))
      }
    }
    
    # Calculate indicators & create chains of character depending on the age
    
    # Round probas
    l9a$p[l9a$p < 0.000000000001] <- 0
    
    # Estimated number of fish of age i
    l9a$n <- with(l9a, number * p)
    
    # Total number of fish 
    eval(parse(text=paste0(" l9b <- l9a %>% group_by(year, ", time_first, ", rect, sampType, landCtry, vslFlgCtry, proj, trpCode, staNum, subSampNo, 
    length, number, vgt) %>% summarise(nsum = sum(n))")))
    l9a <- merge(l9a, l9b, all.x = TRUE, by = c("year", time_first, "rect" , "sampType", "landCtry", "vslFlgCtry", "proj", "trpCode", "staNum", "subSampNo", "length", "number", "vgt"))
    
    # Total weight estimated of fish of age i
    l9a$w <- with(l9a, vgt * p)
    
    # Estimated cumulated length of age i fish
    l9a$nl <- with(l9a, length * n)
    
    # Particular case
    l9a[is.na(l9a$p) == T,c("p", "n", "nsum", "w", "nl")] <- 0
    
    # Summarise (sum up) data 
    eval(parse(text=paste0(" l10 <- l9a %>% group_by(year, ", time_first  , ", rect , sampType, landCtry, vslFlgCtry, proj, trpCode, staNum, subSampNo, age) %>% 
                           summarize_at(.vars = vars(n, w, nl, vgt, number, nsum), .funs = sum)")))
    
    # Get mean values
    l11 <- subset(l10, nsum >= 25)
    
    # Round n
    l11$n[l11$n < 0.000000000001] <- 0
    
    # Mean number of fish at age i estimated
    l11$n_pr_kg <- with(l11, n/vgt)
    
    # Mean weight of a fish at age i estimated
    l11$mw <- with(l11, w/n)
    
    # Mean length of a fish at age i estimated
    l11$ml <- with(l11, nl/n)
    # 
    l11$p <- with(l11, n_pr_kg/ (nsum/vgt))
    
    # Change NaN to 0 (due to 0/0)(need 0 for grouping later, otherwise the result is NA)
    # l11$mw[is.nan(l11$mw)] <- 0 #kibi: changed
    # l11$ml[is.nan(l11$ml)] <- 0 #kibi: changed
    l11$p[is.nan(l11$p)] <- 0
    
    # Get corresponding time and space
    l11 <- merge(l11, tab_space, all.x = TRUE, by = "rect")
    if(time_first != "month"){
      eval(parse(text=paste0(" tab_time2 <- tab_time[!duplicated(tab_time$", time_first, "), - 1]")))
      l11 <- merge(l11, tab_time2, all.x = TRUE, by = time_first)
    }else{
      l11$month[l11$month %in% c(4.5,5.5,6.5)] <- floor(l11$month[l11$month %in% c(4.5,5.5,6.5)])
      l11 <- merge(l11, tab_time, all.x = TRUE, by = time_first)
    }
    
    ## Dataset with one line per square, month year
    # One line for each time and space
    d1 <- tab_space[tab_space$rect != "" & is.na(tab_space$rect) == F,]
    if(specie == 'Ammodytes'){
      d1 <- d1[rep(1:nrow(d1), each = 9*(years[2]-years[1]+1)),]
      d1$year <- rep(years[1]:years[2], each = 9)
      d1$month <- rep(c(1.5,3,4,5,6,7,8,9.5,11.5))
      d1 <- d1[rep(1:nrow(d1), each = ages[2]-ages[1]+1),]
      d1$age <- rep(ages[1]:ages[2])
    }else{
      d1 <- d1[rep(1:nrow(d1), each = length(table(tab_time[,time_first]))),]
      eval(parse(text=paste0("d1$", time_first, " <- rep(unique(na.omit(tab_time$", time_first, ")))")))
      eval(parse(text=paste0("d1$year <- rep(years[1]:years[2], each = length(unique(na.omit(tab_time$", time_first, "))))")))
      d1 <- d1[rep(1:nrow(d1), each = ages[2]-ages[1]+1),]
      d1$age <- rep(ages[1]:ages[2])
    }
    
    d1 <- merge(d1, tab_time[!duplicated(tab_time[,time_first]),], all.x = TRUE, by = time_first)
    if(time_first != "month"){d1 <- subset(d1, select = -month)}
    eval(parse(text=paste0(" d1 <- with(d1, d1[order(rect,", time_first, "),])")))
    d1$levels <- NA
    
    ######### Loop for each level
    lvl <- list()
    for(i in 1:nrow(tab_levels)){
      # Get summary of indicators
      eval(parse(text=paste0(" l12a <- l11 %>% group_by(", paste(vars_lvl[[i]], collapse = ', '), ", age) %>% 
                             summarize_at(.vars = vars(n_pr_kg, mw, ml, p, number), .funs = mean, na.rm = TRUE)")))
      eval(parse(text=paste0(" l12b <- l11 %>% group_by(", paste(vars_lvl[[i]], collapse = ', '), ", age) %>% 
                             summarise( sn = sum(n), nsamples = n() )")))  
      l12 <- merge(l12a, l12b, all.x = TRUE, by = c(vars_lvl[[i]], "age"))
      
      l13 <- subset(l12, nsamples >= min_nsamples) 
      l13$levels <- i
      
      l13$mw[l13$sn < 10] <- NA
      l13$ml[l13$sn < 10] <- NA
      
      if(specie == "Sprattus sprattus" & i == nrow(tab_levels)){ # Specific code sprat for age = 4
        #Repeat each area3 for each quarter
        l13 <- l13[rep(1:nrow(l13), each = 4),]
        l13$quarter <- rep(1:4, by = 1)
        l13$mw[l13$age == 3 & l13$n_pr_kg == 0] <- 0
        l13$mw[l13$age == 4 & l13$n_pr_kg == 0] <- 0
        
        l13 <- with(l13, l13[order(year, area3, quarter, -age),])
        
        for(j in 1:nrow(l13)){
          if(l13$age[j] == 4){div <- 0}
          div <- div + (l13$n_pr_kg[j] * l13$mw[j])
          if(l13$age[j] == 0 & l13$n_pr_kg[j] != 0){
            l13$factor[j] <- with(l13[j,],(n_pr_kg*mw)/(div))
            if(l13$quarter[j] %in% c(1,2) & l13$age[j] == 0){
              l13$n_pr_kg[j] <- 0
            }else{
              l13$n_pr_kg[j] <- with(l13[j,], n_pr_kg/(1-factor))
            }
          }
        }
        
        l13$mw[l13$age == 3 & l13$mw == 0] <- NA
        l13$mw[l13$age == 4 & l13$mw == 0] <- NA
      }
      
      if(i > 1){
        # Update the final dataset
        if(specie == "Sprattus sprattus" & i == 5 ){
          lvl[[i]] <- merge(lvl[[i-1]], subset(l13, select =-c(number, nsamples, sn)), all.x = TRUE, by = c(vars_lvl[[i]], "quarter", "age"))
        }else{
          lvl[[i]] <- merge(lvl[[i-1]], subset(l13, select =-c(number, nsamples, sn)), all.x = TRUE, by = c(vars_lvl[[i]], "age"))
        }
        lvl[[i]]$levels <- ifelse(!is.na(lvl[[i]]$levels.x), lvl[[i]]$levels.x, lvl[[i]]$levels.y)
        for(a in ages[1]:ages[2]){
          lvl[[i]]$n_pr_kg <- ifelse(!is.na(lvl[[i]]$n_pr_kg.x), lvl[[i]]$n_pr_kg.x, lvl[[i]]$n_pr_kg.y)
          lvl[[i]]$mw <- ifelse(!is.na(lvl[[i]]$mw.x), lvl[[i]]$mw.x, lvl[[i]]$mw.y)
          lvl[[i]]$ml <- ifelse(!is.na(lvl[[i]]$ml.x), lvl[[i]]$ml.x, lvl[[i]]$ml.y)
          lvl[[i]]$p <- ifelse(!is.na(lvl[[i]]$p.x), lvl[[i]]$p.x, lvl[[i]]$p.y)
        }
        # Keep variables of interest
        eval(parse(text=paste0(" lvl[[i]] <- subset(lvl[[i]], select = c(", paste(names(d1), collapse = ', '), ", n_pr_kg, mw, ml, p))")))
      }else{
        lvl[[i]] <- merge(d1, subset(l13, select =-c(number, nsamples, sn)), all.x = TRUE, by = c(vars_lvl[[i]], "age")) 
        lvl[[i]]$levels <- ifelse(!is.na(lvl[[i]]$levels.x), lvl[[i]]$levels.x, lvl[[i]]$levels.y)
        lvl[[i]] <- subset(lvl[[i]], select = -c(levels.x, levels.y))
      }
    }
    # Last dataset of the loop
    d2 <- lvl[[nrow(tab_levels)]]
    d2 <- merge(d2, nb_samples, all.x = T, by = c("year", time_first, "rect"))
    d2$n_samples[is.na(d2$n_samples) == T] <- 0
    
    eval(parse(text=paste0("l_distri$a", y, " <- d2")))
  }
  return(l_distri)
}