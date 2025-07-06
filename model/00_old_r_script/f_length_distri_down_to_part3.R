
#-----------------------------------------------------------------------------------------------------#
#-------------------------------------- F_LENGTH_DISTRI ----------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_length_distri <- function(path_data, data_rca, data_rhh, data_rhl, data_rsl, data_nwg, data_ALK, path_graph, tab_levels, tab_time, tab_space, species, specie, years,
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
  
  # Import 
  # RDB - Danish
  setwd(path_data)
  rca <- read.csv(data_rca)
  rhl <- read.csv(data_rhl)
  rsl <- read.csv(data_rsl)
  rhh <- read.csv(data_rhh)
  
  # Specie selection
  rca <- subset(rca, sppName %in% species)
  rhl <- subset(rhl, sppName %in% species)
  rsl <- subset(rsl, sppName %in% species)
  
  # Join with rHH to get the dates
  rca <- merge(rca, rhh[,c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "date")], all.x = TRUE, by = c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum"))
  rca$day <- substr(rca$date, 9, 10)
  rca$day <- as.numeric(rca$day)
  
  # Length
  rca$length <- rca$lenCls
  if(type_length == "scm"){
    rca$length[rca$lenCode == 'scm'] <- (floor(rca$lenCls[rca$lenCode == 'scm']*2)/2)/10
    rca$length[rca$lenCode == 'mm'] <- 0.5*floor(rca$lenCls[rca$lenCode == 'mm']*0.2)        
  }
  
  # Group by length
  grp_rca <- rca %>% group_by(sampType, landCtry, vslFlgCtry, year, month, quarter, day, proj, trpCode, staNum, rect, length) %>%
    summarise(number = n(), vgt = sum(indWt)/1000)
  
  # Mean weight
  grp_rca$m_vgt <- with(grp_rca, vgt/number)
  
  # Add number of fish length measured by sample by year, trip, station
  grp_rca <- merge(grp_rca, rsl[,c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "subSampNo")], all.x = TRUE, by = c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum"))
  
  # !!!!! Only while Norway doesn't give rdb data !!!!! #  
  # Merge Nwg and danish
  if(is.null(nrow(data_nwg)) == F){
    l1 <- rbind(grp_rca,data_nwg) 
  }
  
  # Select years and only samples >= 25
  l1 <- subset(l1, year %in% years & subSampNo >= 25 & is.na(rect) == F & rect != "")
  
  # Get areas
  l1 <- merge(l1, tab_space, all.x = TRUE, by = "rect")
  if(space_first != "rect"){
    eval(parse(text=paste0("l1 <- subset(l1, is.na(", space_first, ") == F & ", space_first, " != '999')")))
  }
  
  # Get months for sandeel 
  if(specie == "Ammodytes"){
    l1$month[l1$month %in% c(1,2)] <- 1.5
    l1$month[(l1$day > 15 & l1$month %in% c(4,5,6))] <- l1$month[(l1$day > 15 & l1$month %in% c(4,5,6))]+ 0.5
    l1$month[l1$month %in% c(9,10)] <- 9.5
    l1$month[l1$month %in% c(11,12)] <- 11.5
    
    l2 <- l1
    } else {l2 <- l1}
  
  # Create graph 1
  f_graph1(data_graph = l2, path_graph = path_graph)
  
  # Get the number of samples - wrong
  l2 <- l2[!duplicated(l2[,c("year", time_first, "rect", "day", "subSampNo")]),]
  eval(parse(text=paste0("nb_samples <- l2 %>% group_by(year,", time_first, ", rect) %>% summarise(n_samples = n())")))
  
  # Get rid of half months
  l2$month[l2$month %in% c(4.5,5.5,6.5)] <- floor(l2$month[l2$month %in% c(4.5,5.5,6.5)])
  
  # Group by sample
  eval(parse(text=paste0("l4 <- l1 %>% group_by(year,",time_first, ", rect, sampType, landCtry, vslFlgCtry, proj, trpCode, staNum, subSampNo, length) %>% summarize_at(.vars = vars(vgt, number), .funs = sum)")))
  l4$lnl <- with(l4, log(length))
  l4$m_vgt <- with(l4, vgt/number) 
  
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
  l8b[l8b$m_vgt > 0.5 * l8b$pred2, "m_vgt"] <- l8b[l8b$m_vgt > 0.5 * l8b$pred2, "pred2"] 
  l8b[l8b$m_vgt < 0.5 * l8b$pred2, "m_vgt"] <- l8b[l8b$m_vgt < 0.5 * l8b$pred2, "pred2"]
  l8b[l8b$m_vgt > 0.5 * l8b$pred2, "vgt"] <- with(l8b[l8b$m_vgt > 0.5 * l8b$pred2,] , number*pred2)
  l8b[l8b$m_vgt < 0.5 * l8b$pred2, "vgt"] <- with(l8b[l8b$m_vgt < 0.5 * l8b$pred2,] , number*pred2)
  
  
  return(l8b)
}