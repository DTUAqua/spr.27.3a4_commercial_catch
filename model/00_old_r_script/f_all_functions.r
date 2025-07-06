#-------------------------------------------------------------------#
# ALK, LENGTH DISTRIBUTION AND CATCH AT AGE FUNCTIONS
# All functions details, inputs and outputs are described in the guidelines furnished with the functions
#
# Date : 26-10-2018
#  
# ALK FUNCTIONS : 
#     - f_data_input_nostratif
#     - f_data_input_stratif 
#     - f_levels_ALK
#     - f_ALK_nostrat
#     - f_ALK_strat
#     - f_graph_nostrat
#     - f_graph_strat
#     - f_compute_ALK
#
# LENGTH DISTRIBUTION FUNCTION :
#     - f_graph1
#     - f_length_distri - moved to own script
#
# CATCH AT AGE FUNCTIONS :
#     - f_catch_age_sandeel
#     - f_catch_age_sprat
#
#-------------------------------------------------------------------#

# Charge libraries
library(sqldf)
library(purrr)
library(prettyR)
library(MASS)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)

#-----------------------------------------------------------------------------------------------------#
#                                               ALK                                                   #
#-----------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------#
#--------------------------------------- F_DATA_INPUT ------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_data_input_nostratif <- function(path_data, data_rca, data_rhh, tab_space, tab_time, time_first, space_first, specie, type_length, ages, years, recruit_month,
                                   length_pred, length_obs ){
  
  # ------------------------------- ##
  # ------- Load data ------------- ##
  #setwd(path_data)
  rca <- read.csv(paste0(path_data, data_rca))
  rhh <- read.csv(paste0(path_data, data_rhh))
  
  # Specie selection
  rca <- subset(rca, sppName == specie) # & !(is.na(age)))
  
  #Delete small fish - kibi lines
  rca <- subset(rca, lenCls >= length_obs[1]*10)
  
  ## ------------------------------- ##
  ## --------- Linkage ------------- ##
  
  # Get the dates from rHH
  rca <- merge(rca, rhh[,c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "date")],
               all.x = TRUE, by = c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum"))
  
  ## --------------------------------------------------------------------------------- ##
  ## --------- Variables modification / Count number fish per combination------------- ##
  
  # Getting day
  rca$day <- substr(rca$date, 9, 10)
  rca$day <- as.numeric(rca$day)
  
  # Getting same length for all fish according to the length given by the user
  rca$length <- rca$lenCls
  if(type_length == "scm"){
    rca$length[na.omit(rca$lenCode) == 'scm'] <- (floor(rca$lenCls[na.omit(rca$lenCode) == 'scm']*2)/2)/10
    rca$length[na.omit(rca$lenCode) == 'mm'] <- 0.5*floor(rca$lenCls[na.omit(rca$lenCode) == 'mm']*0.2)   
  }
  
  # !!!!! Only while Norway doesn't give rdb data !!!!! #
  # Gathering ages >= maximum age chosen, selecting years
  
  if(is.null(nrow(rca_nwg)) == F){
    in1 <- rbind.fill(rca, rca_nwg)
  } else {
    in1 <- rca
  }
  in1$age[in1$age >= ages[2]] <- ages[2]
  in1 <- subset(in1, is.na(age) == F & (years[1] <= year & year <= years[2]))
  
  # Group information by time, space and length
  if(specie != 'Ammodytes marinus' & space_first != "rect"){
    in1 <- merge(in1, tab_space[,c("rect", space_first)], by = c("rect"), all.x = T)
    eval(parse(text=paste0("in1 <- subset(in1, is.na(", space_first, ") == F & ", space_first, "!= 999)")))
  }else{
    in1 <- subset(in1, is.na(rect) == F & rect != "") 
  }
  
  
  eval(parse(
    text = paste0(
      "in1b <- in1 %>% group_by (year, month, day,",
      space_first,
      ", age, length) %>% summarise(n_age = n(), .groups = 'drop')"
    )
  ))
  
  text_n <- c()
  text_s <- c()
  n_age <- 0
  
  for(i in ages[1]:ages[2]){
    eval(parse(text=paste0('in1b$n',i,' <- 0')))
    eval(parse(text=paste0('in1b$n',i,'[in1b$age == ',i,'] <- in1b$n_age[in1b$age == ',i,']')))
    eval(parse(text=paste0("text_n <- c(text_n, 'n",i,"')"))) # Create vector for n
    eval(parse(text=paste0("text_s <- c(text_s, 's",i,"')"))) # Create vector for s
  }
  
  # in1c <-  unique(in1b[,c("year", time_first, space_first, "length")])
  eval(parse(text=paste0("in1c <- in1b %>% group_by (year, month, day, ", space_first, ", length ) %>%
                         summarize_at(.vars = vars(n_age,", paste(text_n, collapse = ', '), "), .funs = sum)")))
  in1c2 <- in1c[rep(1:nrow(in1c), each = ages[3]),]
  in1c2$age <- rep(seq(ages[1],ages[2]), (nrow(in1c2)/ages[3]))
  
  # 1 line per combination (no length)
  in1d <- unique(in1c[,c("year", "month", "day", space_first)])
  
  ### Creation table with all combinations
  in2 <- in1d[rep(1:nrow(in1d), each = ages[3]),]
  in2$age <- rep(seq(ages[1],ages[2]), (nrow(in2)/ages[3]))
  in2a <- in2[rep(1:nrow(in2), each = length_pred[5]), ]
  in2a$length <- rep(seq(length_pred[1],length_pred[2],length_pred[3]), nrow(in2))
  
  in3 <- merge(in2a, in1c2, by = c("year", "month", "day", space_first, "length", "age"), all.x = T)
  in3$n_age[is.na(in3$n_age) == T] <- 0
  
  for(a in ages[1]:ages[2]){
    eval(parse(text=paste0('in3$n', a, '[is.na(in3$n', a, ' == T)] <- 0')))
    eval(parse(text=paste0('in3$n[in3$age == ', a, '] <- in3$n', a, '[in3$age == ', a, ']')))
    eval(parse(text=paste0('in3$s',a,' <- 0')))
  }
  
  # Number of fish per age
  for(i in ages[1]:ages[2]){
    eval(parse(text=paste0('in3$s', i,'[in3$age == ', i, '] <- with(in3[in3$age == ', i, ',], ', paste(text_n[(i+1):ages[3]], collapse = " + "), ')')))
    eval(parse(text=paste0('in3$n', i, '[in3$age != ', i, '] <- 0')))
  }
  
  # Number of fish for the correct length
  #eval(parse(text=paste0('in3$s', ages[1], '[in3$length < length_obs[1] ] <- 0')))
  
  # Put some order
  eval(parse(text=paste0('in3 <- with(in3,in3[order(year, month, day, ', space_first, ', length, age),])')))
  
  # Recruiting month cut
  if(is.na(recruit_month) == F){eval(parse(text=paste0('in3$n', ages[1], '[in3$month <= ', recruit_month, '] <- 0')))}
  
  # Get space and time variables
  if(specie == 'Ammodytes marinus'){
    in3$month[in3$month %in% c(1,2)] <- 1.5
    in3$month[(in3$day > 15 & in3$month %in% c(4,5,6))] <- in3$month[(in3$day > 15 & in3$month %in% c(4,5,6))]+ 0.5
    in3$month[in3$month %in% c(9,10)] <- 9.5
    in3$month[in3$month %in% c(11,12)] <- 11.5
    in4 <- subset(in3, is.na(rect) == F & rect != "")
  }else {
    in4 <- merge(in3, tab_time, all.x = T, by = "month")
    eval(parse(text=paste0("in4 <- subset(in4, is.na(", space_first, ") == F & ", space_first, " != 999 & ", space_first, " != '' )" )))
  }
  
  # Build final data set pre_key
  eval(parse(text=paste0("in5 <- in4 %>% group_by (year,", time_first, ", ", space_first, ", length ) %>%
                         summarize_at(.vars = vars(", paste(paste(text_n, collapse = ', '), paste(text_s, collapse = ', '), sep = ', '), "), .funs = sum)")))
  
  if(specie == 'Ammodytes marinus'){
    in5 <- merge(in5, tab_time, all.x = T, by = "month")
    pre_key <- merge(in5, tab_space, all.x = T, "rect")
  }else{
    in5 <- merge(in5, tab_time[!duplicated(time_table[,time_first]),-1], all.x = T, by = time_first)
    pre_key <- merge(in5, tab_space[!duplicated(tab_space[,space_first]), !(names(tab_space) %in% c("rect"))], all.x = T, by = space_first)    
  }
  return(pre_key)
}


f_data_input_stratif <- function(path_data, data_rca, data_rhh, tab_space, tab_time, time_first, space_first, specie, type_length, ages, years, recruit_month,
                                 length_pred, length_obs, stratif ){
  
  # Stratification variables
  strat_all <- paste(stratif, collapse = ", ")
  
  # ------------------------------- ##
  # ------- Load data ------------- ##
  setwd(path_data)
  rca <- read.csv(data_rca)
  rhh <- read.csv(data_rhh)
  
  # Specie selection
  rca <- subset(rca, sppName == specie)
  
  ## ------------------------------- ##
  ## --------- Linkage ------------- ##
  
  # Get the dates from rHH
  rca <- merge(rca, rhh[,c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "date")],
               all.x = TRUE, by = c("sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum"))
  
  ## --------------------------------------------------------------------------------- ##
  ## --------- Variables modification / Count number fish per combination------------- ##
  
  # Getting day
  rca$day <- substr(rca$date, 9, 10)
  rca$day <- as.numeric(rca$day)
  
  # Getting same length for all fish according to the length given by the user
  rca$length <- rca$lenCls
  if(type_length == "scm"){
    rca$length[na.omit(rca$lenCode) == 'scm'] <- (floor(rca$lenCls[na.omit(rca$lenCode) == 'scm']*2)/2)/10
    rca$length[na.omit(rca$lenCode) == 'mm'] <- 0.5*floor(rca$lenCls[na.omit(rca$lenCode) == 'mm']*0.2)    
  }
  
  # !!!!! Only while Norway doesn't give rdb data !!!!! #
  # Gathering ages >= maximum age chosen, selecting years
  in1 <- rbind.fill(rca, rca_nwg)
  in1$age[in1$age >= ages[2]] <- ages[2]
  in1 <- subset(in1, is.na(age) == F & (years[1] <= year & year <= years[2]))
  
  # !!!! Temporary, while nowergian data not in RDB, or if there are missing values !!!! #
  for(s in 1:length(stratif)){
    eval(parse(text=paste0("in1$", stratif[s], " <- as.character(in1$", stratif[s], ")")))
    eval(parse(text=paste0("in1$", stratif[s], "[is.na(in1$", stratif[s], ") == T] <- 'miss'")))
    eval(parse(text=paste0("in1$", stratif[s], " <- as.factor(in1$", stratif[s], ")")))
  }
  
  # Group information by time, space and length
  if(specie != 'Ammodytes marinus' & space_first != "rect"){
    in1 <- merge(in1, tab_space[,c("rect", space_first)], by = c("rect"), all.x = T)
    eval(parse(text=paste0("in1 <- subset(in1, is.na(", space_first, ") == F & ", space_first, "!= 999)")))
  }else{
    in1 <- subset(in1, is.na(rect) == F & rect != "") 
  }
  
  eval(parse(text=paste0("in1b <- in1 %>% group_by (", strat_all, ", year, month, day,", space_first, ", age, length) %>% summarise(n_age = n(), .groups = 'drop')")))
  
  # Initialisation text variables
  text_n <- c();text_s <- c()
  n_age <- 0
  
  for(i in ages[1]:ages[2]){
    eval(parse(text=paste0('in1b$n',i,' <- 0')))
    eval(parse(text=paste0('in1b$n',i,'[in1b$age == ',i,'] <- in1b$n_age[in1b$age == ',i,']')))
    eval(parse(text=paste0("text_n <- c(text_n, 'n",i,"')"))) # Create vector for n
    eval(parse(text=paste0("text_s <- c(text_s, 's",i,"')"))) # Create vector for s
  }
  
  eval(parse(text=paste0("in1c <- in1b %>% group_by (", strat_all, ", year, month, day, ", space_first, ", length ) %>%
                         summarize_at(.vars = vars(n_age,", paste(text_n, collapse = ', '), "), .funs = sum)")))
  
  in1c2 <- in1c[rep(1:nrow(in1c), each = ages[3]),]
  in1c2$age <- seq(ages[1],ages[2])
  
  # 1 line per combination (no length)
  in1d <- unique(in1c[,c(stratif, "year", "month", "day", space_first)])
  
  ### Creation table with all combinations
  in2 <- in1d[rep(1:nrow(in1d), each = ages[3]),]
  in2$age <- seq(ages[1],ages[2])
  in2a <- in2[rep(1:nrow(in2), each = length_pred[5]), ]
  in2a$length <- seq(length_pred[1],length_pred[2],length_pred[3])
  
  in3 <- merge(in2a, in1c2, by = c(stratif, "year", "month", "day", space_first, "length", "age"), all.x = T)
  in3$n_age[is.na(in3$n_age) == T] <- 0
  
  for(a in ages[1]:ages[2]){
    eval(parse(text=paste0('in3$n', a, '[is.na(in3$n', a, ' == T)] <- 0')))
    eval(parse(text=paste0('in3$n[in3$age == ', a, '] <- in3$n', a, '[in3$age == ', a, ']')))
    eval(parse(text=paste0('in3$s',a,' <- 0')))
  }
  
  # Number of fish per age
  for(i in ages[1]:ages[2]){
    eval(parse(text=paste0('in3$s', i,'[in3$age == ', i, '] <- with(in3[in3$age == ', i, ',], ', paste(text_n[(i+1):ages[3]], collapse = " + "), ')')))
    eval(parse(text=paste0('in3$n', i, '[in3$age != ', i, '] <- 0')))
  }
  
  # Put some order
  eval(parse(text=paste0('in3 <- with(in3,in3[order(', strat_all, ', year, month, day, ', space_first, ', length, age),])')))
  
  # Recruiting month cut
  if(is.na(recruit_month) == F){eval(parse(text=paste0('in3$n', ages[1], '[in3$month <= ', recruit_month, '] <- 0')))}
  
  # Get space and time variables
  if(specie == 'Ammodytes marinus'){
    in3$month[in3$month %in% c(1,2)] <- 1.5
    in3$month[(in3$day > 15 & in3$month %in% c(4,5,6))] <- in3$month[(in3$day > 15 & in3$month %in% c(4,5,6))]+ 0.5
    in3$month[in3$month %in% c(9,10)] <- 9.5
    in3$month[in3$month %in% c(11,12)] <- 11.5
    in4 <- subset(in3, is.na(rect) == F & rect != "")
  }else {
    in4 <- merge(in3, tab_time, all.x = T, by = "month")
    eval(parse(text=paste0("in4 <- subset(in4, is.na(", space_first, ") == F & ", space_first, " != 999 & ", space_first, " != '' )" )))
  }
  
  # Build final data set pre_key
  eval(parse(text=paste0("in5 <- in4 %>% group_by (", strat_all, ", year,", time_first, ", ", space_first, ", length ) %>%
                         summarize_at(.vars = vars(", paste(paste(text_n, collapse = ', '), paste(text_s, collapse = ', '), sep = ', '), "), .funs = sum)")))
  
  if(specie == 'Ammodytes marinus'){
    in5 <- merge(in5, tab_time, all.x = T, by = "month")
    pre_key <- merge(in5, tab_space, all.x = T, "rect")
  }else{
    in5 <- merge(in5, tab_time[!duplicated(time_table[,time_first]),-1], all.x = T, by = time_first)
    pre_key <- merge(in5, tab_space[!duplicated(tab_space[,space_first]),-1], all.x = T, by = space_first)    
  }
  return(pre_key)
}

#-----------------------------------------------------------------------------------------------------#
#--------------------------------------- F_LEVELS_ALK ------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

# Fonction qui retourne la bDD ? utiliser en fonction du niveau choisi

f_levels_ALK <- function(pre_key, lvl, ages, tab_levels, vars_r, list_text, levels, space_first, uncertainty, min_sfish){
  vars <- tab_levels$vars[lvl]
  
  # Automatisation, to do sums for each age 
  var_n <- c()
  var_s <- c()
  for (a in (ages[1]):ages[2]){
    eval(parse(text=paste0("var_n <- c(var_n, 'n",a,"')"))) # vector n0 n1 ..
    eval(parse(text=paste0("var_s <- c(var_s, 's",a,"')"))) # vector s0 s1
  }
  var_ns <- c(var_n, var_s) #concat character chains
  
  # Level selection 
  eval(parse(text=paste0("aglg1 <- pre_key %>% group_by (", vars, ", length ) %>%
                         summarize_at(.vars = vars(", paste(var_ns, collapse = ','), "), .funs = sum)")))
  
  # Delete when missing area or time (except first level where it has already been done)
  if(lvl != 1){
    for(v in 1:length(vars_r[[lvl]])){
      eval(parse(text=paste0("aglg1 <- aglg1[!(aglg1$", vars_r[[lvl]][v], " %in% c('999', '99', '') | is.na(aglg1$", vars_r[[lvl]][v], ") == T),]")))
    }
  }
  
  # Transposition One line for each age
  aglg2 <- aglg1[rep(1:nrow(aglg1), each = ages[3]),]
  aglg2$age <- rep(seq(ages[1], ages[2]), nrow(aglg1))
  
  # Creation variable n and s : number of fish by combination and age
  for (i in ages[1]:ages[2]){
    eval(parse(text=paste0("aglg2$n[aglg2$age ==",i, "]<- aglg2$n",i,"[aglg2$age ==",i,"]") )) # Attribute to n the value associated for each age
    eval(parse(text=paste0("aglg2$s[aglg2$age ==",i, "]<- aglg2$s",i,"[aglg2$age ==",i,"]") )) # Attribute to s the value associated for each age
  }
  
  aglg2$n[aglg2$s == 0] <- NA
  aglg2$s[aglg2$s == 0 | is.na(aglg2$s) == T] <- 1
  
  # Define the minimum and maximum length observed for each combination
  aglg21 <- subset(aglg2, is.na(n) == FALSE)
  eval(parse(text=paste0("aglg2aa <- aglg21 %>% group_by(", vars, ", age) %>% summarise(n_age = sum(n), means = sum(s), nlengths = n(), .groups = 'drop')")))
  aglg2a2 <- subset(aglg2, n != 0 & is.na(n) == FALSE)
  eval(parse(text=paste0("aglg2b <- aglg2a2 %>% group_by(", vars, ", age) %>% summarise(minlgth = min(length), maxlgth = max(length), .groups = 'drop')")))
  
  aglg3a <- merge(aglg2, aglg2aa, by = c("age", vars_r[[lvl]]), all.x = TRUE)
  aglg3 <- merge(aglg3a, aglg2b, by = c("age", vars_r[[lvl]]), all.x = TRUE)
  
  aglg3 <- subset(aglg3, nlengths > 1 )
  aglg3 <- aglg3[!(na.omit(aglg3$means) < 10 & is.na(aglg3$maxlgth) == FALSE),]
  
  # Cutting tails
  
  aglg3$n[is.na(aglg3$maxlgth) == FALSE & ((aglg3$maxlgth < 18 & aglg3$length > (aglg3$maxlgth + 2))
                                           |(aglg3$maxlgth == 19 & aglg3$length > (aglg3$maxlgth + 3))
                                           |(aglg3$maxlgth > 19 & aglg3$length > (aglg3$maxlgth + 4))
  )] <- NA
  
  aglg3$n[is.na(aglg3$minlgth) == FALSE & ((aglg3$minlgth < 20 & aglg3$length < (aglg3$minlgth - 2))
                                           |(aglg3$minlgth == 22 & aglg3$length < (aglg3$minlgth - 3))
                                           |(aglg3$minlgth >= 24 & aglg3$length < (aglg3$minlgth - 4))
  )] <- NA
  
  #### Model
  
  # Only ages oldest than the maximum age
  aglg4 <- subset(aglg3, age < ages[2] )
  
  # Getting all possible combinations by age, year, month and rectangle
  eval(parse(text=paste0( "l <- list(aglg4$age,", paste(list_text[[lvl]], collapse = ","), ")")))
  combi <-  split(aglg4, l)
  combia <- combi
  
  # Deleting combinations without observations
  for(i in 1:length(combi)){
    nom <- names(combi)[i]
    if(nrow(combi[[i]]) == 0){ eval(parse(text=paste0('combia$`',nom,"` <- NULL")))}
  }
  rm(combi)
  
  ### GLM
  
  models <- lapply(combia, function (x) {glm(cbind(n,s-n) ~ length, family = binomial, data = x)})
  
  #### Estimations when n different from NA #########
  # Get confidence intervals
  
  p <- lapply(models, function (x) {predict(x, type = "link", se.fit = TRUE)})
  
  lower <- lapply(p, function(x) { exp(x$fit - 1.96*x$se.fit)/(1+exp(x$fit - 1.96*x$se.fit))})
  upper <- lapply(p, function(x) { exp(x$fit + 1.96*x$se.fit)/(1+exp(x$fit + 1.96*x$se.fit))})
  
  # Joining results with the table
  aglg10 <- combia
  
  # Handling row.names with merge
  rn <- function(tab){
    row.names(tab) <- tab$Row.names
    tab$Row.names <- NULL
    return(tab)
  }
  
  for(i in 1:length(combia)){
    aglg10[[i]] <- merge(aglg10[[i]], data.frame(pred = models[[i]][[3]]), by = 'row.names', all.x = TRUE)
    aglg10[[i]] <- rn(aglg10[[i]])
    aglg10[[i]] <- merge(aglg10[[i]], data.frame(low = lower[[i]]), by = 'row.names', all.x = TRUE)
    aglg10[[i]] <- rn(aglg10[[i]])
    aglg10[[i]] <- merge(aglg10[[i]], data.frame(up = upper[[i]]), by = 'row.names', all.x = TRUE)
    aglg10[[i]] <- rn(aglg10[[i]])
  }
  
  #### Estimations when n is NA #########
  
  withNA <- lapply(aglg10, function (x) {subset(x, is.na(pred) == TRUE)})
  withoutNA <- lapply(aglg10, function (x) {subset(x, is.na(pred) == F)})
  withNA_p <- list()
  
  for(i in 1:length(withNA)){
    withNA[[i]]$pred <- predict(models[[i]], newdata = withNA[[i]], type = 'response')
    withNA_p[[i]] <- predict(models[[i]], newdata = withNA[[i]], se.fit = T)
    withNA[[i]]$low <- exp(withNA_p[[i]]$fit - 1.96*withNA_p[[i]]$se.fit)/(1+exp(withNA_p[[i]]$fit - 1.96*withNA_p[[i]]$se.fit))
    withNA[[i]]$up <- exp(withNA_p[[i]]$fit + 1.96*withNA_p[[i]]$se.fit)/(1+exp(withNA_p[[i]]$fit + 1.96*withNA_p[[i]]$se.fit))
    aglg10[[i]] <- rbind(withNA[[i]], withoutNA[[i]])
  }
  
  #####
  
  # Get one dataset instead of list
  aglg12 <- do.call(rbind, aglg10)
  eval(parse(text=paste0( "aglg12 <- with(aglg12,aglg12[order(age,", vars, ", length),])")))
  
  aglg13 <- aglg12
  
  aglg13$maxp[(is.na(aglg13$maxlgth) == F) & (aglg13$length == aglg13$maxlgth)] <- aglg13$pred[(is.na(aglg13$maxlgth) == F) & (aglg13$length == aglg13$maxlgth)]
  aglg13$minp[(is.na(aglg13$minlgth) == F) & (aglg13$length == aglg13$minlgth)] <- aglg13$pred[(is.na(aglg13$minlgth) == F) & (aglg13$length == aglg13$minlgth)]
  aglg13$up[is.nan(aglg13$up) == T] <- 1
  aglg13$prange <- with(aglg13, up-low)
  aglg13$prange[is.na(aglg13$n) == T] <- NA
  
  aglg13$sum <- aglg13$s
  aglg13$sum[is.na(aglg13$n) == T] <- NA
  eval(parse(text=paste0("aglg13b <- aglg13 %>% group_by(age, ", vars, ") %>% summarise(prange = max(na.omit(prange)), 
                         minp = max(na.omit(minp)), maxp = max(na.omit(maxp)), sums = sum(s) , .groups = 'drop')")))
  eval(parse(text=paste0( " aglg12 <- subset(aglg12, select = c(age,", vars,", length, minlgth, maxlgth, pred, n, s))")))
  
  aglg13b$minp[aglg13b$minp == '-Inf'] <- NA
  aglg13b$maxp[aglg13b$maxp == '-Inf'] <- NA
  
  # Summarizing by each available combination
  aglg13c <-  merge(aglg12, aglg13b, by = c("age", vars_r[[lvl]]), all.x = TRUE)
  
  tol <- 1e-8
  
  if(lvl != levels[2]){
    aglg14 <- aglg13c[!((is.na(aglg13c$minp) == F & abs(aglg13c$maxp - aglg13c$minp) <= tol)),] # remove when very clode to 1
    aglg14 <- aglg14[!((is.na(aglg14$minp) == F & aglg14$maxp >= aglg14$minp)),]
    aglg14 <- aglg14[!(aglg14$age == 0 & is.na(aglg14$maxp) == F & aglg14$maxp > 0.9999),]
    aglg14 <- aglg14[!(aglg14$sums < min_sfish & aglg14$prange > uncertainty),]
    return(aglg14)
  }else {
    return(aglg13c)
  }
}

#-----------------------------------------------------------------------------------------------------#
#--------------------------------------- F_ALK -------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_ALK_nostrat <- function(ages, levels, years, areas, list_lvl, specie, length_pred, vars_r, tab_levels, tab_space, tab_time, space_first, time_first,
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

f_ALK_strat <- function(ages, levels, years, areas, list_lvl, specie, length_pred, vars_r, tab_levels, tab_space, tab_time, stratif, space_first, time_first,
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
    
    # Base table
    combs <- tab_space
    eval(parse(text=paste0('combs <- combs[rep(1:nrow(combs), each = length(na.omit(tab_time2$', time_first, '))*(ages[2]-ages[1])*length_pred[5]),] '))) # (month) * (ages-1) * (length range)
    combs$year <- yr
    eval(parse(text=paste0('combs$',time_first,' <- rep(na.omit(tab_time2$', time_first , '), each = (ages[2]-ages[1])*length_pred[5])')))
    combs$age <- rep(ages[1]:(ages[2]-1), each = length_pred[5])
    combs$length <- seq(length_pred[1],length_pred[2],length_pred[3])    
    
    # Initialisation
    nmoda <- 0 
    list_modas <- list()
    text_split <- c()
    for(s in 1:length(stratif)){
      eval(parse(text=paste0('nmoda <- nmoda + length(levels(list_lvl[[1]]$', stratif[s], '))'))) # Get the number of modalities
      eval(parse(text=paste0('list_modas[[s]] <- levels(list_lvl[[1]]$', stratif[s], ')'))) # Get modalities names
      eval(parse(text=paste0('text_split <- c(text_split, "combs$', stratif[s], '")')))
    }
    
    # Complete the main dataset
    combs <- combs[rep(1:nrow(combs), each = nmoda),]
    combs <- merge(combs, tab_time2, by = time_first, all.x = T)
    
    combi_modas <- as.data.frame(expand.grid(list_modas)) # Get all the possible combination of modalities
    
    # Sort the data
    eval(parse(text=paste0('combs <- with(combs,combs[order(year, ', space_first, ', ', time_first, ', length, age),])')))
    
    for (s in 1:length((stratif))) { # Complete the main dataset with the different combinations
      eval(parse(text=paste0('combs$', stratif[s], '<- rep(combi_modas[,s])')))
    }
    
    # Create list containing each combination
    eval(parse(text=paste0('list_combi1 <- split(combs, list( ', paste(text_split, collapse = ","), '))')))
    list_combi<- list_combi1
    for(i in 1:length(list_combi1)){
      nom <- names(list_combi1)[i]
      if(nrow(list_combi1[[i]]) == 0){ eval(parse(text=paste0('list_combi$`',nom,"` <- NULL")))}
    }
    rm(list_combi1)
    
    #### Combination all levels in one dataset (for each year)
    #################### Ajouter ann?e !
    l_comb  <- list()
    for(c in 1:(nmoda)){
      for(l in levels[1]:levels[2]){
        if(l == levels[1]){
          eval(parse(text=paste0('comb',l, ' <- merge(list_combi[[c]], lvls_yrs$lv',l,'_l$`', yr,'`, by = c(vars_r[[l]],"age", "length"), all.x = TRUE)')))
          eval(parse(text=paste0('comb',l, ' $level <- l')))
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
          eval(parse(text=paste0('comb',l, ' <- merge(comb',l-1, ', lvls_yrs$lv',l,'_l$`',yr,'`,  by = c(vars_r[[l]],"age", "length"), all.x = TRUE)')))
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
      eval(parse(text=paste0('l_comb[[c]] <- comb', levels[2])))
      names(l_comb)[c] <- names(list_combi)[c]
    }
    
    ####
    l11 <- lapply(l_comb, function(x){subset(x, is.na(rect) == F)})
    
    # Initialization vectors for sql
    text_pi <- c();text_l <- c();text_var <- c();text_s_fish <- c()
    
    # Creation variables pi and level for each age
    for (c in 1:(nmoda)){
      for (i in ages[1]: (ages[2]-1)){
        eval(parse(text=paste0('l11[[c]]$pi',i,'[l11[[c]]$age == i] <- l11[[c]]$pi[l11[[c]]$age == i]')))
        eval(parse(text=paste0('l11[[c]]$l',i,'[l11[[c]]$age == i] <- l11[[c]]$level[l11[[c]]$age == i]')))
        eval(parse(text=paste0('l11[[c]]$var',i,'[l11[[c]]$age == ',i,'] <- l11[[c]]$variance[l11[[c]]$age == ',i,']')))
        eval(parse(text=paste0('l11[[c]]$s_fish',i,'[l11[[c]]$age == ',i,'] <- l11[[c]]$s_fish[l11[[c]]$age == ',i,']')))
        
        # Text for max calculation (only one repeat)
        if(c == 1){
          eval(parse(text=paste0('text_pi <- c(text_pi, "pi',i,'")'))) #vector pi0, pi1, pi2, pi3
          eval(parse(text=paste0('text_l <- c(text_l, "l',i,'")'))) #vector l0, l1, l2, ...
          eval(parse(text=paste0('text_var <- c(text_var, "var',i,'")'))) #vector var0, var1, var2, ...
          eval(parse(text=paste0('text_s_fish <- c(text_s_fish, "s_fish',i,'")'))) #vector s_fish0, s_fish1, s_fish2, ...
        }
      }
    }
    
    text_max <- paste(paste(text_pi, collapse = ", "), paste(text_l, collapse = ", "), 
                      paste(text_var, collapse = ", "), paste(text_s_fish, collapse = ", "),sep = ", ")
    
    # Replacing NA by '0' for the group by function 
    for(c in 1:(nmoda)){
      for (a in ages[1]: (ages[2]-1)){
        eval(parse(text=paste0('l11[[c]]$pi', a, '[is.na(l11[[c]]$pi', a, ') == T] <- 0')))
        eval(parse(text=paste0('l11[[c]]$l', a, '[is.na(l11[[c]]$l', a, ') == T] <- 0')))
        eval(parse(text=paste0('l11[[c]]$var', a, '[is.na(l11[[c]]$var', a, ') == T] <- 0')))
        eval(parse(text=paste0('l11[[c]]$s_fish', a, '[is.na(l11[[c]]$s_fish', a, ') == T] <- 0')))
      }
    }
    
    # Attribute the level and probabilty to each combination
    eval(parse(text=paste0("l12 <- lapply(l11, function(x){x %>% group_by(rect, ", time_first, ", length) %>% 
                           summarize_at(.vars = vars(", text_max, "), .funs = max)})")))
    
    # Rep for each age : careful case where age_min = 0
    l12b <- lapply(l12, function(x){x[rep(1:nrow(x), each = (ages[2] - ages[1] + 1)),]})
    l12b <- lapply(l12b, function(x) {cbind(x, age = rep(seq(ages[1], ages[2], by = 1), nrow(l12b[[1]])/(ages[2] - ages[1] + 1)) )})
    
    ## l13
    l13 <- lapply(l12b, function(x) {subset(x, is.na(rect) == F & rect != "")})
    
    #####  Specificity for species
    if(specie == 'Ammodytes marinus'){ # Sandeel
      for(c in 1:(nmoda)){
        l13[[c]]$pi0[is.na(l13[[c]]$pi0) == TRUE & l13[[c]]$l0 %in% c(8,9) & l13[[c]]$month %in% c(1.5,3,4,4.5,5,5.5,6,6.5)] <- 0
        l13[[c]]$pi0[is.na(l13[[c]]$pi0) == TRUE & (is.na(l13[[c]]$l0) == TRUE | l13[[c]]$l0 == 10) & l13[[c]]$month %in% c(1.5,3,4,4.5)] <- 0
        l13[[c]]$pi0[l13[[c]]$month < 5] <- 0
        
        if(yr %in% c(1973,1986)){
          l13[[c]]$pi2 <- 1
          l13[[c]]$l2 <- 10
        }
        
        if(yr == 1994){
          l13[[c]]$pi0 <- 0
          l13[[c]]$l0 <- 10
        }
        
        if(yr == 1996){
          l13[[c]]$pi0[l13[[c]]$month %in% c(1.5,3,4,4.5,5,5.5,6,6.5)] <- 0
          l13[[c]]$l0[l13[[c]]$month %in% c(1.5,3,4,4.5,5,5.5,6,6.5)] <- 10
        }
      }
    }
    
    ## Unconditional probabilities
    l13 <- lapply(l13, function(x) {cbind(x, p = rep(0, nrow(l13[[1]])) ) })
    
    for(c in 1:(nmoda)){
      eval(parse(text=paste0("l13[[c]]$p[l13[[c]]$age == ages[1]] <- l13[[c]]$pi", ages[1], "[l13[[c]]$age == ages[1]]")))
      
      for(i in (ages[1]+1):ages[2]){
        prod <- 1
        if(i != ages[2]){ # from first age to last age -1
          for(j in ages[1]:(i)){
            eval(parse(text=paste0("p", j, " <- l13[[c]]$pi", j, "[l13[[c]]$age == i]"))) # gets pi for specific age
          }
          for(j in ages[1]:(i-1)){
            eval(parse(text=paste0("prod <- prod*(1 - p", j, ")"))) # calculates product (see formula)
          }          
          eval(parse(text=paste0("l13[[c]]$p[l13[[c]]$age == i] <- p", i, " * prod"))) # unconditional probability for specific age
          
        }else{ # Last age
          prod <- 1
          for(j in ages[1]:(i-2)){ # loop until the previous age
            eval(parse(text=paste0("p", j, " <- l13[[c]]$pi", j, "[l13[[c]]$age == i]")))
            eval(parse(text=paste0("prod <- prod*(1 - p", j, ")")))
          }
          eval(parse(text=paste0("l13[[c]]$p[l13[[c]]$age == i] <- prod - (prod * l13[[c]]$pi", i-1, "[l13[[c]]$age == i])"))) # unconditional probability for specific age
        }
      }
      
      # Transposition to 1 column for each age
      # + initialisation for text sql
      text_p <- c(); text_l <- c(); text_pi <- c()
      
      for(i in ages[1]:ages[2]){
        eval(parse(text=paste0("l13[[c]]$p", i, "<- 0")))
        eval(parse(text=paste0("l13[[c]]$p", i,"[l13[[c]]$age == ", i, "] <- l13[[c]]$p[l13[[c]]$age == ", i, "]")))
        
        # Text for max
        eval(parse(text=paste0("text_p <- c(text_p, 'p",i,"')"))) #vector p0, p1 ...
        
        if(i != ages[2]){
          eval(parse(text=paste0("text_l <- c(text_l, 'l",i,"')"))) #vector l0, l1 ...
          eval(parse(text=paste0("text_pi <- c(text_pi, 'pi",i,"')"))) #vector pi0, pi1 ...
        }
      }
    }
    
    eval(parse(text=paste0("l14 <- lapply (l13, function(x){ x %>% group_by(rect, ", time_first, ", length) %>% 
                           summarize_at(.vars = vars(", paste(paste(text_p, collapse = ", "), paste(text_l, collapse = ", "), 
                                                              paste(text_pi, collapse = ", "), paste(text_var, collapse = ", "),
                                                              paste(text_s_fish, collapse = ", "), sep = ", "), "), .funs = max)})"))) 
    
    eval(parse(text=paste0('ALKs$a',yr,' <- l14 ')))
}
  
  return(ALKs)
}
#-----------------------------------------------------------------------------------------------------#
#------------------------------------------ F_GRAPH --------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_graph_nostrat <- function(path_graph, data_graph, years, ages, tab_time, tab_levels, time_first){
  
  # Get time scale
  eval(parse(text=paste0("tab_time2 <- tab_time[!duplicated(tab_time$", time_first, "),]")))
  eval(parse(text=paste0("t <- unlist(subset(tab_time2, select = ", time_first, ", subset = is.na(", time_first, ") == F))")))
  
  # Path to save the graphs
  setwd(path_graph)
  
  # Loop to write p0, p1 ...
  text_p <- c()
  for (i in ages[1]:ages[2]){
    text_p <- c(text_p, paste('p', i, sep = ""))
  }
  text_p <- paste(text_p, collapse = ", ")
  
  for(y in years[1]:years[2]){
    for (i in 1:length(t)){
      eval(parse(text=paste0("png('graph_",y, "_", t[i], ".png', width =800, height = 800)")))
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      eval(parse(text=paste0("matplot(data_graph$a", y, "$length[data_graph$a", y, "$", time_first, " == t[i]], subset(data_graph$a", y, "[data_graph$a", y, "$", time_first, " == t[i],], select = c(", text_p, ")), type = 'b', lty = 'solid', lwd = 1.8,
                             pch = seq(1:ages[3]), col = rainbow(ages[3]), xlab = 'cm', ylab = 'p', cex = 0.7,
                             main = 'Year = ", y, ", ", time_first, " = ", t[i],"')")))
      axis(2,at = seq(0,1, by = 0.1))
      legend("topright", inset=c(-0.1,0), title = "Age", legend = c(ages[1]:ages[2]), col = rainbow(ages[3]), cex=1, pch = seq(1:ages[3]), box.lty = 0)
      dev.off()
    }
  }
}

f_graph_strat <- function(path_graph, data_graph, years, ages, tab_time, tab_levels, time_first){
  
  # Get time scale
  eval(parse(text=paste0("tab_time2 <- tab_time[!duplicated(tab_time$", time_first, "),]")))
  eval(parse(text=paste0("t <- unlist(subset(tab_time2, select = ", time_first, ", subset = is.na(", time_first, ") == F))")))
  
  # Path to save the graphs
  setwd(path_graph)
  
  # Loop to write p0, p1 ...
  text_p <- c()
  for (i in ages[1]:ages[2]){
    text_p <- c(text_p, paste('p', i, sep = ""))
  }
  text_p <- paste(text_p, collapse = ", ")
  
  eval(parse(text=paste0("nmodas <- length(data_graph$a", years[1], ")")))
  for(y in years[1]:years[2]){
    for(c in 1:nmodas){
      for (i in 1:length(t)){
        eval(parse(text=paste0("png('graph_",y, "_", t[i], "_", paste("names(data_graph$a", y, sep = ""),"[c].png', width =800, height = 800)")))
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        eval(parse(text=paste0("matplot(data_graph$a", y, "[[c]]$length[data_graph$a", y, "[[c]]$", time_first, " == t[i]], subset(data_graph$a", y, "[[c]][data_graph$a", y, "[[c]]$", time_first, " == t[i],], select = c(", text_p, ")), type = 'b', lty = 'solid', lwd = 1.8,
                               pch = seq(1:ages[3]), col = rainbow(ages[3]), xlab = 'cm', ylab = 'p', cex = 0.7,
                               main = 'Year = ", y, ", ", time_first, " = ", t[i],"')")))
        axis(2,at = seq(0,1, by = 0.1))
        legend("topright", inset=c(-0.1,0), title = "Age", legend = c(ages[1]:ages[2]), col = rainbow(ages[3]), cex=1, pch = seq(1:ages[3]), box.lty = 0)
        dev.off()
      }
    }
  }
}
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
  
  # ALK by level
  list_lvl <- list()
  for(l in levels[1]:levels[2]){
    eval(parse(text=paste0('list_lvl$lv',l,'<- f_levels_ALK(pre_key = pre_key,
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
  
  
  
  #return(list(ALK = ALK, pre_key = pre_key))
  return(ALK)
}

#-----------------------------------------------------------------------------------------------------#
#                                 LENGTH DISTRIBUTION                                                 #
#-----------------------------------------------------------------------------------------------------#

#Copied to own script

#-----------------------------------------------------------------------------------------------------#
#--------------------------------------- F_GRAPH1 ----------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_graph1 <- function(data_graph, path_graph){
  data_graph$lnl <- with(data_graph, log(length))
  png(paste(path_graph,'graph1.png', sep = ""), width =800, height = 800)
  plot(m_vgt ~ lnl, data_graph, xlab = "lnl", ylab = "Mean weight", pch = 3, cex = 0.8)
  dev.off()
}



#-----------------------------------------------------------------------------------------------------#
#                                 CATCH AT AGE                                                        #
#-----------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------#
#---------------------------------- F_CATCH_AGE_SANDEEL ----------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_catch_age_sandeel <- function(ld, path_catch_data, specie, time_first, time_group, space_group, tab_space, tab_faktcorr, years, ages, type, rtm){
  
  # Import catch data
  catch <- read.csv(path_catch_data)

  # Specific sandeel: renaming variable
  names(catch)[which(names(catch) == "square")] <- "rect"
  names(catch)[which(names(catch) == "intsq")] <- "rect"
  
  # Clearing variables
  catch <- subset(catch, is.na(year) == F)
  
  catch <- subset(catch, year == years[1] & !(is.na(ICESdiv)))
  
  # Correction
  catch_div <- merge(catch, tab_faktcorr, by = c("year"), all.x = T)
  catch_div$totIIIa[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IIIa"] <- catch_div$fakttotIIIa[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IIIa"]
  catch_div$DKIIIa[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IIIa"] <- catch_div$faktDKIIIa[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IIIa"]
  catch_div$Total[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IV"] <- catch_div$faktTotal[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IV"]
  catch_div$Denmark[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IV"] <- catch_div$faktDenmark[catch_div$year %in% tab_faktcorr$year &  catch_div$ICESdiv == "IV"]
  
  catch_div <- subset(catch_div, !(is.na(ton) == T & is.na(iton) == T))
  catch_div <- subset(catch_div, !(ton == 0 & is.na(iton) == T))
  catch_div <- subset(catch_div, !(ton == 0 & iton == 0))
  catch_div$iton[is.na(catch_div$iton) == T & catch_div$year > 1993] <- 0
  
  catch_div$ton[catch_div$ICESdiv == "IV" ] <- with(catch_div[catch_div$ICESdiv == "IV" ,] , ton*Denmark/dktondiv)
  catch_div$ton[catch_div$ICESdiv == "IIIa" ] <- with(catch_div[catch_div$ICESdiv == "IIIa" ,] , ton*DKIIIa/dktondiv)
  catch_div$iton[catch_div$ICESdiv == "IV" ] <- with(catch_div[catch_div$ICESdiv == "IV" ,] , iton*(Total-Denmark)/itondiv)
  catch_div$iton[catch_div$ICESdiv == "IIIa" ] <- with(catch_div[catch_div$ICESdiv == "IIIa" ,] , iton*(totIIIa-DKIIIa)/itondiv)
  catch_div$totton[catch_div$ICESdiv == "IV" ] <- with(catch_div[catch_div$ICESdiv == "IV" ,] , totton*Total/ttondiv)
  catch_div$totton[catch_div$ICESdiv == "IIIa"] <- with(catch_div[catch_div$ICESdiv == "IIIa" ,] , totton*totIIIa/ttondiv)
  
  catch_div$iton[is.infinite(catch_div$iton)] <- NA
  
  catch_div$iton[catch_div$ICESdiv == "IV" & catch_div$year <= 1993] <- with(catch_div[catch_div$ICESdiv == "IV" & catch_div$year <= 1993,] , totton*(Total - Denmark)/Total)
  catch_div$ton[catch_div$ICESdiv == "IV" & catch_div$year <= 1993] <- with(catch_div[catch_div$ICESdiv == "IV" & catch_div$year <= 1993,] , totton*Denmark/Total)
  catch_div$iton[catch_div$ICESdiv == "IIIa" & catch_div$year <= 1996] <- with(catch_div[catch_div$ICESdiv == "IIIa" & catch_div$year <= 1996,] , totton*(totIIIa - DKIIIa)/totIIIa)
  catch_div$ton[catch_div$ICESdiv == "IIIa" & catch_div$year <= 1993] <- with(catch_div[catch_div$ICESdiv == "IIIa" & catch_div$year <= 1993,] , totton*DKIIIa/totIIIa)
  catch_div$iton[catch_div$ICESdiv == "IIIa" & catch_div$year == 2009] <- with(catch_div[catch_div$ICESdiv == "IIIa" & catch_div$year == 2009,] , totton*(totIIIa - DKIIIa)/totIIIa)
  
  catch_div$iton[is.infinite(catch_div$iton)] <- NA
  
  catch_div$iton[is.na(catch_div$iton) == T] <- 0
  catch_div$totton <- with(catch_div, ton + iton)
  catch_div$dkton <- catch_div$ton
  catch_div$intton <- catch_div$iton
  
  
  catch_div <- subset(catch_div, select = -c(Area))
  
  
  # Prepare ld if more than one year
  if(years[1] != years[2]){
    ld <- do.call(rbind,ld)
  }else{
    ld <- ld[[1]]
  }
  
  # Merge catch data & ld 
  d1 <- merge(ld, catch_div, by = c("year", time_first, "rect"), all = T)
  #d1 <- merge(d1, tab_space[,c("rect", "Area")], by = "rect", all.x = T)
  
  
  if (rtm == "yes") {
    
    d1 <- subset(d1, month == 4)
    
  } else {
    
    d1 <- d1
  }
  
  
  # Select years, data with information
  d2a <- subset(d1, year %in% c(years[1]:years[2]) & is.na(Area) == F & is.na(dkton) == F & is.na(intton) == F) # 123
  d2b <- subset(d2a, !(dkton == 0 & (is.na(intton) == T | intton == 0)))
  d2 <- subset(d2b, is.na(age) == F)
  
  # Creation indicators ### cb10 sprat
  d2$dkn <- with(d2, dkton*n_pr_kg) 
  d2$dkwmw <- with(d2, dkn*mw)
  d2$inn <- with(d2, intton*n_pr_kg)
  d2$inwmw <- with(d2, inn*mw)
  d2$totn <- with(d2, totton*n_pr_kg)
  d2$totwmw <- with(d2, totn*mw)
  
  # Get sum by combination
  eval(parse(text=paste0(" d3 <- d2 %>% group_by(year, ", time_group , ", ", space_group, ", age) %>% 
                         summarize_at(.vars = vars(dkn, dkwmw, inn, inwmw, totn, totwmw, n_samples, dkton, intton, totton), .funs = sum)")))
  
  # Creation indicators
  d3$dkmw <- with(d3, dkwmw/dkn)
  d3$inmw <- with(d3, inwmw/inn)
  d3$totmw <- with(d3, totwmw/totn)
  
  # Delete vars
  d3 <- subset(d3, select = -c(dkwmw, inwmw, totwmw))
  
  #Transform Nan is na
  d3$dkmw[is.nan(d3$dkmw) == T] <- NA
  d3$inmw[is.nan(d3$inmw) == T] <- NA
  d3$totmw[is.nan(d3$totmw) == T] <- NA
  
  # Delete area duplicates
  eval(parse(text=paste0("d4 <- d3[!duplicated(d3[,c('", space_group, "', 'age')]),c('year', 'age', '", space_group, "', '", time_group, "')]")))
  d4 <- subset(d4, is.na(age) == F)
  
  # Get all combinations
  eval(parse(text=paste0("d4 <- d4[rep(1:nrow(d4), each = length(unique(time_table$", time_group, "))),]")))
  eval(parse(text=paste0("d4$", time_group, " <- rep(unique(time_table$", time_group, "), nrow(d4)/length(unique(time_table$", time_group, ")))")))
  
  # Get the indicators
  eval(parse(text=paste0("d5 <- merge(d4, d3, all.x = T, by = c('year', '", time_group, "', '", space_group, "', 'age'))")))
  
  d5$dkn[is.na(d5$dkn) == T] <- 0 
  d5$inn[is.na(d5$inn) == T] <- 0 
  d5$totn[is.na(d5$totn) == T] <- 0 
  d5$dkton[is.na(d5$dkton) == T] <- 0 
  d5$intton[is.na(d5$intton) == T] <- 0 
  d5$totton[is.na(d5$totton) == T] <- 0 
  
  # Get indicators mean for correction, we compute mean by year, to impute later
  eval(parse(text=paste0("d6 <- d5 %>% group_by(", time_group , ", ", space_group, ", age) %>% summarise(mmw = mean(totmw), .groups = 'drop')")))
  
  # Correction
  eval(parse(text=paste0("d7 <- merge(d5, d6, all.x = T, by = c('", time_group, "', '", space_group, "', 'age'))")))
  
  d7$dkmw[is.na(d7$dkmw) == T] <- 0
  d7$inmw[is.na(d7$inmw) == T] <- 0
  d7$totmw[is.na(d7$totmw) == T] <- 0
  d7$totmw[d7$n_samples < 5 & d7$year >= 1993 & is.na(d7$n_samples) == F ] <- d7$mmw[d7$n_samples < 5 & d7$year >= 1993 & is.na(d7$n_samples) == F ]
  
  # Correction of mw
  d8 <- d7
  d8$n_samples[is.na(d8$n_samples) == T] <- 0
  
  eval(parse(text=paste0("sop_corr <- d8[!duplicated(d8[,c('year', 'n_samples', '", time_group, "', '", space_group, "' )]),
                         c('year', 'n_samples', '", time_group, "', '", space_group, "' )]")))
  
  sop_corr$sopcorr <- 0
  sop_corr$sop <- 0
  
  deno <- 0 # denominateur sop
  comb <- 0 # counter for each combination
  
  for (i in 1:nrow(d8)){
    if(d8$age[i] == ages[1]){
      comb <- comb + 1
      sop <- 0
    }
    sop <- sop + with(d8[i,], totn*totmw)
    if(d8$age[i] == ages[2]){
      sop_corr$sopcorr[comb] <- d8$totton[i]/sop
      sop_corr$sop[comb] <- sop
    }
  }  
  
  eval(parse(text=paste0("d8 <- merge(d8, sop_corr, by = c('year', 'n_samples', '", time_group, "', '", space_group, "'), all.x = T)")))
  d8$sopcorr[is.na(d8$sopcorr) == T | d8$totton == 0] <- 1 
  d8$n <- d8$totn
  d8$mw <- with(d8, sopcorr*totmw)
  d8$ton <- d8$totton
  
  # Final data
  eval(parse(text=paste0("tot_catch <- subset(d8, select = c(year, ", time_group, ", ", space_group, ", age, n, mw, ton, n_samples, sop))")))
  
  if (rtm == "yes") {
    
    tot_catch <- subset(tot_catch, month == 4)
    
  } else {
    
    tot_catch <- tot_catch
  }
}

#-----------------------------------------------------------------------------------------------------#
#---------------------------------- F_CATCH_AGE_SPRAT ------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#

f_catch_age_sprat <- function(ld, path_catch_data, specie, time_first, time_group, space_group, tab_space, tab_faktcorr, years, ages){
  
  # Import catch data
  catch <- read.csv(path_catch_data)
  catch <- subset(catch, year %in% years)
  
  # Specific sandeel: renaming variable
  names(catch)[which(names(catch) == "intsq")] <- "rect"
  names(catch)[which(names(catch) == "square")] <- "rect"
  
  # Create the faktors variables
  catch_div <- merge(catch, tab_space[,c("rect", "area3")], by = "rect", all.x = T)
  catch_div$area3[is.na(catch_div$area3) == T ] <- "IV"
  catch_div <- catch_div %>% group_by(year, area3) %>% summarise(ton = sum(ton), .groups = 'drop')
  catch_div <- merge(catch_div, tab_faktcorr, by = "year")
  faktorIV <- with(catch_div[catch_div$area3 == "IV",] , 1000*IV/ton)
  faktorIIIa <- with(catch_div[catch_div$area3 == "IIIa",] , 1000*IIIa/ton)
  
  # Prepare ld if more than one year
  if(years[1] != years[2]){
    ld <- do.call(rbind,ld)
  }else{
    ld <- ld[[1]]
  }
  
  # Merge catch data & mnpk  = a4 sas and merge to get faktor
  d1 <- merge(ld, catch, by = c("year", time_first, "rect"), all = T)
  d1$area3[is.na(d1$area3) == T ] <- "IV"
  
  # Select years, data with information
  d2 <- subset(d1, year %in% c(years[1]:years[2]) & is.na(age) == F)
  d2$ton[d2$area3 == "IV"] <- with(d2[d2$area3 == "IV",], faktorIV*ton)
  d2$ton[d2$area3 == "IIIa"] <- with(d2[d2$area3 == "IIIa",], faktorIIIa*ton)
  
  ####################
  # Creation indicators ### cb10 sprat
  d2$n <- with(d2, ton*n_pr_kg) 
  d2$wmw <- with(d2, n*mw)
  d2$wml <- with(d2, n*ml)
  
  d2$n[is.na(d2$n) == T] <- 0
  d2$wmw[is.na(d2$wmw) == T] <- 0
  d2$wml[is.na(d2$wml) == T] <- 0
  d2$ton[is.na(d2$ton) == T] <- 0
  
  # Get sum by combination
  eval(parse(text=paste0(" d3 <- d2 %>% group_by(year, ", time_group , ", ", space_group, ", age) %>% 
                         summarize_at(.vars = vars(n, wmw, wml, n_samples, ton), .funs = sum)")))
  
  # Creation indicators
  d3$mw <- with(d3, wmw/n)
  d3$ml <- with(d3, wml/n)
  
  # Delete vars
  d3 <- subset(d3, select = -c(wmw, wml))
  
  #Transform Nan is na
  d3$mw[is.nan(d3$mw) == T] <- NA
  d3$ml[is.nan(d3$ml) == T] <- NA
  
  # Delete area duplicates
  eval(parse(text=paste0("d4 <- d3[!duplicated(d3[,c('", space_group, "', 'age')]),c('year', 'age', '", space_group, "', '", time_group, "')]")))
  d4 <- subset(d4, is.na(age) == F)
  
  # Get all combinations
  eval(parse(text=paste0("d4 <- d4[rep(1:nrow(d4), each = length(unique(time_table$", time_group, "))),]")))
  eval(parse(text=paste0("d4$", time_group, " <- rep(unique(time_table$", time_group, "), nrow(d4)/length(unique(time_table$", time_group, ")))")))
  
  # Get the indicators # Actually it is the same table as d3, but in case one strata is missing
  eval(parse(text=paste0("d5 <- merge(d4, d3, all.x = T, by = c('year', '", time_group, "', '", space_group, "', 'age'))")))
  
  d5$mw[is.na(d5$mw) == T] <- 0 
  d5$ml[is.na(d5$ml) == T] <- 0 
  d5$ton[is.na(d5$ton) == T] <- 0 
  
  # Get indicators mean for correction, we compute mean by year, to impute later
  eval(parse(text=paste0("d6 <- d5 %>% group_by(", time_group , ", ", space_group, ", age) %>% summarise(mmw = mean(mw), .groups = 'drop')")))
  
  # Correction
  eval(parse(text=paste0("d7 <- merge(d5, d6, all.x = T, by = c('", time_group, "', '", space_group, "', 'age'))")))
  
  d7$mw[is.na(d7$mw) == T] <- d7$mmw[is.na(d7$mw) == T]
  d7$mw[d7$n_samples < 5 & is.na(d7$n_samples) == F ] <- d7$mmw[d7$n_samples < 5 & is.na(d7$n_samples) == F ]
  
  # Correction of mw
  d8 <- d7
  d8$n_samples[is.na(d8$n_samples) == T] <- 0
  
  eval(parse(text=paste0("sop_corr <- d8[!duplicated(d8[,c('year', 'n_samples', '", time_group, "', '", space_group, "' )]),
                         c('year', 'n_samples', '", time_group, "', '", space_group, "' )]")))
  deno <- 0 # denominateur sop
  comb <- 0 # counter for each combination
  
  for (i in 1:nrow(d8)){
    if(d8$age[i] == ages[1]){sop <- 0; comb <- comb + 1}
    if(d8$age[i] == ages[2]){
      sop <- sop + with(d8[i,], n*mw)
      sop_corr$sopcorr[comb] <- d8$n[i]/sop
      sop_corr$sop[comb] <- sop
    }
  }  
  eval(parse(text=paste0("d8 <- merge(d8, sop_corr, by = c('year', 'n_samples', '", time_group, "', '", space_group, "'), all.x = T)")))
  d8$sopcorr[is.na(d8$sopcorr) == T | d8$ton == 0] <- 1 
  d8$mw <- with(d8, sopcorr*mw)
  
  # Final data
  eval(parse(text=paste0("tot_catch <- subset(d8, select = c(year, ", time_group, ", ", space_group, ", age, n, mw, ton, n_samples, sop))")))
  
}


