
prepare_data_for_est <- function(path_data, data_rca, data_rhh, tab_space, tab_time, time_first, space_first, specie, type_length, ages, years, recruit_month,
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
