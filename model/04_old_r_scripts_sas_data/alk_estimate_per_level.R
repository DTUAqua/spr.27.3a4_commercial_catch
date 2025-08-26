
estimate_alk_per_level <- function(pre_key, lvl, ages, tab_levels, vars_r, list_text, levels, space_first, uncertainty, min_sfish){
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
  
  # aglg1 <- pre_key %>%
  #   group_by_at(vars(one_of(c(trimws(unlist(strsplit(vars, ","))), "length")))) %>%
  #   summarize_at(vars(one_of(var_ns)), sum)
  
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
