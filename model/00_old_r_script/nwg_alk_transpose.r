## ------------------------------------------------------- ##
## -------------------- To run before -------------------- ##

ages <- c(0,4)

## ------------------------------------------------------- ##
## --------- Transposing Norwegian data 2017 ------------- ##

nwg <- read.csv("M:/Tobis/Tobis_assessment/SMS_2020/Data/catch/norwegian_alk.csv", header = T)

nwg2 <-   nwg[rep(1:nrow(nwg), each = (ages[2]-ages[1] + 1)),]
nwg2$age <- seq(ages[1],ages[2],1)

# Fill n

for(i in ages[1]:ages[2]){
  eval(parse(text=paste0('nwg2$n_age[nwg2$age == ', i,'] <- nwg2$a', i, '[nwg2$age == ', i,']')))
}

nwg3 <- subset(nwg2, select = -c(a0, a1, a2, a3, a4, stat, Total_number_length_class, Total_weight_length_class, Prop_aged, Mean_weight, b, atot,
                                 PP_ar_tx, stat_unique, update_date),n_age > 0)

nwg4 <- nwg3[rep(seq_len(dim(nwg3)[1]),nwg3$n_age), ]

names(nwg4)
names(nwg4)[1] <- "rect"
names(nwg4)[2] <- "length"
names(nwg4)[5] <- "year"
nwg4$rect <- as.character(nwg4$rect)
nwg4$landCtry <- "NOR"
# nwg4$date <- paste(nwg4$year, ifelse(nwg4$month > 9, nwg4$month, paste('0', nwg4$month, sep = "")), nwg4$day, sep = "-")


## ------------------------------------------------------- ##
## --------- Norwegian in RDB format --------------------- ##
rca_nwg <- subset(nwg4,select = -c(n_age))

rm(nwg, nwg2, nwg3, nwg4)
