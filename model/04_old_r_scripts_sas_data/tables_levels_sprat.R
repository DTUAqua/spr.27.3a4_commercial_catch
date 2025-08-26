#---------------------------------------#
# Table sprat
#---------------------------------------#

# # # # 20/03/2018 # # # # 

# Levels table

level <- c(1,2,3,4,5)
nb_var <- c(rep(3,4),2)
var1 <- c("area1", "area2", rep("area3", 3))
var2 <- rep("year", 5)
var3 <- c(rep("quarter",3),"halfyear","")

levels_table <- data.frame(level, nb_var, var1, var2, var3)

levels_table$var1 <- as.character(levels_table$var1)
levels_table$var2 <- as.character(levels_table$var2)
levels_table$var3 <- as.character(levels_table$var3)

rm(level, nb_var, var1, var2, var3)

# Time table
month <- c(1:12, NA)
quarter <- c(rep(1,3),rep(2,3), rep(3, 3), rep(4, 3), NA)
halfyear <- c(rep(1,6),rep(2,6),99)
time_table <- data.frame(month, quarter, halfyear)
rm(month, quarter, halfyear)

# Space table

space_table <- list_areas
names(space_table)[4] <- "rect"
names(space_table)[2] <- "area1"

space_table$rect <- as.character(space_table$rect)
space_table$area1 <- as.character(space_table$area1)
space_table$area2 <- as.character(space_table$area2)
space_table$area3 <- as.character(space_table$area3)
missing <- c("", rep("999", 3))
space_table <- rbind(space_table, missing)

