#-------------------------------------------------------------------#
# LENGTH DISTRIBUTION LAUNCHING SPRAT
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#   1. LOAD functions
#-------------------------------------------------------------------#
source("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/functions/f_all_functions.R")

#-------------------------------------------------------------------#
#   2. CREATION TABLES
#-------------------------------------------------------------------#

# Get Tables levels, space, time  
list_areas <- read.csv("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/sprat_areas.csv")
list_areas <- select()
source("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/tables_levels_sprat.r")

#-------------------------------------------------------------------#
#   3. GET ALK
#-------------------------------------------------------------------#

# NWG data sprat
rca_nwg <- read.csv("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/2020/alk_samples_foreign_t_2019_2020.csv")

# ALK function
ALK_2019 <- f_compute_ALK(path_data = "Q:/dfad/users/kibi/data/RDB/RDB_in_csv/",
                                          data_rca = "rCA_2019.csv",
                                          data_rhh = "rHH_2019.csv",
                                          tab_levels = levels_table,
                                          tab_space = space_table,
                                          tab_time = time_table,
                                          space_first = "area1",
                                          time_first = "quarter",
                                          specie = 'Sprattus sprattus',
                                          ages = c(0,4),
                                          years = c(2019, 2019),
                                          uncertainty = 0.5,
                                          min_sfish = 50,
                                          recruit_month = 3,
                                          type_length = "scm" ,
                                          length_pred = c(1,20),
                                          length_obs = c(3,20),
                                          stratif = c(),
                                          return_graph = "Yes",
                                          path_graph = "Q:\\dfad\\users\\anbes\\home\\Results\\ALK Graph sprat\\" )

saveRDS(ALK_2019$a2019, paste0("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/output/2020/ALK_2019_", Sys.Date() ,".rds"))

# path_data = "Q:/dfad/users/kibi/data/RDB/RDB_in_csv/"
# data_rca = "rCA_2019.csv"
# data_rhh = "rHH_2019.csv"
# tab_levels = levels_table
# tab_space = space_table
# tab_time = time_table
# space_first = "area1"
# time_first = "quarter"
# specie = 'Sprattus sprattus'
# ages = c(0,4)
# years = c(2019, 2019)
# uncertainty = 0.5
# min_sfish = 50
# recruit_month = 3
# type_length = "scm"
# length_pred = c(1,20)
# length_obs = c(3,30)
# stratif = c()
# return_graph = "Yes"
# path_graph = "Q:\\dfad\\users\\anbes\\home\\Results\\ALK Graph sprat\\"

#-------------------------------------------------------------------#
#   4. LENGTH DISTRIBUTION
#-------------------------------------------------------------------#

# NWG data sprat
# nwg_sprat <- read.csv("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/2020/ld_samples_foreign_2019_2020.csv")
source("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/functions/f_length_distri_sprat.R")
# Function
length_distri_sprat <- f_length_distri(path_data = "Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/2020/",
                                         data_all = "ld_samples_input_2019_2020.csv",
                                         # data_rhh = "rHH_2019.csv",
                                         # data_rhl = "rHL_2019.csv",
                                         # data_rsl = "rSL_2019.csv",
                                         # data_nwg = nwg_sprat,
                                         data_ALK = ALK_2019,
                                         path_graph = "Q:\\dfad\\users\\anbes\\home\\Results\\Length distribution sprat\\Sprat 2016 RDB\\",
                                         tab_levels = levels_table,
                                         tab_time = time_table,
                                         tab_space = space_table,
                                         specie = "Sprattus sprattus",
                                         species = "Sprattus sprattus",
                                         years = c(2019, 2019),
                                         type_length = "scm",
                                         ages = c(0,4),
                                         space_first = "area1", 
                                         time_first = "quarter",
                                         min_nsamples = 5)

test <- length_distri_sprat$a2019
library(ggplot2)
ggplot(test, aes(x = age, y = n_pr_kg, group = age)) + geom_boxplot()


ld <- read.csv("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/2020/ld_samples_input_2019_2020.csv")

ggplot(ld, aes(x = length, y = number, col = staNum)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none")


# path_data = "Q:/dfad/users/kibi/data/RDB/RDB_in_csv/"
# data_rca = "rCA_2020.csv"
# data_rhh = "rHH_2020.csv"
# data_rhl = "rHL_2020.csv"
# data_rsl = "rSL_2020.csv"
# data_nwg = nwg_sprat
# data_ALK = ALK_2020
# path_graph = "Q:\\dfad\\users\\anbes\\home\\Results\\Length distribution sprat\\Sprat 2016 RDB\\"
# tab_levels = levels_table
# tab_time = time_table
# tab_space = space_table
# species = c("Sprattus sprattus")
# specie = "Sprattus sprattus"
# years = c(2020, 2020)
# type_length = "scm"
# ages = c(0,4)
# space_first = "area1"
# time_first = "quarter"
# min_nsamples = 5


#-------------------------------------------------------------------#
#   5. CATCH AT AGE
#-------------------------------------------------------------------#

# time_table <- subset(time_table, is.na(month) == F) # Delete when missing month
# time_table <- time_table[!duplicated(time_table$quarter),-1]

#------------------------------------#
#     CREATION TABLE WITH FACTORS TO CORRECT THE WEIGHT
#------------------------------------#

year <- c(2019)
IV <- c(147.614)
IIIa <- c(0)
tab_faktcorr <- data.frame(year, IV, IIIa)

catch_at_age_2019 <- f_catch_age_sprat(ld = length_distri_sprat,
                                    path_catch_data = "Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/2020/sprat_landings_year_quarter_square_2019_2020.csv",
                                 specie = "Sprattus sprattus",
                                 time_first = "quarter",
                                 time_group = "quarter",
                                 space_group = "area3",
                                 tab_space = space_table,
                                 tab_faktcorr = tab_faktcorr,
                                 years = c(2019,2019),
                                 ages = c(0,4)
                        )

names(catch_at_age_2019)
mw <- select(filter(catch_at_age_2020, halfyear != 99), year, halfyear, Area, age, mw)
library(tidyr)
mw_t <- rename(spread(mw, key = age, value = mw, sep = ""), mw0 = age0, mw1 = age1, mw2 = age2, mw3 = age3, mw4 = age4)

n <- select(filter(catch_at_age_2020, halfyear != 99), year, halfyear, Area, age, n)
n_t <- rename(spread(n, key = age, value = n, sep = ""), n0 = age0, n1 = age1, n2 = age2, n3 = age3, n4 = age4)

final <- merge(mw_t, n_t)


ld = length_distri_sprat
path_catch_data = "Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sprat/input/2020/sprat_landings_year_quarter_square_2019_2020.csv"
specie = "Sprattus sprattus"
time_first = "quarter"
time_group = "quarter"
space_group = "area3"
tab_space = space_table
tab_faktcorr = tab_faktcorr
years = c(2019,2019)
ages = c(0,4)
