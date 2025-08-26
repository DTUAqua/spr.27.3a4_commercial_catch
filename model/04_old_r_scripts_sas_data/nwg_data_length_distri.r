#-------------------------------------------------------------------#
# LENGTH DISTRIBUTION
#
# IMPORT NWG DATA - SANDEEL
#
#-------------------------------------------------------------------#

library(haven)

# Import
nwg_sandeel <- read_sas("Q:/mynd/Assessement_discard_and_the_like/assessment_scripts/HAWG_sandeel/2020/norwegian_length_freq.sas7bdat")

# Rename
names(nwg_sandeel)[which(names(nwg_sandeel) == "intsq")] <- "rect"
names(nwg_sandeel)[which(names(nwg_sandeel) == "aar")] <- "year"
names(nwg_sandeel)[which(names(nwg_sandeel) == "scm")] <- "length"
names(nwg_sandeel)[which(names(nwg_sandeel) == "antal")] <- "number"
names(nwg_sandeel)[which(names(nwg_sandeel) == "st_antal")] <- "subSampNo"

nwg_sandeel$month <- as.integer(nwg_sandeel$month)
nwg_sandeel$day <- as.integer(nwg_sandeel$day)


nwg_sandeel$staNum <- paste(nwg_sandeel$stat, nwg_sandeel$rect, nwg_sandeel$month, nwg_sandeel$day, sep = "-")

# Add weight
nwg_sandeel$vgt <- with(nwg_sandeel, number * m_vgt)


# Get similar columns as danish data
nwg_sandeel <- nwg_sandeel[,-c(2,7,8,10)]
nwg_sandeel[,c("sampType", "landCtry", "vslFlgCtry", "proj", "trpCode", "quarter")] <- NA
