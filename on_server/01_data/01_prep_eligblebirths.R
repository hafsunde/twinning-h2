#############################################################
#                                                           #
#                   PREPARE SAMPLE                          #
#                                                           #
#############################################################
#  This script gathers all data into a practical data file  #
#                         #
#############################################################
#                         #
#############################################################

setwd("../")
library(data.table)

# Set the observation period where zygosity info is available
min.year <- 1966
max.year <- 1992


################################################
######     IMPORT AND PREPARE RAW DATA   #######
################################################


# Imports Basic information / Complete Population Register  
population <- fread("N:/durable/Data14/NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv",
                    select = c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_","foedselsaar", "kjoenn","invkat"))

# Change names of variables
setnames(population, 
         c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_","foedselsaar", "kjoenn","invkat"),
         c("index_id", "mother_id", "father_id","birth_year", "sex", "immigration_category"))


# Import pregnancy_id's and twin info
pregnancies <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/pregnancy_ids.csv",na.strings = "")
twins <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/all_twins_04112024.csv",na.strings = "")

# Create indicator variables which we'll use later
twins[,':='(DZ = Zygo %in% c(2,3) | opposite_sex_twin)]
twins[is.na(Zygo) & !opposite_sex_twin, DZ := NA]



################################################
######        FINDING MULTIPLES          #######
################################################


# Remove individuals with unknown mothers, first-generation immigrants, and born outside our study period
#births <- population[!is.na(mother_id) & immigration_category != "B" & between(birth_year, 1966,1992)]
births <- population[!is.na(mother_id) & immigration_category != "B"]

# Attach pregnancy id
births[pregnancies, on = c("index_id", "mother_id", "birth_year"), pregnancy_id := i.pregnancy_id]
# prenancy id would normally just need index_id, but I don't have index_id for the most recent births. 
# Hence why mother_id+birth_year is also used

births[,c("immigration_category", "sex") := NULL]

# Attach info from twin data
births[twins[,.SD[1], pregnancy_id], on = "pregnancy_id",':='(opposite_sex_twin = i.opposite_sex_twin, 
                                                                      multiples_count = i.multiples_count,
                                                                      DZ = i.DZ, MZ = i.Zygo==1)]

# Fill info for non-multiples
births[is.na(multiples_count), ':='( multiples_count = 1,opposite_sex_twin =F, DZ=F, MZ=F)]


# Attach maternal age
births[population, on =c("mother_id" = "index_id"), maternal_age := birth_year - i.birth_year]

# Collapse twins into one row
births <- births[,.SD[1], pregnancy_id]


# Limit sample to births with known, eligble parents within the observation period
births <- births[!is.na(mother_id) & !is.na(father_id) & 
                   between(birth_year,min.year,max.year) & 
                   between(birth_year-maternal_age,min.year-20,max.year-35) & 
                   maternal_age >= 20 & maternal_age <= 35,]



# Double check sex of parent matches role (some 80 births in this period are then excluded)
births[population[!is.na(index_id),.(index_id, sex)], on = c("mother_id" = "index_id"),mother_s := i.sex]
births[population[!is.na(index_id),.(index_id, sex)], on = c("father_id" = "index_id"),father_s := i.sex]
births <- births[mother_s == 2 & father_s == 1]
births[,c("father_s", "mother_s") :=NULL]

# Calculates % known zyg among SS twins
births[multiples_count > 1 & !opposite_sex_twin, .(knon_zyg = mean(!is.na(MZ)))]


saveRDS(births, "01_data/births.rds")

