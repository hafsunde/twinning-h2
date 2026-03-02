# FINDINGS ALL SIBLINGS IN THE POPULATION
# Script author: HANS FREDRIK SUNDE (hfsu@fhi.no)
library(data.table)
setwd("../")


#############################################################
######     IMPORT SAMPLE FOR WHICH TO FIND SIBLINGS   #######
#############################################################

mothers <- readRDS("01_data/mothers.rds")
fathers <- readRDS("01_data/fathers.rds")


################################################
######     IMPORT AND PREPARE RAW DATA   #######
################################################

## load a registry file, which needs to have columns for: person's ID, mother's ID, father's ID
# Should include the entire population
reg <- fread("N:/durable/Data14/NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv", 
             select = c("mor_lnr_k2_", "far_lnr_k2_","w19_1011_lnr_k2_", "kjoenn", "fodeland","invkat"))

setnames(reg, 
         c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_", "kjoenn", "fodeland","invkat"),
         c("index_id", "mother_id", "father_id","sex","birth_country","immigration_category"))
reg <- reg[!is.na(index_id)] # In my data, newborns (birth_year > 2019) do not have ID codes
setkey(reg, index_id)



# Removes individuals whose mother/father doesn't match the indicated sex
# (This would imply adoption or other forms of entry errors)
# Could also be later legal sex change
reg[reg, on = c("mother_id" = "index_id"), mother_s := i.sex]
reg <- reg[is.na(mother_id) | mother_s == 2]

reg[reg, on = c("father_id" = "index_id"), father_s := i.sex]
reg <- reg[is.na(father_id) | father_s == 1]
reg[,c("father_s", "mother_s") :=NULL]


################################################
######        SUBSET TO SAMPLE           #######
################################################

# subsets to rows where the person is in the sample AND both parents are known 
reg <- reg[(index_id %in% mothers$index_id | index_id %in% fathers$index_id) & !is.na(mother_id) & !is.na(father_id)]

# Number of mothers with known parents:
nrow(reg[index_id %in% mothers$index_id])

# Number of fathers with known parents:
nrow(reg[index_id %in% fathers$index_id])






################################################
######    IDENTIFY LIKELY ADOPTEES       #######
################################################

# Find adoptees (born to Norwegian parents abroad in countries with adoptee-agreements )
# Immigration cateogry "G" is foreign-born to Norwegian Parents
reg[, adopt:=0]
reg[immigration_category == "G" & birth_country %in% c(484, 492, 730, 725, 359, 444, 432, 152, 393, 
                                                       113, 568, 428, 289, 760, 424, 575, 715, 246, 241), adopt := 1] 



################################################
######            FIND SIBLINGS          #######
################################################

########## FIND MATERNAL SIBLINGS
shared_mothers <- reg[,.(index_id,N=.N),by=mother_id][N>1]
shared_mothers[,N:=NULL]
shared_mothers <- shared_mothers[sample(1:nrow(shared_mothers))]      #Randomize order so that sibling order is random
shared_mothers[,id:=.I] # Creates numerical ID which helps remove duplicate pairs (e.g., self-matches and 1-2 == 2-1)



# Perform an efficient within-group self-join using 'mother_id'
# This creates all possible pairwise combinations of observations within the same group
# `allow.cartesian = TRUE` allows large join results when groups are big
maternal_pairs <- shared_mothers[shared_mothers, on = .(mother_id), allow.cartesian = TRUE][
    id < i.id, # Keep only unique pairs (avoid self-pairs and duplicates)
    .(i_id = index_id, j_id = i.index_id)
  ]


maternal_pairs[,':='(
  pair_id = paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
               pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_"), 
  r=.25,
  lineage = "m"
)]




########## FIND PATERNAL SIBLINGS

shared_fathers <- reg[,.(index_id,N=.N),by=father_id][N>1]
shared_fathers[,N:=NULL]
shared_fathers <- shared_fathers[sample(1:nrow(shared_fathers))]      #Randomize order so that sibling order is random
shared_fathers[,id:=.I] # Creates numerical ID which helps remove duplicate pairs (e.g., self-matches and 1-2 == 2-1)



# Perform an efficient within-group self-join using 'mother_id'
# This creates all possible pairwise combinations of observations within the same group
# `allow.cartesian = TRUE` allows large join results when groups are big
paternal_pairs <- shared_fathers[shared_fathers, on = .(father_id), allow.cartesian = TRUE][
  id < i.id, # Keep only unique pairs (avoid self-pairs and duplicates)
  .(i_id = index_id, j_id = i.index_id)
]

paternal_pairs[,':='(
  pair_id = paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_"), 
  r=.25,
  lineage = "p"
)]



# FIND FULL SIBLINGS (i.e, both maternal and paternal siblings)
all_siblings <- rbindlist(list(maternal_pairs,paternal_pairs),use.names=T)
rm(paternal_pairs, maternal_pairs, shared_mothers, shared_fathers)

all_siblings[,N:=.N,pair_id]
all_siblings[N==2,':='(r=.5,lineage="b")]
all_siblings <- all_siblings[,.SD[1],pair_id]
all_siblings[,N:=NULL]
all_siblings[r==.5, type := "full sibs"]
all_siblings[r==.25,type := "half sibs"]
gc()





#######################################################
######                 FIND TWINS               #######
#######################################################


## Import twins
twins <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/all_twins_04112024.csv", na.strings = "")
twins <- twins[!is.na(index_id)]
twins <- twins[sample(1:nrow(twins))]      #Randomize order so that sibling order is random

# Creates pairwise data
twins[,id:=.I] # Creates numerical ID which helps remove duplicate pairs (e.g., self-matches and 1-2 == 2-1)
twin_pairs <- twins[twins, on = .(pregnancy_id), allow.cartesian = TRUE][id < i.id, .(i_id = index_id, j_id = i.index_id, OS_t = opposite_sex_twin, Zygo = Zygo)]


twin_pairs[,pair_id := paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                            pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_")]


twin_pairs[is.na(Zygo),Zygo := 0] # Unknown Zygosity is assigned cateogry 0
table(twin_pairs$Zygo)

twin_pairs[OS_t==T, Zygo:=3] # OS twins must be DZ
twin_pairs[, Zygo := as.character(factor(Zygo, levels = c(0, 1, 2, 3), labels = c("UZ", "MZ", "DZ", "DZ")))]
table(twin_pairs$Zygo)


all_siblings[twin_pairs, on = "pair_id", twin := i.Zygo]
all_siblings[!is.na(twin), type:="twins"]
all_siblings[twin=="MZ" & r==.5, r := 1]
all_siblings[twin=="UZ" & r==.5, r := .75]
all_siblings[,pair_id :=NULL]



#######################################################
######        FIX INCONSISTENCIES               #######
#######################################################

# Some are marked as both twins and half siblings. 
# They are most likely legally adopted by a different father, 
# and still genetic full siblings. Changing the relatedness accordingly:

all_siblings[twin == "MZ" & r==.25, r := 1]
all_siblings[twin == "DZ" & r==.25, r := .5]
all_siblings[twin == "UZ" & r==.25, r := .75]




#######################################################
######             FIND ADOPTIVES               #######
#######################################################


# Mark adoptives
all_siblings <- all_siblings[reg[,.(index_id, adopt)], on = c("i_id" = "index_id"),nomatch=0, allow.cartesian = T]
all_siblings <- all_siblings[reg[,.(index_id, adopt)], on = c("j_id" = "index_id"),nomatch=0, allow.cartesian = T]
all_siblings[adopt == 1 | i.adopt == 1,adopt := 1]
all_siblings[,c("i.adopt") :=NULL]

# Some are both twins and adopted. I've checked a few pairs, and it seems that
# in most circumstances, they are adopted from the same country.
# Most likely, they are adopted twins (i.e., biological twins)
# (They could also be marked as being born on the same day due to administrative errors,
# for example because they used date of entry as date of birth...)
# I have not investigate this subgroup (179 pairs in the entire population) thoroughly, but I'm 
# removing the adopt-indicator
all_siblings[type=="twins" & adopt==1, adopt:=0]



# Change relatedness for adoptees:
all_siblings[adopt==1, r:=0]


# Clean up (for memother_idy): 
rm(twin_pairs, twins); gc()





#######################################################
######        ASSIGN SEX CONCORDANCE            #######
#######################################################

# Both fathers and mothers are included in the subsample we found relatives for, but we are really just interested in the sisters

all_siblings[reg[,.(index_id, sex)], on = c("i_id" = "index_id"),i_sex := i.sex]
all_siblings[reg[,.(index_id, sex)], on = c("j_id" = "index_id"),j_sex := i.sex]


all_siblings[i_sex == 1 & j_sex == 1, sex := 1]
all_siblings[i_sex == 2 & j_sex == 2, sex := 2]
all_siblings[i_sex == 1 & j_sex == 2, sex := 0]
all_siblings[i_sex == 2 & j_sex == 1, sex := 0]
all_siblings[, c("i_sex", "j_sex") := NULL]



saveRDS(all_siblings, "01_data/all_siblings.rds")
# 



