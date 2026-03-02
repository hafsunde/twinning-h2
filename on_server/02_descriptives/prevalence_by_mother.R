library(data.table)
setwd("../")
mothers <- readRDS("01_data/mothers.rds")


mothers[,.(mean_birth = mean(n_birth),sd_birth = sd(n_birth), mean_child = mean(n_children),sd_child = sd(n_children))]

nrow(mothers[TW==T]); nrow(mothers[TW==T ]) / nrow(mothers)*100 # TW births
nrow(mothers[OS==T]); nrow(mothers[OS==T ]) / nrow(mothers)*100 # OS births
nrow(mothers[DZ==T]); nrow(mothers[DZ==T ]) / nrow(mothers)*100 # DZ births
nrow(mothers[MZ==T]); nrow(mothers[MZ==T ]) / nrow(mothers)*100 # MZ births 
nrow(mothers[is.na(DZ)]); nrow(mothers[is.na(DZ) ]) / nrow(mothers)*100 # UZ births



# Number of higher order multiples
nrow(mothers[multiples_count>2]) 
nrow(mothers[multiples_count>2]) / nrow(mothers)*100
nrow(mothers[multiples_count>2]) / nrow(mothers[multiples_count>1])*100
mean(mothers[multiples_count>2,opposite_sex_twin])


# Subsample with sisters:
mothers_2 <- readRDS("01_data/mothers_Wsib.rds")



########
# PREVALENCE

nrow(mothers_2[TW==T]) / nrow(mothers_2)*100
nrow(mothers_2[OS==T]) / nrow(mothers_2)*100
nrow(mothers_2[DZ==T]) / nrow(mothers_2)*100
nrow(mothers_2[MZ==T]) / nrow(mothers_2)*100


# For those with sisters
nrow(mothers_2[TW==T & !is.na(sib_TW)]) / nrow(mothers_2[!is.na(sib_TW)])*100
nrow(mothers_2[OS==T & !is.na(sib_TW)]) / nrow(mothers_2[!is.na(sib_TW)])*100
nrow(mothers_2[DZ==T & !is.na(sib_TW)]) / nrow(mothers_2[!is.na(sib_TW)])*100
nrow(mothers_2[MZ==T & !is.na(sib_TW)]) / nrow(mothers_2[!is.na(sib_TW)])*100

