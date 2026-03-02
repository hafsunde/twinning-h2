library(data.table)
setwd("../")


births <- readRDS("01_data/births.rds")


mothers <- births[,
                  .(n_children = sum(multiples_count), # Number of children in obs. period
                    n_birth = .N, # Number of births in obs. period
                    birth_year = first(birth_year - maternal_age), # Birth year of mother
                    TW = any(multiples_count>1), OS = any(opposite_sex_twin), DZ = any(DZ), MZ = any(MZ)), mother_id] # Any OS/DZ twins

setnames(mothers, "mother_id", "index_id")




fathers <- births[,
                  .(n_children = sum(multiples_count),
                    n_birth = .N,
                    birth_year = first(birth_year - maternal_age), # birth year of FIRST MOTHER! (we don't care about paternal ages...)
                    TW = any(multiples_count>1), OS = any(opposite_sex_twin), DZ = any(DZ), MZ = any(MZ)), father_id]


setnames(fathers, "father_id", "index_id")


saveRDS(mothers, "01_data/mothers.rds")
saveRDS(fathers, "01_data/fathers.rds")


############ CALCULATES DESCRIPTIVES:


nrow(mothers)
sum(mothers$n_children)
sum(mothers$n_birth)


nrow(fathers)
sum(fathers$n_children)
sum(fathers$n_birth)



