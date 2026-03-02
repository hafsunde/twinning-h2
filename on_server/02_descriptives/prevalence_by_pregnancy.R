library(data.table)
setwd("../")
births <- readRDS("01_data/births.rds")



# Results
message("\nNumber of succesful pregnancies: ", nrow(births),
        "\n",
        "\nNumber of multiple births (per pregnancy): ",
        sum(births$multiples_count!=1), 
        " (",round(sum(births$multiples_count!=1)/nrow(births)*100,2),"%)",
        "\nNumber of twin births (per pregnancy): ",
        sum(births$multiples_count==2), 
        " (",round(sum(births$multiples_count==2)/nrow(births)*100,2),"%)",
        "\nNumber of twin births with opposite_sex twins (per pregnancy): ", 
        sum(births$multiples_count==2 & births$opposite_sex_twin),
        " (",round(sum(births$multiples_count==2 & births$opposite_sex_twin)/nrow(births)*100,2),"%)",
        "\n \n",
        "Implied numbers of DZ/MZ twin pairs (per pregnancy):\n",
        "DZ: ", 
        sum(births$multiples_count==2 & births$opposite_sex_twin)*2,
        " (",round(2*sum(births$multiples_count==2 & births$opposite_sex_twin)/nrow(births)*100,2),"%)",
        "\nMZ: ",
        sum(births$multiples_count==2) - sum(births$multiples_count==2 & births$opposite_sex_twin)*2,
        " (",round((sum(births$multiples_count==2) - 2*sum(births$multiples_count==2 & births$opposite_sex_twin))/nrow(births)*100,2),"%)")



nrow(births[DZ==T & multiples_count==2]); nrow(births[DZ==T & multiples_count==2]) / nrow(births)*100 # DZ births
nrow(births[DZ==F & multiples_count==2]); nrow(births[DZ==F & multiples_count==2]) / nrow(births)*100 # MZ births
nrow(births[is.na(DZ) & multiples_count==2]); nrow(births[is.na(DZ) & multiples_count==2]) / nrow(births)*100 # UZ births



# Number of higher order multiples
nrow(births[multiples_count>2]) 
nrow(births[multiples_count>2]) / nrow(births)*100
nrow(births[multiples_count>2]) / nrow(births[multiples_count>1])*100
mean(births[multiples_count>2,opposite_sex_twin])

