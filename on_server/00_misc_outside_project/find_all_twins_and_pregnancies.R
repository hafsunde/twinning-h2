# Creates pregnancy id's and finds all sets of multiples among individuals with known motheres.
# Also attaches zygosity information from twin register, and attaches twin pairs with unknown mothers

# Run time: ~52 seconds

# -------------------------------------------------------------------------
# This script identifies twins and multiple pregnancies in population-level
# registry data. It implements a data-processing pipeline that reads individual-
# level records, detects potential multiple births based on shared parental and
# temporal information, assigns pregnancy identifiers, and links additional
# metadata where available.
#
# The code is written as a standalone pipeline and relies on specific input
# files and data conventions. It is intended for internal research use and is
# expected to evolve. Core components may later be refactored into reusable
# functions.
# -------------------------------------------------------------------------


library(data.table)

################################################
######     IMPORT AND PREPARE RAW DATA   #######
################################################
t1 <- Sys.time()

# Imports Basic information / Complete Population Register
population <- fread("N:/durable/Data14/NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv",
                    select = c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_","foedselsaar","foedsels_aar_mnd", "kjoenn"))

# Change names of variables
setnames(population,
         c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_","foedselsaar","foedsels_aar_mnd", "kjoenn"),
         c("index_id", "mother_id", "father_id","birth_year","birth_month", "sex"))




################################################
######        FINDING MULTIPLES          #######
################################################


# Remove individuals with unknown mothers...
population <- population[!is.na(mother_id)]

# First, we want to change birth_month variable to years with months in decimals. (i.e., January 1985 = 1985.0; December 1985 = 1985.92)
# Currently, birth_month is a six digit integer: YYYYMM
# For the decimal conversion to work, we want January to be the 0'th month and December to be the 11th month
population[,birth_month := birth_year + ((as.numeric(substr(birth_month,5,6))-1)/12)]


# Sort the data.table by mother_id and birth_month so that twins are on consecutive rows
setkey(population, mother_id, birth_month)

# Identify potential twins based on mother_id and small difference in birth_month
population[, twin := (
  (mother_id == shift(mother_id, type = "lag", fill = FALSE) &
     abs(birth_month - shift(birth_month, type = "lag", fill = Inf)) < 0.1) |
    (mother_id == shift(mother_id, type = "lead", fill = FALSE) &
       abs(birth_month - shift(birth_month, type = "lead", fill = Inf)) < 0.1)
)]
# The above code does two thing:
# 1.  Check if the current row and the previous row share the same mother_id
#     and have a birth_month difference of less than 0.1
# 2.  Check if the current row and the next row share the same mother_id
#     and have a birth_month difference of less than 0.1

# Explanation:
# 1. `setkey()` sorts the data.table by `mother_id` and `birth_month`.
# 2. `shift()` is used to access the previous (`lag`) and next (`lead`) rows.
# 3. `fill = FALSE` ensures comparisons are valid even at the boundaries by
#    filling out-of-bound `mother_id` with `FALSE`.
# 4. `fill = Inf` for `birth_month` ensures that comparisons outside of the data
#    range do not produce `NA`, effectively bypassing such checks.
# 5. The `twin` column will be `TRUE` for rows identified as twins and `NA` or `FALSE` otherwise.



################################################
######       CREATE PREGNANCY IDs        #######
################################################

# First, we must shift the birth month of all second-born twins in pairs born on either side of new_years eve
population[twin == T &   ((mother_id == shift(mother_id, type = "lag", fill = FALSE) &
                             abs(birth_month - shift(birth_month, type = "lag", fill = Inf)) < 0.1 &
                             birth_year != shift(birth_year, type = "lag", fill = NA)) |
                            (mother_id == shift(mother_id, type = "lead", fill = FALSE) &
                               abs(birth_month - shift(birth_month, type = "lead", fill = Inf)) < 0.1 &
                               birth_year != shift(birth_year, type = "lead", fill = NA)) ),
           birth_month := birth_month - .5
]

# Create a pregnancy_id
# For singletons, this is just the index_id
# For multiples, we create a new id with with six digits based on mother_id and birth year (shared by all members of a set of multiples)
# We are here assuming that no mothers have gotten two sets of multiples within the same year (you may want to double check this!)
population[, pregnancy_id := index_id]
population[is.na(pregnancy_id), pregnancy_id := sprintf("na_%06d", .I)] # If people have missing index_id, these must be filled in
population[twin==T, pregnancy_id := sprintf("id_%06d", .GRP), by = .(mother_id, floor(birth_month))]
# Explanation: `sprintf("id_%06d", .GRP)` formats the pregnancy_id as a string with a prefix `id_` and zero-padded to 6 digits.

# Count the number of individuals in each pregnancy
population[, multiples_count := .N, by = pregnancy_id]
# Explanation: `.N` counts the number of rows in each pregnancy_id.


# Inspect quadruplets+ to ensure that there are actually not two sets of multiples born within on year
# Any pregnancy_id's returned by this line of code needs further scrutiny
population[multiples_count >= 4, .N,by=.(pregnancy_id, birth_month)][N<4] # Any less than 4 would indicate a problem




#####################################################################
######   IDENTIFIES PREGNANCIES WITH OPPOSITE-SEX MULTIPLES   #######
#####################################################################


# Create the opposite_sex_twin variable by checking if not all individuals within a pregnancy have the same sex
population[twin==T, opposite_sex_twin := (length(unique(sex)) > 1), by = pregnancy_id]


pregnancies <- population[,.(index_id, birth_year, mother_id, pregnancy_id)]
fwrite(pregnancies, "pregnancy_ids.csv")



##########################################################
######       LINKS ZYGOSITY FROM Twin Registry     #######
##########################################################

# Subset twins and remove non-essential variables
twins_population <- population[twin==T]
twins_population[,c("birth_month", "twin"):=NULL]


# Imports Twin Registry
twins_ntr <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/ntr_zyg_2020-10-15.csv")

# adds pregnancy_id from identified twins
twins_ntr[twins_population, on = c("LNr_SSB_k2_" = "index_id"), pregnancy_id := i.pregnancy_id]
twins_ntr[is.na(pregnancy_id), pregnancy_id := sprintf("tr_%06d", ParNr)]
twins_ntr[, N_in_pair := .N, by = pregnancy_id]


# Adds info from twin register to identified twins
twins_population[twins_ntr[,head(.SD,1), by = pregnancy_id], on = "pregnancy_id", ':='(Zygo = i.Zygo, N_in_pair = i.N_in_pair)]
twins_population[,Person_in_NTR := index_id %in% twins_ntr$LNr_SSB_k2_]
twins_population[is.na(N_in_pair),             Pair_in_NTR := "No"]
twins_population[N_in_pair != multiples_count, Pair_in_NTR := "Partial"]
twins_population[N_in_pair == multiples_count, Pair_in_NTR := "Complete"]
twins_population[,N_in_pair := NULL]


# Some twins in the twin register have missing mothers in the population register.
# We can add them to the list if both twins are included
twins_ntr_extra <- twins_ntr[!LNr_SSB_k2_ %in% twins_population$index_id & N_in_pair > 1]

# Reimport population file
twins_WOmothers <- fread("N:/durable/Data14/NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv",
                    select = c("w19_1011_lnr_k2_","mor_lnr_k2_","far_lnr_k2_","foedselsaar", "kjoenn"))
setnames(twins_WOmothers,
         c("w19_1011_lnr_k2_","mor_lnr_k2_","far_lnr_k2_","foedselsaar", "kjoenn"),
         c("index_id","mother_id","father_id", "birth_year", "sex"))


twins_WOmothers <- twins_WOmothers[index_id %in% twins_ntr_extra$LNr_SSB_k2_]

twins_WOmothers[twins_ntr_extra, on = c("index_id" = "LNr_SSB_k2_"), ':='(Zygo = i.Zygo, multiples_count = i.N_in_pair, pregnancy_id = i.pregnancy_id)]
twins_WOmothers[,':='(Person_in_NTR=T, Pair_in_NTR = "Complete")]
twins_WOmothers[, opposite_sex_twin := (length(unique(sex)) > 1), by = pregnancy_id]


twins_population <- rbindlist(list(twins_population, twins_WOmothers),use.names = T)

fwrite(twins_population,"all_twins_04112024.csv")

Sys.time() - t1



# Plots number of twins by birth year
#    test1 <- twins_population[,.N,.(birth_year,sex,Pair_in_NTR)]
#    test2 <- expand.grid(birth_year = 1915:2015,sex=c(1,2),Pair_in_NTR = c("Partial","Complete","No"))
#    test3 <- merge(test1,test2, all.y = T)
#    test4 <-  test3[,.(N = sum(N, na.rm = T), Pair_in_NTR = "Total"), .(birth_year,sex)]
#    test <- rbindlist(list(test3,test4),use.names = T) # %>% filter(Pair_in_NTR != "No")
#
#    library(ggplot2)
#   ggplot(test,aes(x=birth_year, y=N,  color=Pair_in_NTR, group=paste(sex,Pair_in_NTR))) +
#      geom_line(linewidth=1) +
#      theme_bw(base_size=20) +
#      theme(legend.position = "bottom") +
#      facet_wrap(~sex)

