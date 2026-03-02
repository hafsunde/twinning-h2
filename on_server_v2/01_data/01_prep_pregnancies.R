#############################################################
#                                                           #
#                  PREPARE SAMPLE (V2)                     #
#                                                           #
#############################################################
#  Data wrangling pipeline for pregnancy-level records.     #
#  Current version includes pregnancies 1975-2021 and       #
#  supports optional linkage of ART/IVF indicators.         #
#############################################################

setwd("../")
library(data.table)

# Observation period for pregnancies
min.year <- 1975
max.year <- 2021
max.maternal.age <- 40

# Optional input for treatment indicators prepared externally.
# Expected columns: pregnancy_id, ART (0/1), IVF (0/1)
# Set to NULL to skip merge.
art_ivf_file <- "inputs/pregnancy_art_ivf.csv"


################################################
######     IMPORT AND PREPARE RAW DATA   #######
################################################

# Imports Basic information / Complete Population Register
population <- fread(
  "N:/durable/Data14/NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv",
  select = c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_", "foedselsaar", "kjoenn", "invkat")
)

# Change names of variables
setnames(
  population,
  c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_", "foedselsaar", "kjoenn", "invkat"),
  c("index_id", "mother_id", "father_id", "birth_year", "sex", "immigration_category")
)

# Import pregnancy IDs and twin info
pregnancies <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/pregnancy_ids.csv", na.strings = "")
twins <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/all_twins_04112024.csv", na.strings = "")

# Create indicator variables used later
twins[, `:=`(DZ = Zygo %in% c(2, 3) | opposite_sex_twin)]
twins[is.na(Zygo) & !opposite_sex_twin, DZ := NA]


################################################
######        FINDING MULTIPLES          #######
################################################

# Remove individuals with unknown mothers and first-generation immigrants
births <- population[!is.na(mother_id) & immigration_category != "B"]

# Attach pregnancy ID
births[pregnancies, on = c("index_id", "mother_id", "birth_year"), pregnancy_id := i.pregnancy_id]
# pregnancy_id would normally just need index_id, but mother_id + birth_year
# are also used because index_id can be missing in recent birth cohorts.

births[, c("immigration_category", "sex") := NULL]

# Attach info from twin data
births[
  twins[, .SD[1], pregnancy_id],
  on = "pregnancy_id",
  `:=`(
    opposite_sex_twin = i.opposite_sex_twin,
    multiples_count = i.multiples_count,
    DZ = i.DZ,
    MZ = i.Zygo == 1
  )
]

# Fill info for non-multiples
births[is.na(multiples_count), `:=`(multiples_count = 1, opposite_sex_twin = FALSE, DZ = FALSE, MZ = FALSE)]

# Attach maternal age
births[population, on = c("mother_id" = "index_id"), maternal_age := birth_year - i.birth_year]

# Keep records with identified pregnancy ID
births <- births[!is.na(pregnancy_id)]

# Collapse siblings in same pregnancy to one row
births <- births[, .SD[1], pregnancy_id]

# Limit sample to pregnancies in target period and maternal age <= 40
births <- births[
  !is.na(mother_id) & !is.na(father_id) &
    between(birth_year, min.year, max.year) &
    maternal_age >= 0 & maternal_age <= max.maternal.age
]

# Double check sex of parent matches role
births[population[!is.na(index_id), .(index_id, sex)], on = c("mother_id" = "index_id"), mother_s := i.sex]
births[population[!is.na(index_id), .(index_id, sex)], on = c("father_id" = "index_id"), father_s := i.sex]
births <- births[mother_s == 2 & father_s == 1]
births[, c("father_s", "mother_s") := NULL]


################################################
######       OPTIONAL ART/IVF LINKAGE    #######
################################################

# If provided, merge indicators for hormone treatment (ART) and IVF.
# Expected values are 0/1. Non-matched pregnancies remain NA.
if (!is.null(art_ivf_file) && file.exists(art_ivf_file)) {
  art_ivf <- fread(art_ivf_file)

  required_cols <- c("pregnancy_id", "ART", "IVF")
  missing_cols <- setdiff(required_cols, names(art_ivf))

  if (length(missing_cols) > 0) {
    stop(sprintf(
      "ART/IVF file is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  art_ivf <- art_ivf[, ..required_cols]
  births[art_ivf, on = "pregnancy_id", `:=`(ART = i.ART, IVF = i.IVF)]
} else {
  births[, `:=`(ART = NA_integer_, IVF = NA_integer_)]
}


################################################
######                SAVE               #######
################################################

saveRDS(births, "01_data/births_v2.rds")
