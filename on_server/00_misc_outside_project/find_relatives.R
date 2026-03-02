# -------------------------------------------------------------------------
# This script identifies and classifies biological relatives in a population-
# level registry using parent–offspring links. Starting from individual records
# with known maternal and paternal identifiers, it constructs pairwise
# relationships including full and half siblings, twins, parent–offspring,
# avuncular relations, cousins (first, second, removed), and various forms of
# double cousins.
#
# The pipeline assigns expected genetic relatedness coefficients, relationship
# types, lineage indicators, and adoption status, and integrates external twin
# and zygosity information where available. The code is implemented as a
# standalone data.table-based pipeline intended for internal research use and
# may be refactored into reusable functions in the future.
# -------------------------------------------------------------------------


# FINDINGS (practically) ALL RELATIVES IN THE POPULATION
# Script author: HANS FREDRIK SUNDE (hfsu@fhi.no)
library(data.table)


t1 <- Sys.time()

################################################
######     IMPORT AND PREPARE RAW DATA   #######
################################################

## load a registry file, which needs to have columns for: person's ID, mother's ID, father's ID
reg <- fread("N:/durable/Data14/NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv",
             select = c("mor_lnr_k2_", "far_lnr_k2_","w19_1011_lnr_k2_", "kjoenn", "fodeland","invkat"))

setnames(reg,
         c("w19_1011_lnr_k2_", "mor_lnr_k2_", "far_lnr_k2_", "kjoenn", "fodeland","invkat"),
         c("index_id", "mor", "far","sex","birth_country","immigration_category"))
reg <- reg[!is.na(index_id)]
setkey(reg, index_id)



# Removes individuals whose mother/father doesn't match the indicated sex
# (This would imply adoption or other forms of entry errors)
# Could also be later legal sex change
reg[reg, on = c("mor" = "index_id"), mother_s := i.sex]
reg <- reg[is.na(mor) | mother_s == 2]

reg[reg, on = c("far" = "index_id"), father_s := i.sex]
reg <- reg[is.na(far) | father_s == 1]
reg[,c("father_s", "mother_s") :=NULL]


# Find adoptees (born to Norwegian parents abroad in countries with adoptee-agreements )
reg[, adopt:=0]
reg[immigration_category == "G" & birth_country %in% c(484, 492, 730, 725, 359, 444, 432, 152, 393,
                                                       113, 568, 428, 289, 760, 424, 575, 715, 246, 241), adopt := 1]



# Initialize the ids data.table with the primary ids and their parents
# Only individuals with known parents here
ids <- reg[!is.na(mor) & !is.na(far), .(index_id, mor, far,adopt)]


################################################
######            FIND SIBLINGS          #######
################################################


########## FIND MATERNAL SIBLINGS
mothers <- ids[,.(index_id,N=.N),by=mor][N>1]
mothers[,N:=NULL]
mothers <- mothers[sample(1:nrow(mothers))]      #Randomize order so that sibling order is random
mothers[, sibling_num := seq_len(.N), by = mor]  #Create a sequence column to differentiate siblings within the same 'mor' group


# Use dcast to create wide format with siblings spread across columns
wide_dt <- dcast(mothers, mor ~ sibling_num, value.var = "index_id")

maternal_pairs <- rbindlist(
  lapply(
    lapply(2:(ncol(wide_dt)-1), function(i) {
      lapply((i + 1):ncol(wide_dt), function(j) {
        wide_dt[!is.na(get(names(wide_dt)[i])) & !is.na(get(names(wide_dt)[j])), .(mor, i_id = get(names(wide_dt)[i]), j_id = get(names(wide_dt)[j]))]
      })
    }), rbindlist))

maternal_pairs[,':='(
  pair_id = paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
               pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_"),
  mor = NULL,
  r=.25,
  lineage = "m"
)]






########## FIND PATERNAL SIBLINGS


fathers <- ids[,.(index_id,N=.N),by=far][N>1]
fathers[,N:=NULL]
fathers <- fathers[sample(1:nrow(fathers))]      #Randomize order so that sibling order is random
fathers[, sibling_num := seq_len(.N), by = far]  #Create a sequence column to differentiate siblings within the same 'far' group


# Use dcast to create wide format with siblings spread across columns
wide_dt <- dcast(fathers, far ~ sibling_num, value.var = "index_id")

paternal_pairs <- rbindlist(
  lapply(
    lapply(2:(ncol(wide_dt)-1), function(i) {
      lapply((i + 1):ncol(wide_dt), function(j) {
        wide_dt[!is.na(get(names(wide_dt)[i])) & !is.na(get(names(wide_dt)[j])), .(far, i_id = get(names(wide_dt)[i]), j_id = get(names(wide_dt)[j]))]
      })
    }), rbindlist))

paternal_pairs[,':='(
  pair_id = paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_"),
  far = NULL,
  r=.25,
  lineage = "p"
)]


# FIND FULL SIBLINGS (i.e, both maternal and paternal siblings)

all_siblings <- rbindlist(list(maternal_pairs,paternal_pairs),use.names=T)

all_siblings[,N:=.N,pair_id]
all_siblings[N==2,':='(r=.5,lineage="b")]
all_siblings <- all_siblings[,.SD[1],pair_id]
all_siblings[,N:=NULL]
all_siblings[r==.5, type := "full sibs"]
all_siblings[r==.25,type := "half sibs"]
rm(paternal_pairs, maternal_pairs, mothers, fathers, wide_dt)
gc()


#######################################################
######                 FIND TWINS               #######
#######################################################


## Import twins
twins <- fread("N:/durable/Data14/DataModuler/tvillingzygositet/all_twins_04112024.csv", na.strings = "")
twins <- twins[!is.na(index_id)]

twins <- twins[sample(1:nrow(twins))]      #Randomize order so that sibling order is random
twins[, sibling_num := seq_len(.N), by = pregnancy_id]  #Create a sequence column to differentiate siblings within the same 'far' group


wide_dt <- dcast(twins[,.(pregnancy_id, sibling_num, index_id)], pregnancy_id ~ sibling_num, value.var = "index_id")

twin_pairs <- rbindlist(
  lapply(
    lapply(2:(ncol(wide_dt)-1), function(i) {
      lapply((i + 1):ncol(wide_dt), function(j) {
        wide_dt[!is.na(get(names(wide_dt)[i])) & !is.na(get(names(wide_dt)[j])), .(pregnancy_id, i_id = get(names(wide_dt)[i]), j_id = get(names(wide_dt)[j]))]
      })
    }), rbindlist))

twin_pairs[twins[,.SD[1], pregnancy_id], on="pregnancy_id", ':='(OS_t = i.opposite_sex_twin,
                                                                 Zygo = i.Zygo)]
twin_pairs[,pair_id := paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                            pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_")]


twin_pairs[is.na(Zygo),Zygo := 0]
table(twin_pairs$Zygo)
twin_pairs[OS_t==T, Zygo:=3]
twin_pairs[, Zygo := as.character(factor(Zygo, levels = c(0, 1, 2, 3), labels = c("UZ", "MZ", "DZ", "DZ")))]
table(twin_pairs$Zygo)


all_siblings[twin_pairs, on = "pair_id", twin := i.Zygo]
all_siblings[!is.na(twin), type:="twins"]

# Some twins are not in the all_sibling file due to missing parents.
# Let us add those with known zygosity
twin_pairs_extra <- twin_pairs[!pair_id %in% all_siblings$pair_id]
twin_pairs_extra[,':='(pregnancy_id=NULL, OS_t=NULL, r = 0.5, lineage="b", type="twins", twin=Zygo, Zygo=NULL)]



# Combine and adjust r-values where known
all_siblings <- rbindlist(list(all_siblings,twin_pairs_extra), use.names = T)

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
# I have not investigate this subgroup (179 pairs) thouroughly, but I'm
# removing the adopt-indicator
all_siblings[type=="twins" & adopt==1, adopt:=0]



# Change relatedness for adoptees:
all_siblings[adopt==1, r:=0]


# Clean up (for memory):
rm(twin_pairs, twin_pairs_extra, twins); gc()

################################################
######      FIND PARENT OFFSPRING PAIRS  #######
################################################



mothers <- reg[!is.na(mor),.(index_id,mor,adopt)]
setnames(mothers,c("index_id","mor"),c("child_id","parent_id"))
mothers[,':='(r = .5, lineage="m", type= "par offspring")]


fathers <- reg[!is.na(far),.(index_id,far,adopt)]
setnames(fathers,c("index_id","far"),c("child_id","parent_id"))
fathers[,':='(r = .5, lineage="p", type= "par offspring")]

parent_offspring <- rbindlist(list(mothers,fathers))
rm(mothers, fathers); gc()


# Change relatedness for adoptees:
parent_offspring[adopt==1, r:=0]


###################################################
######    FIND COUSIN AND AVUNCULAR PAIRS   #######
###################################################
# (i.e., children of siblings)

# ATTACHES CHILDREN OF SIBLINGS
cousin_pairs <- merge(all_siblings,parent_offspring[,.(child_id,parent_id, lineage,adopt)],
                      by.x = "i_id", by.y = "parent_id", all.x = T, allow.cartesian = T)
setnames(cousin_pairs, c("child_id","lineage.x", "lineage.y", "adopt.x", "adopt.y"), c("child_id_i","lineage", "lineage_i", "adopt", "adopt_i"))
cousin_pairs <- merge(cousin_pairs,parent_offspring[,.(child_id,parent_id, lineage,adopt)],
                      by.x = "j_id", by.y = "parent_id", all.x = T, allow.cartesian = T)
setnames(cousin_pairs, c("child_id","lineage.x", "lineage.y", "adopt.x", "adopt.y"), c("child_id_j","lineage", "lineage_j", "adopt", "adopt_j"))



# FIND UNCLES/AUNTS
avuncular_pairs1 <- cousin_pairs[!is.na(child_id_j),.(i_id,child_id_j, r,lineage, lineage_j, adopt,adopt_j,twin)]
avuncular_pairs2 <- cousin_pairs[!is.na(child_id_i),.(j_id,child_id_i, r,lineage, lineage_i, adopt,adopt_i,twin)]
setnames(avuncular_pairs1, c(       "lineage_j", "child_id_j", "adopt_j"), c(       "lineage_y", "j_id", "adopt_y"))
setnames(avuncular_pairs2, c("j_id","lineage_i", "child_id_i", "adopt_i"), c("i_id","lineage_y", "j_id", "adopt_y"))

avuncular_pairs <- rbindlist(list(avuncular_pairs1,avuncular_pairs2))
rm(avuncular_pairs1,avuncular_pairs2); gc()

avuncular_pairs <- avuncular_pairs[,.SD[1], by = .(i_id, j_id)] # Remove duplicates

avuncular_pairs[adopt == 1 | adopt_y == 1,adopt := 1]
avuncular_pairs[, adopt_y :=NULL]

avuncular_pairs[, ':='(r=r*0.5, lineage = paste0(lineage,lineage_y))]
avuncular_pairs[,lineage_y:=NULL]

avuncular_pairs[, type:="avuncular"]
avuncular_pairs[!grepl("b",lineage), type:="half avuncular"]


# FIND 1st COUSINS and half cousins
cousin_pairs <- cousin_pairs[!is.na(child_id_i) & !is.na(child_id_j), .(child_id_i, child_id_j, r, lineage, lineage_i, lineage_j, adopt, adopt_i, adopt_j,twin)]
cousin_pairs[, ':='(r=r*0.5*0.5, lineage = paste0(lineage_i,lineage,lineage_j), adopt = adopt_i+adopt+adopt_j)]
cousin_pairs[adopt>0, ':='(adopt=1)]
cousin_pairs[,':='(lineage_i=NULL, lineage_j=NULL, adopt_i=NULL, adopt_j=NULL)]
setnames(cousin_pairs, c("child_id_i", "child_id_j"), c("i_id", "j_id"))

cousin_pairs[, type:="1st cousins"]
cousin_pairs[!grepl("b",lineage), type:="half 1st cousins"]




################################################
######       FIND DOUBLE 1st COUSINS      #######
################################################

# Add ID number for pair that is constant regardless of i_id versus j_id placement
cousin_pairs[,  pair_id := paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                               pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_")]


cousin_pairs[,N:=.N,pair_id] # Identify double pairs by duplicates

double_cousins <- cousin_pairs[N>1]
cousin_pairs <- cousin_pairs[N==1]
cousin_pairs[,N:=NULL]

double_cousins[N>1, N2 := length(unique(type)),by=pair_id] # Identify number of types of relationships

# Fix lineages
double_cousins[N2==2, type := "misc cousins"]
double_cousins[N2==2 | type=="half 1st cousins", lineage:=NA]
double_cousins[N2==1 & type=="1st cousins", lineage:="bbb"]


double_cousins <- double_cousins[,.(i_id = first(i_id),
                          j_id = first(j_id),
                          r = sum(r),
                          lineage = first(lineage),
                          adopt = max(adopt),
                          twin = paste0(unique(na.omit(twin)), collapse="_"),
                          type = paste0("double ",first(type))), pair_id]
double_cousins[twin=="", twin:=NA]



cousin_pairs <- rbindlist(list(cousin_pairs, double_cousins), use.names = T)




#######################################################
######   FIND 1st COUSINS 1R AND 2nd COUSINS    #######
#######################################################




sec_cousin_pairs <- merge(cousin_pairs,parent_offspring[,.(child_id,parent_id, lineage,adopt)],
                      by.x = "i_id", by.y = "parent_id", all.x = T, allow.cartesian = T)
setnames(sec_cousin_pairs, c("child_id","lineage.x", "lineage.y", "adopt.x", "adopt.y"), c("child_id_i","lineage", "lineage_i", "adopt", "adopt_i"))
sec_cousin_pairs <- merge(sec_cousin_pairs,parent_offspring[,.(child_id,parent_id, lineage,adopt)],
                      by.x = "j_id", by.y = "parent_id", all.x = T, allow.cartesian = T)
setnames(sec_cousin_pairs, c("child_id","lineage.x", "lineage.y", "adopt.x", "adopt.y"), c("child_id_j","lineage", "lineage_j", "adopt", "adopt_j"))





# FIND 1st cousins 1 removed
cousin_removed1 <- sec_cousin_pairs[!is.na(child_id_j),.(i_id,child_id_j, r,lineage, lineage_j, adopt,adopt_j,twin)]
cousin_removed2 <- sec_cousin_pairs[!is.na(child_id_i),.(j_id,child_id_i, r,lineage, lineage_i, adopt,adopt_i,twin)]
setnames(cousin_removed1, c(       "lineage_j", "child_id_j", "adopt_j"), c(       "lineage_y", "j_id", "adopt_y"))
setnames(cousin_removed2, c("j_id","lineage_i", "child_id_i", "adopt_i"), c("i_id","lineage_y", "j_id", "adopt_y"))

cousin_removed <- rbindlist(list(cousin_removed1,cousin_removed2))
rm(cousin_removed1,cousin_removed2); gc()

cousin_removed <- cousin_removed[,.SD[1], by = .(i_id, j_id)] # Remove duplicates

cousin_removed[adopt == 1 | adopt_y == 1,adopt := 1]
cousin_removed[, adopt_y :=NULL]

cousin_removed[, ':='(r=r*0.5, lineage = paste0(lineage,lineage_y))]
cousin_removed[,lineage_y:=NULL]

cousin_removed[, type:="1st cousin 1R"]
cousin_removed[!grepl("b",lineage), type:="half 1st cousin 1R"]
cousin_removed[grepl("bbb",lineage), type := "D1st cousin 1R"]
cousin_removed[grepl("NA",lineage), ':='( lineage= NA, type = "misc cousins")]

cousin_removed[,  pair_id := paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                                   pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_")]


# Find 2nd cousins and half 2nd cousins
sec_cousin_pairs <- sec_cousin_pairs[!is.na(child_id_i) & !is.na(child_id_j), .(child_id_i, child_id_j, r, lineage, lineage_i, lineage_j, adopt, adopt_i, adopt_j, twin)]
sec_cousin_pairs[, ':='(r=r*0.5*0.5, lineage = paste0(lineage_i,lineage,lineage_j), adopt = adopt_i+adopt+adopt_j)]
sec_cousin_pairs[adopt>0, ':='(adopt=1)]
sec_cousin_pairs[,':='(lineage_i=NULL, lineage_j=NULL, adopt_i=NULL, adopt_j=NULL)]
setnames(sec_cousin_pairs, c("child_id_i", "child_id_j"), c("i_id", "j_id"))


sec_cousin_pairs[, type:="2nd cousins"]
sec_cousin_pairs[!grepl("b",lineage), type:="half 2nd cousins"]

sec_cousin_pairs[grepl("bbb",lineage), type:="misc cousins" ]
sec_cousin_pairs[grepl("NA",lineage), ':='( lineage= NA, type = "misc cousins")]

sec_cousin_pairs[,  pair_id := paste(pmax(substr(i_id,4,10), substr(j_id,4,10)),
                                     pmin(substr(i_id,4,10), substr(j_id,4,10)), sep = "_")]



################################################
######       FIND MORE DOUBLE COUSINS      #######
################################################
# Here we find additional double cousins (in addition to double (half) 1st cousins)
# This will include double 2nd cousins, double 1st cousins 1R, and mixes of all types of cousins
allcousins <- rbindlist(list(
  cousin_pairs,
  sec_cousin_pairs,
  cousin_removed
), use.names = T)

rm(cousin_pairs,sec_cousin_pairs, cousin_removed); gc()


setorder(allcousins, pair_id)
allcousins[,N:=.N,pair_id] # Identify double pairs

double_cousins <- allcousins[N>1]
allcousins <- allcousins[N==1]
allcousins[,N:=NULL]

double_cousins[, N2 := length(unique(type)),by=pair_id]



# Fix lineages
double_cousins[N2>1, type := "misc cousins"]
double_cousins[N>2, type := "misc cousins"]
double_cousins[, lineage:=NA]


double_cousins <- double_cousins[,.(i_id = first(i_id),
                                    j_id = first(j_id),
                                    r = sum(r),
                                    lineage = first(lineage),
                                    adopt = max(adopt),
                                    twin = paste0(unique(na.omit(twin)), collapse="_"),
                                    type = paste0("double ",first(type))), pair_id]
double_cousins[twin=="", twin:=NA]



allcousins <- rbindlist(list(allcousins, double_cousins), use.names = T)
allcousins[,pair_id:=NULL]



################################################
######         COMBINES INTO ONE         #######
################################################

setnames(parent_offspring, c("child_id", "parent_id"), c("j_id", "i_id"))
parent_offspring[,twin:=NA_character_]

allrelatives <- rbindlist(list(
  all_siblings,
  allcousins,
  parent_offspring,
  avuncular_pairs
), use.names = T)


allrelatives[reg[,.(index_id, sex)], on = c("i_id" = "index_id"),i_sex := i.sex]
allrelatives[reg[,.(index_id, sex)], on = c("j_id" = "index_id"),j_sex := i.sex]


allrelatives[i_sex == 1 & j_sex == 1, sex := 1]
allrelatives[i_sex == 2 & j_sex == 2, sex := 2]
allrelatives[i_sex == 1 & j_sex == 2, sex := 0]
allrelatives[i_sex == 2 & j_sex == 1, sex := 0]
allrelatives[, c("i_sex", "j_sex") := NULL]


print(Sys.time() - t1)
print(table(allrelatives[,r]))

fwrite(allrelatives, "relatives.csv", sep = ",")
Sys.time() - t1

#



