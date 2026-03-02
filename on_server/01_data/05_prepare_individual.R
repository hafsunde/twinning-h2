library(data.table)
setwd("../")
births <- readRDS("01_data/births.rds")
mothers <- readRDS("01_data/mothers.rds")
fathers <- readRDS("01_data/fathers.rds")
sibling_pairs <- readRDS("01_data/sibling_pairs.rds")
twinmother_pairs <- sibling_pairs[sex==2]
twinfather_pairs <- sibling_pairs[sex==1]


# Here, we use the pairs and combine into a format where each individual is listed once, with variables indicating whether they had an affected sibling



# Because we will be joining according to i_id, we must make sure individuals in all pairs are represented both as i_id and j_id
twinmother_pairs2 <- copy(twinmother_pairs) #(we need to use copy() because of how data.table works in memory)

setnames(twinmother_pairs2, 
         old = names(twinmother_pairs2), 
         new = gsub("^temp_", "i_", gsub("^i_", "j_", gsub("^j_", "temp_", names(twinmother_pairs2)))))

sisters <- rbindlist(list(twinmother_pairs, twinmother_pairs2), use.names = T)


sisters <- sisters[,.(TW = any(j_TW),DZ = any(j_DZ),n_birth = mean(j_n_birth)), by = i_id]
setnames(sisters, old = names(sisters)[-1], new=paste0("sib_",names(sisters)[-1]))
rm(twinmother_pairs2); gc()


mothers_2 <- merge(mothers, sisters, by.x = "index_id", by.y = "i_id", all.x = T)

