library(data.table)
setwd("../")

# Import data
all_siblings <- readRDS("01_data/all_siblings.rds")
mothers <- readRDS("01_data/mothers.rds")
fathers <- readRDS("01_data/fathers.rds")
parents <- rbindlist(list(mothers,fathers))

#  restrict sample to just the pairs you are interested in
rel <- all_siblings[type %in% c("full sibs", "twins") & (i_id %in% parents$index_id & j_id %in% parents$index_id)]
rel[type=="twins" & r==.5, type:="full sibs"]
rel <- rel[type!="twins" & adopt==0]
rel[,':='(twin = NULL, adopt = NULL, r = NULL, type = NULL, lineage=NULL)]

# Attach phenotypes
rel[parents, on = c("i_id" = "index_id"), ':='(i_TW = i.TW, i_DZ = i.DZ, i_n_birth = i.n_birth)]
rel[parents, on = c("j_id" = "index_id"), ':='(j_TW = i.TW, j_DZ = i.DZ, j_n_birth = i.n_birth)]

# Descriptives for mothers
length(unique(c(rel[sex==2]$i_id, rel[sex==2]$j_id)))
nrow(rel[sex==2])

# save data
saveRDS(rel, "01_data/sibling_pairs.rds")
sibling_pairs <- rel; rm(rel)






