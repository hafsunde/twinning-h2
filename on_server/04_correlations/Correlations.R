#setwd("../")
library(data.table)
library(polycor)
source("09_functions/misc_functions.R")
cor_custom <- function(x,y) {
  if (length(x) < 50) return(c("rho" = NA_real_, "var" = NA_real_, "n" = length(x), "status" = 1, "min_cell" = NA_real_))
  if (any(table(x,y) == 0)) return(c("rho" = NA_real_, "var" = NA_real_, "n" = length(x), "status" = 2, "min_cell" = 0))
  return(c(unlist(polycor::polychor(x, y, std.err = T)[c("rho","var","n")]), "status" = 0, "min_cell" = min(table(x,y))))
} 

sibling_pairs <- readRDS("01_data/sibling_pairs.rds")
sibling_pairs[,i_n_birth := factor(i_n_birth,ordered = T)]
sibling_pairs[,j_n_birth := factor(j_n_birth,ordered = T)]

twinmother_pairs <- sibling_pairs[sex==2]
twinfather_pairs <- sibling_pairs[sex==1]
twinbother_pairs <- sibling_pairs[sex==0]




 # MOTHERS
result1 <- rbind(
  as.data.frame(t(cor_custom(twinmother_pairs$i_DZ,twinmother_pairs$j_DZ))),
  as.data.frame(t(cor_custom(twinmother_pairs$i_TW,twinmother_pairs$j_TW)))
)

result1[,"se"] <- sqrt(result1[,"var"])
result1[,"Phenotype"] <- c("confirmed_DZ","any_twin")
result1[, c("Lower", "Upper")] <- SEtoCI(result1$rho,result1$se, is_correlation = T)
result1[,"sex"] <- "Sisters"

# FATHERS
result2 <- rbind(
  as.data.frame(t(cor_custom(twinfather_pairs$i_DZ,twinfather_pairs$j_DZ))),
  as.data.frame(t(cor_custom(twinfather_pairs$i_TW,twinfather_pairs$j_TW)))
)
result2[,"se"] <- sqrt(result2[,"var"])
result2[,"Phenotype"] <-  c("confirmed_DZ","any_twin")
result2[, c("Lower", "Upper")] <- SEtoCI(result2$rho,result2$se, is_correlation = T)
result2[,"sex"] <- "Brothers"


# FATHERS
result3 <- rbind(
  as.data.frame(t(cor_custom(twinbother_pairs$i_DZ,twinbother_pairs$j_DZ))),
  as.data.frame(t(cor_custom(twinbother_pairs$i_TW,twinbother_pairs$j_TW)))
)
result3[,"se"] <- sqrt(result3[,"var"])
result3[,"Phenotype"] <-  c("confirmed_DZ","any_twin")
result3[, c("Lower", "Upper")] <- SEtoCI(result3$rho,result3$se, is_correlation = T)
result3[,"sex"] <- "Opposite-Sex"



result <- rbind(result1,result2,result3)
write_csv(result, "08_result_files/sibling_correlations.csv")

contingency_tables <- list(
  mother_DZ = table(twinmother_pairs$i_DZ,twinmother_pairs$j_DZ),
  mother_TW = table(twinmother_pairs$i_TW,twinmother_pairs$j_TW),
  father_DZ = table(twinfather_pairs$i_DZ,twinfather_pairs$j_DZ),
  father_TW = table(twinfather_pairs$i_TW,twinfather_pairs$j_TW),
  bother_DZ = table(twinbother_pairs$i_DZ,twinbother_pairs$j_DZ),
  bother_TW = table(twinbother_pairs$i_TW,twinbother_pairs$j_TW)
)
saveRDS(contingency_tables, "08_result_files/contingency_tables.rds")

# Temporary figure
(p1 <- ggplot(filter(result), 
       aes(y=rho, x=Phenotype, ymin = Lower, ymax=Upper,color=sex, group=sex, label =paste0("r = ",numformat(rho,3)))) +
  geom_hline(yintercept=0,linewidth=1) +
  geom_errorbar(width=.2, linewidth=1, position=position_dodge(width=.7)) +
  geom_point(size=5, position=position_dodge(width=.7)) +
  geom_text(mapping = aes(y=Upper+.02), size=5, position=position_dodge(width=.7),color="black") +
  custom_theme(base_size=20) +
  theme(#axis.text.x = element_text(angle=45, hjust=1), 
        panel.background = element_rect(fill = "grey95"),
        panel.grid.major.y = element_line(color="grey", linetype="dashed",linewidth = 0.5),
        legend.key = element_blank(),
        legend.position = "top") +

  coord_cartesian(ylim=c(-.3,.4)) +
  scale_y_continuous(labels = function(x) numformat(x), breaks = seq(-1,1,.1)) +
  scale_color_manual(values=clrs.drk) +
  labs(y = "Tetrachoric Correlation", 
       x="Type of multiple birth", 
       color="", 
       title="")
  
)



