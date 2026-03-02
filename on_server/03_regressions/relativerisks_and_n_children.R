library(data.table)

mothers_2 <- readRDS("01_data/mothers_Wsib.rds")



# Calculate confidence intervals
z <- qnorm((1 + .95) / 2) # Z-score for given confidence level



prev_DZ <- mothers_2[!is.na(DZ) & !is.na(sib_TW), .(total = .N, positive = sum(DZ == 1)), by = sib_DZ]
prev_DZ[,prevalence := positive / total]
prev_DZ[, se := sqrt((prevalence * (1 - prevalence)) / total)]
prev_DZ[, `:=`(
  ci_lower = prevalence - z * se,
  ci_upper = prevalence + z * se
)]

print(prev_DZ)

m_DZ      <- glm(DZ ~ sib_DZ, family = binomial(link = "log"), data = mothers_2)
exp(coef(m_DZ)); exp(confint(m_DZ     ))     






prev_TW <- mothers_2[!is.na(TW) & !is.na(sib_TW), .(total = .N, positive = sum(TW == 1)), by = sib_TW]
prev_TW[,prevalence := positive / total]
prev_TW[, se := sqrt((prevalence * (1 - prevalence)) / total)]
prev_TW[, `:=`(
  ci_lower = prevalence - z * se,
  ci_upper = prevalence + z * se
)]

print(prev_TW)
m_TW      <- glm(DZ ~ sib_TW, family = binomial(link = "log"), data = mothers_2)
exp(coef(m_TW)); exp(confint(m_TW     ))     




# Number of children
mothers_2[,.(mean_birth = mean(n_birth),sd_birth = sd(n_birth), mean_child = mean(n_children),sd_child = sd(n_children))]
mothers_2[!is.na(sib_TW),.(mean_birth = mean(n_birth),sd_birth = sd(n_birth), mean_child = mean(n_children),sd_child = sd(n_children))]
summary(lm(n_children ~ !is.na(sib_TW), mothers_2))
summary(lm(n_birth ~ !is.na(sib_TW), mothers_2))

mothers_2[!is.na(sib_TW), .(mean_n_child = mean(n_children), sd_n_child = sd(n_children)), by = sib_DZ]
mothers_2[!is.na(sib_TW), .(mean_n_child = mean(n_children), sd_n_child = sd(n_children)), by = sib_TW]

summary(lm(n_children ~ sib_DZ, data=mothers_2))
summary(lm(n_children ~ sib_TW, data=mothers_2))


summary(lm(n_children ~ sib_DZ, data=mothers_2[TW==F]))
summary(lm(n_children ~ sib_TW, data=mothers_2[TW==F]))
