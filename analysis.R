library(FrF2)
library(tidyverse)
library(readxl)

data <- read_csv("data.csv")

# check the index of ABC, BCD, and ABD
# Yates_names <- names(Yates)
# which(Yates_names == "ABD")

# create a design objective
# generators: ABC, BCD
# block generator: ABD
# default.levels = c("-", "+")
plan <- FrF2(nruns = 16, nfactors = 6,
     blocks = 2, block.name = "ABD",
     generators = c("ABC", "BCD"), randomize = TRUE, seed = 123) 
# summary(plan)

# response for each combination of factors
doe_mx <- plan %>% as.tibble() %>% select(-ABD)
factor_letters <- letters[1:6]

# initilization
factor_comb <- vector(mode = "character", nrow(doe_mx))
for (i in 1:nrow(plan)) {
  
  r <- doe_mx %>% slice(i) %>% unlist()
  factor_comb_idx <- which(r == 1)
  
  if (length(factor_comb_idx) == 0) {
    factor_comb[i] <- "(-1)"
  } else {
    factor_comb[i] <- factor_letters[factor_comb_idx] %>% str_c(collapse = "")
  }
}

# load response from the raw data (got from the experimentations)
exp_factor_comb <- data$`Treatment Combination`
matching_idx <- match(factor_comb, exp_factor_comb)
res_mean <- data$Mean[matching_idx]
res_var <- data$Variance[matching_idx]

# Enter response (mean)
plan_mean <- add.response(plan, res_mean)

# Enter response (variance)
plan_var <- add.response(plan, res_var)

### Visualizations

# main effeccts
# MEPlot(plan_mean)

# 2fi interaction plots
# IAPlot(plan_mean, abbrev = 5, show.alias = TRUE, lwd = 2, 
#        cex = 2, cex.xax = 1.2, cex.lab = 1.5)

# NPP (Daniel's plot)
# DanielPlot(plan_mean, code = TRUE, half = TRUE, alpha = 0.1)

# ANOVA
# my_anova <- lm(plan_mean)
# summary(my_anova)
