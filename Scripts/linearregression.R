# This code was written by Azeem Zaman
# please contact [first] . [last] @ duke (dot) edu with questions

library(xtable)
data.files <- list.files(path = 'data/Complete_Data_wdiver_Nov19',
                         full.names = TRUE)
list.of.data <- lapply(data.files, read.table)

# function to do this
computeSummary <- function(formula, list.of.data, my.var){
###########################################################
# This function runs a specified regression on list of data.  It is designed
# to be used on MICE data, i.e. when there are mulitple data sets with 
# imputed values and an average of regressions on all of the data sets is 
# desired.
#
# Inputs:
#   -formula:  the regression formula as a string
#   -list.of.data:  a list containing all of the data sets to be analyzed
#   -my.var:  a string variable name that will be summarized
#
# Outputs
#   -A vector containing the mean of the estimate, the variance of the estiamte
#    the average std error, t stastistic, and p value for the estimate
###########################################################
  reg.formula <- as.formula(formula)
  list.of.regs <- lapply(list.of.data, function(x) {lm(reg.formula, data = x)})
  coef.vals <- sapply(list.of.regs, function(x){coef(x)[my.var]})
  coef.mean <- mean(coef.vals)
  coef.var <- var(coef.vals)
  summary.stats <- lapply(list.of.regs, function(x) {summary(x)$coefficients[my.var,]})
  summary.df <- do.call(rbind, summary.stats)
  df.vals <- colMeans(summary.df)
  df.vals <- c(coef.mean, coef.var, df.vals[-1])
}
# a list of the regression we running
list.of.formula <- c("ALL_RATE_1112 ~ diver",
                     "ALL_RATE_1112 ~ diver + Aggregate_HH_INC_ACS_08_12",
                     "ALL_RATE_1112 ~ diver*Aggregate_HH_INC_ACS_08_12",
                     "ALL_RATE_1112 ~ diver + Prs_Blw_Pov_Lev_ACS_08_12",
                     "ALL_RATE_1112 ~ diver * Prs_Blw_Pov_Lev_ACS_08_12",
                     "ALL_RATE_1112 ~ diver * Prs_Blw_Pov_Lev_ACS_08_12 + diver*Aggregate_HH_INC_ACS_08_12")
# get list of summary vectors
res <- lapply(list.of.formula, function(x) {computeSummary(x, list.of.data, my.var = 'diver')})
# bind into data frame
df <- do.call(rbind, res)
# name rows and columsn
colnames(df) <- c("Mean", "Variance", "Std. Error", "t value", "p value")
rownames(df) <- c("Diversity", "Diversity + Median Income", "Diversity + Income interaction")
# displays the summary table in latex 
xtable(df) 