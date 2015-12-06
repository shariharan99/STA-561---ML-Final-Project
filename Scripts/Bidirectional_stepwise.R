library(MASS)
# Read in the first dataset
data_1_stepwise <- read.table("data/Complete_Data_wdiver_Nov19/dataset1")

# Take away the leaid column
data_1_stepwise <- data_1_stepwise[,-1]

# Fit the regression and run bidirectional stepwise using AIC criterion
# It will take about 8 hours to run on a single dataset
fit = lm(ALL_RATE_1112~., data = data_1_stepwise)
step <- stepAIC(fit, direction="both")