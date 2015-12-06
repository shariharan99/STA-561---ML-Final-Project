#!/usr/bin/Rscript
# this code was written by Azeem Zaman
# contact: [first] . [last] @ duke (dot) edu

# get names of files in directory with imputed data
data.files <- list.files(path = '/home/grad/srs65/521-final-project/Complete_Data_wdiver_Nov19',
                         full.names = TRUE)
# read all data tables into list
list.of.data <- lapply(data.files, read.table)
# extract the diversity column from all data tables
list.of.diver <- lapply(list.of.data, function(x) return(x$diver))
# bind into data frame
diver.df <- do.call(cbind, list.of.diver)

# randomly sample 4 data sets for histograms
sample1 <- sample(1:length(data.files), 4)
sample2 <- sample(1:length(data.files), 4)

# make titles for histograms
hist.names1 <- sapply(sample1, function(x) paste("Histogram of data set", x, sep = " "))
hist.names2 <- c(paste("Data sets", sample2[1], "and", sample2[2], sep = " "),
                 paste("Data sets", sample2[3], "and", sample2[4], sep = " "))
# plot 4 histograms in a single image, each of a different data set
pdf("diver4hists.pdf")
par(mfrow = c(2,2))
hist(diver.df[,sample1[1]], main = hist.names1[1], xlab = "Diversity")
hist(diver.df[,sample1[2]], main = hist.names1[2], xlab = "Diversity")
hist(diver.df[,sample1[3]], main = hist.names1[3], xlab = "Diversity")
hist(diver.df[,sample1[4]], main = hist.names1[4], xlab = "Diversity")
dev.off()

# plot two histograms with the difference between two data sets in single image
pdf("diver2hists.pdf")
par(mfrow = c(1,2))
hist(diver.df[,sample2[1]]-diver.df[,sample2[2]],
     main = hist.names2[1],
     xlab = "Difference")
hist(diver.df[,sample2[3]]-diver.df[,sample2[4]],
     main = hist.names2[2],
     xlab = "Difference")
dev.off()