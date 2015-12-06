#This script imputes data for the missing information on the graduation dataset
#based on the file 'na_racial_data.Rdata'
load("data/na_racial_data.Rdata")

library(mice)

#Use the mice package to impute 50 different dataset
miceData <- mice(na_racial_data,m=50,maxit=20,meth='pmm',seed=500)

setwd("data/Mice_Data")

#Extract the imputed dataset
Dataset_mice =list()
for (i in 1:50){
  Dataset_mice[[i]]<- complete(miceData,i)
}

#Save the imputed dataset onto the drive
for(i in 1:50){
  write.table(Dataset_mice[[i]], file = paste("micedata",i,sep="")) 
}

