

library(tidyverse)
library(evd)
library(data.table)

#create zipcode-gender possibilities

zipcodes <- data.frame(
    cbind(rep(c("A" ,"B","C", "D" ,"E","F"), 2),
        c(rep(1, 6), rep(0,6))))
names(zipcodes) <- c("zipcode", "gender")

#for each cell, expand 14 times (each row is a student)
df.expanded <- zipcodes[rep(row.names(zipcodes), 14), 1:2]
df.expanded$studentID <- 1:nrow(df.expanded)

#read in distiance matrix
distances <- read.csv2("distances.csv")
distancesLong <- pivot_longer(distances, cols = starts_with("University"))
names(distancesLong) <- c("zipcode","alternative", "time")

logitdata <- merge(df.expanded,distancesLong,by="zipcode",all=T)
logitdata$gender <- as.numeric(as.character(logitdata$gender))

coeftime <- -0.10
gendercoef <- -.05

set.seed(1212)
logitdata$timeValuated <- exp(logitdata$time * coeftime +  
                          logitdata$time * coeftime * gendercoef + rgumbel(n=nrow(logitdata), loc=0, scale=1))

#max value per id is choice
logitdata <- data.table(logitdata)
logitdata[, maxValue:=max(timeValuated), studentID]
logitdata[,choice:=0]
logitdata[maxValue==timeValuated, choice:=1]

#check
sum(logitdata$choice)== logitdata[, uniqueN(studentID)]

#variation?
logitdata[choice==1,.N,time]
logitdata[time==0,.N,choice]

#clean
logitdata <- logitdata[,.(studentID, zipcode, gender, alternative, time, choice)]
logitdata <- logitdata[order(studentID, zipcode)]

write.csv2(logitdata, "logitdata.csv", row.names = FALSE)
