library(plyr)
library(dplyr)
library(tidyverse)


#This script generates data for which instances of the 24HR
#that UKB participants took.
#https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20080

source('../ukb34137_loaddata.r') #15 min
bd <- as_tibble(bd)

#Get 24h recall taken data-----------
daycols<-c("f.20080.0.0", "f.20080.1.0", "f.20080.2.0",
           "f.20080.3.0", "f.20080.4.0")

#Change all the values to characters for easier manipulation
bd1<-bd[,c("f.eid", daycols)]
bd1[,daycols]<-as_tibble(sapply(bd1[,daycols], as.character))
#Change NA's to zeros and days to 1's
bd1[,daycols][(is.na(bd1[,daycols]))]<-"0"
bd1[,daycols][bd1[,daycols]!="0"] <-"1"
#Change these back to numeric
bd1[,daycols]<-sapply(bd1[,daycols], as.integer)
bd1<-as_tibble(bd1)
#Now make a new column, everyone with rowSums zero write FALSE
#and those with >0 write TRUE
sum<-apply(bd1[,daycols], 1, sum)
bd1$took_24HR<- sum
bd1$took_24HR[bd1$took_24HR>0]<-1
sum(bd1$took_24HR) #[1] 211018
colnames(bd1)<-c("IID", "RecallInstance0", "RecallInstance1",
                 "RecallInstance2","RecallInstance3","RecallInstance4",
                 "ever_took_24HR")

write.table(bd1, "UKB_24hourRecall-ParticipantInstancesTaken.txt",
            quote=FALSE, row.names=FALSE)
