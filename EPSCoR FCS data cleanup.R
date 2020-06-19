library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggplot2)
#Importing one file to test filtering 
file1<-read.csv("EPSCoRJune.csv") 

str(file1)

file1 = file1[-c(1, 2, 3, 4, 5),]
# removing extra rows

colnames(file1) = file1[1, ] 
# the first row will be the header


header.true<-function(file1) %>%  
names(file1)<-as.character(unlist(file1[1,])) 
#changes the header names from numbers to labels
  
file1 <- file1[-1, ] 
# removes extra row of headers now that the actual headers are correctly labeled 
# does not work within brackets with the above section for some reason, only by itself. 


for(file1) {
  read.table(file1)
  blanks <- subset(file1, grepl("blank", file1$sample_id))
  blanks$pico <-as.character(blanks$pico)
  blanks$pico <-as.numeric(blanks$pico)
  blanks$nano <-as.character(blanks$nano)
  blanks$nano <-as.numeric(blanks$nano)
  blanks$synecho <-as.character(blanks$synecho)
  blanks$synecho <-as.numeric(blanks$synecho)
  blanks$hetbacteria <-as.character(blanks$hetbacteria)
  blanks$hetbacteria <-as.numeric(blanks$hetbacteria)
  mean(blanks$pico)
  unstained <- subset(file1, !grepl("_sybr", file1$sample_id))
  stained <- subset(file1, grepl("_sybr", file1$sample_id))
  unstained <- subset(unstained, !grepl("blank", unstained$sample_id))
  stained <- subset(stained, !grepl("blank", stained$sample_id))
  stained$synecho <- NA
  unstained$hetbacteria <- NA
  unstained$pico<-as.character(unstained$pico)
  unstained$pico <-as.numeric(unstained$pico)
  blankpico <-(mean(blanks$pico))
  unstained$pico <- unstained$pico - blankpico
  
  stained$pico<-as.character(stained$pico)
  stained$pico <-as.numeric(stained$pico)
  blankpico <-(mean(blanks$pico))
  stained$pico <- stained$pico - blankpico
  
  unstained$nano<-as.character(unstained$nano)
  unstained$nano <-as.numeric(unstained$nano)
  blanknano <-(mean(blanks$nano))
  unstained$nano <- unstained$nano - blanknano
  
  stained$nano<-as.character(stained$nano)
  stained$nano <-as.numeric(stained$nano)
  blanknano <-(mean(blanks$nano))
  stained$nano <- stained$nano - blanknano 
  
  unstained$synecho<-as.character(unstained$synecho)
  unstained$synecho <-as.numeric(unstained$synecho)
  blanksynecho <-(mean(blanks$synecho))
  unstained$synecho <- unstained$synecho - blanksynecho
  
  stained$hetbacteria<-as.character(stained$hetbacteria)
  stained$hetbacteria <-as.numeric(stained$hetbacteria)
  blankhet <-(mean(blanks$hetbacteria))
  stained$hetbacteria <- stained$hetbacteria - blankhet
}



file2 <- rbind(stained,unstained)
#this file is now complete, with blanks subtracted and some values set to NA.

library(RColorBrewer)
coul <- brewer.pal(3, "Set4") 
barplot(height=stained$hetbacteria, names=stained$sample_id, col=coul, main="Heterotrophic bacteria concentrations")

barplot(height=unstained$synecho, names=unstained$sample_id, col=coul, main="Synecho concentrations")  



write.csv(file2, "/Users/Emily/Desktop", row.names = FALSE)
