#Set working directory
setwd("~/R Projects/ChapterOne_StatisticalAnalyses")

#Packages
library(corrplot)
library(RColorBrewer)
library(PerformanceAnalytics)
library(tidyverse)
library(Hmisc)
library(pals)

#Load and tidy data
Data <- read.csv("Data.csv")
Data_tidy <- Data[-c(34:35),] #Removed empty rows
Ch1_PHR1 <- Data_tidy[-c(10:33),c(6:19)] #Removed all columns except PHR1
Ch1_PHR3 <- Data_tidy[-c(1:9, 19:33),c(5,7:19)] #Removed all columns except PHR3
Ch1_CBC1 <- Data_tidy[-c(1:18, 27:33),c(6:19)] #Removed all columns except CBC1
Ch1_CBC3 <- Data_tidy[-c(1:27),c(5,7:19)] #Removed all columns except CBC3

#Run Correlation Analysis
Correl <- cor(Ch1_PHR3, method = c("kendall"))

#Plotting a correlogram
col <- brewer.rdylbu(100)
cor_5 <- rcorr(as.matrix(Correl))
m <- cor_5$r
p_mat <- cor_5$P
corrplot(Correl, method = "color", col = col, type = "upper", tl.col = "black", number.cex = 0.5, tl.cex = 0.6,
         p.mat = p_mat, sig.level = 0.05, insig = "blank", diag = FALSE, addCoef.col = "black")
#Save as EPS (Illustrator File) 1000 x 467 


