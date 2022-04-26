#Set working directory
setwd("~/R Projects/ChapterOne_StatisticalAnalyses")

#Packages
library(car)

### NESTED (HIERARCHICAL) ANOVA

Data <- read.csv("Data.csv")
Data_tidy <- Data[-c(34:35),] #Removed empty rows
Data_tidy$Site <- as.factor(Data_tidy$Site) #Tells R to treat site as a factor
Data_tidy$Sampling_Event <- as.factor(Data_tidy$Sampling_Event)
str(Data_tidy) 


model <- aov(PC.Chla~Sampling_Event + River_Estuary + WBT + Site%in%WBT, Data_tidy)

#Tests for normality
shapiro.test(residuals(model))
hist(resid(model)) #Visual for normality
plot(model, 2) #QQ plot  for normality

#Tests for homogeneity
plot(model, 1) #Residuals vs. fitted

#Summary tables
Anova(model)
summary(model)
