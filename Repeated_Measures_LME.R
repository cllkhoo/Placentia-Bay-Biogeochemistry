## Question: Are the sites different to one another, while accounting for time?

---------------------------------------Linear Mixed Effect Approach----------------------------------------

#The approach can determine the effect of time and site on DOC (and other parameters)
##Site is a fixed effect because we visited the same location each time
##Time is a random effect because instantaneous sampling can't describe the entire period

library(stargazer)
library(psycho)
library(ggplot2)
library(ez)
library(lme4)
library(lmerTest)
library(tidyverse)
  
#Load Data by parameter
Data <- read.csv("LME_Data.csv")
Data_tidy <- DOC[-c(36:40),-c(2)] #Removed empty rows
str(Data) 

Test_DOC <- lmer(DOC_uM~Sample_Category + (1|Event_Number), data = Data) 
Test_DOC2 <- lm(DOC_uM~Event_Number + Sample_Category + Event_Number*Sample_Category, data = DOC_Data) 
summary(Test_DOC)
fixef(Test_DOC)
fit <- fitted(Test_DOC)
res <- residuals(Test_DOC)

plot(Test_DOC)
qqnorm(resid(Test_DOC))
qqline(resid(Test_DOC))
hist(resid(Test_DOC))

anova(Test_DOC)
summary(Test_DOC)
confint(Test_DOC)
coef(Test_DOC)

ls_means(Test_DOC)

library(multcompView)
library(agricolae)
library(rstatix)
DOC_plot <- lsmeans(Test_DOC, ~Sample_Category)
HSD.test(Test_DOC,"Sample_Category")

##Unpaired T-test
library(rstatix)
library(ggpubr)
library(tidyverse)
DOC <- pairwise_t_test(Data, DOC_uM~Sample_Category, paired = FALSE, pool.sd=FALSE, p.adjust.method = "bonferroni")
DOC_plot <- DOC %>% add_xy_position(x='Sample_Category', step.increase = 1)
ggboxplot(Data, x="Sample_Category", y="DOC_uM") + stat_pvalue_manual(DOC_plot,hide.ns=TRUE)

#Dunnett's test - tests one group against all the others
library(DescTools)
DunnettTest(x=Data$DOC_uM, g=Data$Sample_Category)


