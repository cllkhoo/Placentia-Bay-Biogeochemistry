## Question: Which OM variables best describe DOC and PC concentrations in each Placentia Bay study site?;
## Can we describe the relationships between POM and DOM in these sites?

---------------------------------------Multiple Linear Regression - Stepwise Method----------------------------------------

#Multiple linear regressions give information on:
  #1. The relationship between the criterion variable and all predictor variables
  #2. The variance in the criterion that can be explained by the predictor variables acting together
  #3. How much each predictor variable contributes to the criterion variable 
  
#The stepwise regression method instructs the computer to enter the predictor variables in various combinations until a 
  #"best" equation is found

#Load Data and Split into sites
Ch1 <- read.csv("Ch1_DataSubset.csv")
Ch1_Data <- Ch1[-c(37:41),] #Removed empty rows
Ch1_PHR1 <- Ch1_Data[-c(1, 11:36),] #Removed all columns except PHR1
Ch1_PHR3 <- Ch1_Data[-c(1:10, 20:36),] #Removed all columns except PHR3
Ch1_CBC1 <- Ch1_Data[-c(1:20, 29:36),] #Removed all columns except CBC1
Ch1_CBC3 <- Ch1_Data[-c(1:28, 32),] #Removed all columns except CBC3

###Piper's Hole Freshwater DOC
#Step 1: Formulate a regression with all the variables
FitAll_PHR1a <- lm(DOC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + PC_uM + del13C_permil, data = Ch1_PHR1) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_PHR1a <- lm(DOC_uM~1, data=Ch1_PHR1) 
summary(FitStart_PHR1a)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_PHR1a, direction = "both", scope = formula(FitAll_PHR1a))

###Piper's Hole Freshwater PC
#Step 1: Formulate a regression with all the variables
FitAll_PHR1b <- lm(PC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + DOC_uM + del13C_permil, data = Ch1_PHR1) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_PHR1b <- lm(PC_uM~1, data=Ch1_PHR1) 
summary(FitStart_PHR1b)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_PHR1b, direction = "both", scope = formula(FitAll_PHR1b))

###Piper's Hole Saline DOC
#Step 1: Formulate a regression with all the variables
FitAll_PHR3a <- lm(DOC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + PC_uM + del13C_permil, data = Ch1_PHR3) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_PHR3a <- lm(DOC_uM~1, data=Ch1_PHR3) 
summary(FitStart_PHR3a)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_PHR3a, direction = "both", scope = formula(FitAll_PHR3a))

###Piper's Hole Saline PC
#Step 1: Formulate a regression with all the variables
FitAll_PHR3b <- lm(PC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + DOC_uM + del13C_permil, data = Ch1_PHR3) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_PHR3b <- lm(PC_uM~1, data=Ch1_PHR3) 
summary(FitStart_PHR3b)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_PHR3b, direction = "both", scope = formula(FitAll_PHR3b))

###Come By Chance Freshwater DOC
#Step 1: Formulate a regression with all the variables
FitAll_CBC1a <- lm(DOC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + PC_uM + del13C_permil, data = Ch1_CBC1) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_CBC1a <- lm(DOC_uM~1, data=Ch1_CBC1) 
summary(FitStart_CBC1a)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_CBC1a, direction = "both", scope = formula(FitAll_CBC1a))

###Come By Chance Freshwater PC
#Step 1: Formulate a regression with all the variables
FitAll_CBC1b <- lm(PC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + DOC_uM + del13C_permil, data = Ch1_CBC1) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_CBC1b <- lm(PC_uM~1, data=Ch1_CBC1) 
summary(FitStart_CBC1b)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_CBC1b, direction = "both", scope = formula(FitAll_CBC1b))

###Come By Chance Saline DOC
#Step 1: Formulate a regression with all the variables
FitAll_CBC3a <- lm(DOC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + PC_uM + del13C_permil, data = Ch1_CBC3) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_CBC3a <- lm(DOC_uM~1, data=Ch1_CBC3) 
summary(FitStart_CBC3a)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_CBC3a, direction = "both", scope = formula(FitAll_CBC3a))

###Come By Chance Saline PC
#Step 1: Formulate a regression with all the variables
FitAll_CBC3b <- lm(PC_uM ~ Julian_Day + Runoff + Fe_uM + SUVA254 + Sr + Chl.a_uM + DOC_uM + del13C_permil, data = Ch1_CBC3) 
#Step 2: Formulate an intercept only model that we will add variables to
FitStart_CBC3b <- lm(PC_uM~1, data=Ch1_CBC3) 
summary(FitStart_CBC3b)
#Step 3: Tell R to formulate the best model, combining both backward and forward selection
step(FitStart_CBC3b, direction = "both", scope = formula(FitAll_CBC3b))

plot(lm(PC_uM ~ Chl.a_uM + del13C_permil + DOC_uM + Fe_uM + Julian_Day + Runoff, data = Ch1_CBC3))
