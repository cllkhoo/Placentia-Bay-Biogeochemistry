-----------------------------------------------------------------------------------PUBLICATION R CODE: FIGURES------------------------------------------------------------------------------------------

--Figures associated with "Size Fractionated Biogeochemical Constituents across adjacent coastal systems informs approaches for integrating small catchment studies into regional models" 
--submitted to L&O August 2022--

--Manuscript Authors: C.L.L.Khoo, R.E.Sipler, S.J.M.Faulkner, S.G.Boyd, M.Beheshti Foroutani, C.E.McBride, S.E.Ziegler--
--Code Author: C.L.L.Khoo (cllkhoo@mun.ca; celyn.khoo3@gmail.com)--

-----------------------------------------------------------------------------------------FIGURE INVENTORY-----------------------------------------------------------------------------------------------
-Figure 1: Site Map - done in ArcGIS
-Figure 2: Environmental Conditions - Hydrology and salinity time series
-Figure 3: DOM - Time series and Box and whisker plots of DOC, DON, DOC:DON, DFe, DFe:DOC, SUVA254, and S275-295 data
-Figure 4: PM concentrations - Time series and box and whisker plots of PC, PC:PN, d13C-PC, Chl-a, and PC:Chl-a data
-Figure 5: Correlogram: See Correlation Analysis R Script

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Clear the environment ->
rm(list=ls())

#Set working directory
setwd("/Users/celynkhoo/R Projects/MUN L&O Publication/Figures")

#Packages Needed
library(dplyr) #data manipulation package
library(lubridate) #aids with date and time in R
library(ggplot2) #data visualization package
library(ggpubr) #combined multiple graphs into one figure

---------------------------------------------------------------------------------------------FIGURE TWO-------------------------------------------------------------------------------------------------

--GOAL: Illustrate stream discharge (influences the freshwater and saline sites) and salinity (influences the saline sites) throughout the 2019 study period--

Discharge <- read.csv("PHR_CBC_Discharge2019.csv") #Load data and saves it as a name
str(Discharge) #Tells you the structure of each column. Eg. whether it is a date or factor - to change the structure, see R tips file
Discharge$Date <- as.Date(Discharge$Date, "%d/%m/%Y") #Recognises the date as a date

##Runoff Area Plot##
Hydro <- ggplot(Discharge, aes(x=Date, y=Runoff_mm.day, colour = River)) + #calls ggplot as graphing function and tells ggplot which data to include 
  geom_area(fill="gray20") + #Type of graph and in this case a fill colour for the area under the line -- can include geom_point etc.
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + #setting intervals along the x-axis when there are too many time points
  labs(y="Area Normalized Runoff (mm/day)") + #sets title for the y axis - for an x axis title, write as labs(y="", x="")
  scale_color_manual(values = c("skyblue4", "sienna2")) + #Fill colours -- can include manual scales or predetermined R colour scales - see colour sheet PDF +
  scale_y_continuous(limits = c(0,45)) + #scales the y axis
    theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
          panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = 'none',
          axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) #theme choice inclusing borders, grid types, legend types etc. - See theme PDF
#Save as EPS 600 (width) x 200 (height) - EPS files let you edit in Adobe Illustrator

##Figure for Salinity at both saline sites##
Ch1_Env.Cond <- Ch1[-c(1:10, 20:28),] #Removes the freshwater rows
Salinity <- ggplot(Ch1_Env.Cond, aes(x=Date, y=Salinity_psu)) + geom_point(aes(color=River), size=3, shape = 0) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,45)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(Salinity~(psu))) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))
        
#Combining salinity and hydrology graphs##
Fig2 <- ggarrange(Salinity + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Hydro2 + theme(plot.margin = margin(t = 1)), 
          ncol = 1, common.legend = TRUE, legend = "top", align = "v", heights = c(0.8,1))
#Combine figure into one plot using Illustrator

-------------------------------------------------------------------------------------------FIGURE THREE------------------------------------------------------------------------------------------------

--GOAL: Illustrate how DOM constituents vary with space and time--

Data1 <- read.csv("Ch1_DataSubset.csv")
Ch1 <- Data1[-c(37,38,39,40,41),] #Deleting Rows from the dataframe
Ch1$Date <- as.Date(Ch1$Date,"%d/%m/%Y") #Recognising the date
str(Ch1)

##DOC FIGURES##
#DOC vs. Time, Grouped by Site - Time Series#
DOC_conc <- ggplot(Ch1, aes(x=Date, y=DOC_uM)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,1200)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(DOC~(paste(mu,M)))) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 
#Save as 700 x 400 as JPEG

#DOC vs. Site Boxplot#
DOC_conc2 <- ggplot(Ch1, aes(x=Sample_Site, y=DOC_uM, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,1200)) + xlab("Site") + ylab(expression(DOC~(paste(mu,M)))) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

##DON FIGURES##
#DON vs. Time, Grouped by Site - Time Series#
DON_conc <- ggplot(Ch1, aes(x=Date, y=DON_uM)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,25)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(DON~(paste(mu,M)))) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#DON vs. Site Boxplot#
DON_conc2 <- ggplot(Ch1, aes(x=Sample_Site, y=DON_uM, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,25)) + xlab("Site") + ylab(expression(DON~(paste(mu,M)))) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

#DOC:DON FIGURES
#C:N Ratio vs. Time, grouped by Site - Time Series#
CN_Ratio1 <- ggplot(Ch1, aes(x=Date, y=DOC_DON, group='Sample_Category')) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,75)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab("DOC:DON") +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#C:N Ratio Boxplot#
CN_Ratio2 <- ggplot(Ch1, aes(x=Sample_Site, y=DOC_DON, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,75)) + xlab("Site") + ylab("DOC:DON") + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

##METALS FIGURES##
#Fe vs. Time, grouped by Site - Time Series#
Fe_conc. <- ggplot(Ch1, aes(x=Date, y=Fe_uM, group='Sample_Category')) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,10)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(Fe~(paste(mu,M)))) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
              panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
              axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 
#Save as 700 x 400 as JPEG 

#Fe vs. Site Boxplot#
Fe_conc2 <- ggplot(Ch1, aes(x=Sample_Site, y=Fe_uM, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,10)) + xlab("Site") + ylab(expression(Fe~(paste(mu,M)))) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

#Iron to DOC ratio vs. Time, grouped by Site - Time Series#
Fe_DOC2 <- ggplot(Ch1, aes(x=Date, y=Fe_DOC, group='Sample_Category')) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,0.0125)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab("Fe:DOC") +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 
        
#Fe:DOC Boxplot#
Fe_DOC3 <- ggplot(Ch1, aes(x=Sample_Site, y=Fe_DOC, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,0.0125)) + xlab("Site") + ylab("Fe:DOC") + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")
 
##ABSORBANCE FIGURES##
#SUVA vs. time, grouped by Site - Time Series#
SUVA1 <- ggplot(Ch1, aes(x=Date, y=SUVA254, group='Sample_Category')) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,6)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(SUVA[254])) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 
#Save as 700 x 400 as JPEG 

#SUVA Boxplot#
SUVA2 <- ggplot(Ch1, aes(x=Sample_Site, y=SUVA254, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,6)) + xlab("Site") + ylab(expression(SUVA[254])) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

#Sr vs. time, grouped by Site - Time Series#
Sr1 <- ggplot(Ch1, aes(x=Date, y=Sr, group='Sample_Category')) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,2.5)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(S[275-295]/S[350-400])) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 
#Save as 700 x 400 as JPEG 

#Sr Boxplot#
Sr2 <- ggplot(Ch1, aes(x=Sample_Site, y=Sr, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,2.5)) + xlab("Site") + ylab(expression(S[275-295]/S[350-400])) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

  
#Combining DOM time series and box and whisker plots - For Publication#
Fig3 <-
ggarrange(DOC_conc + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          DOC_conc2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          DON_conc + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          DON_conc2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          CN_Ratio1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          CN_Ratio2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Fe_conc. + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Fe_conc2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Fe_DOC2 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Fe_DOC3 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          SUVA1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          SUVA2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Sr1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          Sr2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
          ncol = 2, nrow= 7, common.legend = TRUE, legend = "top", align = "hv", widths = c(0.75, 0.25), heights = c(0.8,0.8,0.8,0.8,0.8,0.8,0.8)) 
#Save as 900 (width) x 1000 (height) as EPS

-------------------------------------------------------------------------------------------FIGURE FOUR------------------------------------------------------------------------------------------------

--GOAL: Illustrate how PM constituents vary with space and time--

##Parculate Carbon Figures##
PC_Conc1 <- ggplot(Ch1, aes(x=Date, y=PC_uM)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,60)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(PC~(paste(mu,M)))) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#PC vs. Site Boxplot
PC_Conc2 <- ggplot(Ch1, aes(x=Sample_Site, y=PC_uM, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,60)) + xlab("Site") + ylab(expression(PC~(paste(mu,M)))) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")
  
##Stable Isotope of POM##
del13C_1 <- ggplot(Ch1, aes(x=Date, y=del13C_permil)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(-35,-15)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(delta^13~C~PC)) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#del13C vs. Site Boxplot
del13C_2 <- ggplot(Ch1, aes(x=Sample_Site, y=del13C_permil, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(-35,-15)) + xlab("Site") + ylab(expression(delta^13~C~PC)) +
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

##Chlorophyll-a##
Chla_1 <- ggplot(Ch1, aes(x=Date, y=Chl.a_uM)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,0.4)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression(Chl-a)) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#Chl-a vs. Site Boxplot
Chla_2 <- ggplot(Ch1, aes(x=Sample_Site, y=Chl.a_uM, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,0.4)) + xlab("Site") + ylab(expression(Chl-a)) +
theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")

##Particulate Ratios##
#PC:Chl-a time series
P_Ratio1 <- ggplot(Ch1, aes(x=Date, y=PC.Chla)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,10000)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression("PC:Chl-a")) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#PC:Chl-a boxplot
P_Ratio2 <- ggplot(Ch1, aes(x=Sample_Site, y=PC.Chla, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,10000)) + xlab("Site") + ylab(expression("PC:Chl-a")) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")    
  
#PC:PN time series
P_Ratio5 <- ggplot(Ch1, aes(x=Date, y=PC.PN)) + geom_point(aes(color=River, shape=Sample_Site), size=3) + 
  scale_x_date(limits = c(min, max), breaks = "15 days", date_labels = "%d-%b") + scale_y_continuous(limits = c(0,25)) +
  scale_colour_manual(values = c("skyblue4", "sienna2")) + ylab(expression("PC:PN")) +
  theme(axis.text = element_text(colour = 'black', size=12), axis.title = element_text(colour = 'black', size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "top", 
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) 

#PC:PN boxplot
P_Ratio6 <- ggplot(Ch1, aes(x=Sample_Site, y=PC.PN, fill=Sample_Category)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue4", "skyblue4", "sienna2", "sienna2")) + theme_classic() + scale_x_discrete(limits=c("Freshwater","Marine")) +
  scale_y_continuous(limits = c(0,25)) + xlab("Site") + ylab(expression("PN:Chl-a")) + 
  theme(panel.border = element_rect(fill = NA, color = "black", size = 1), legend.position = "none")
  
#Combining PC and PC:Chl-a time series figures
Fig4 <- ggarrange(PC_Conc1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  PC_Conc2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  P_Ratio5 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  P_Ratio6 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  del13C_1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  del13C_2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  Chla_1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  Chla_2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  P_Ratio1 + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  P_Ratio2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(b = 1, r = 4)),
                  ncol = 2, nrow= 5, common.legend = TRUE, legend = "top", align = "hv", widths = c(0.75, 0.25), heights = c(0.8,0.8,0.8,0.8,0.8))
#Save as 900 (width) x 800 (height) as EPS - will become an illustrator file
          
        
