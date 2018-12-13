# NCSS_Characterization_Analysis
## Code to import NCSS characterization pedons, clean dataset, create visualizations, and perform statistics.
## R 3.5.1
## This script was my exploratory analysis for a paper I'm currently working on.
## Temporal Dynamics of the Tama Soil Series: A Mandate for Reevaluation of the Central Series Concept ##Currently being edited ##12/13/2018
## Load Library
library(aqp)
library(plyr)
library(soilDB)
library(sharpshootR)
library(igraph)
library(latticeExtra)
library(lattice)
library(cluster)
library(MASS)
library(RColorBrewer)
library(plotrix)
library(ggplot2)

## Import Tama Pedons from NCSS
s <- fetchKSSL(series='Tama')

## Get bottom soil depth for pedons
s$soil.depth <- profileApply(s, FUN=max)

## conditional statment to replace missing bottom horizon depths with top horizon depths
s$hzn_bot[!is.na(s$hzn_top) & is.na(s$hzn_bot)] <- s$hzn_top[!is.na(s$hzn_top) & is.na(s$hzn_bot)]

## quick initial analysis of pedon graphics; colored via organi carbon values
plot(s, name='hzn_desgn', cex.names=0.85, axis.line.offset=-4, color='oc')

## #########################################This portion is due to my lack of R skills##############################################
## Priority #1: Learn how to write a function that calculates horizon thickness####################################################

## Until then we write to excel files and perform calculations with pivot tables

## export site data
## carbon is not a great name here; probably should switch to Tama
write.xlsx(s@site, file = 'scarbon.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
           
## export horizon data (from which we use pivot table to get horizon thickness)
write.xlsx(s@horizons, file = 'scarbonhz.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

## years have been renamed in excel after looking at s@site$pedon_id
## import excel files (import dataset in RStudio); does not work on Terra, need to update Java to 64-bit; 12 Dec 2018

## year group: "pre-1960", "'64 to '69", "'71 to '76", "'78 to '83", "'84 to '88"
## merge vectors to @site for s Soil Profile collection; (must be ordered!!!!)
s$yr <- scarbonyr$yr
s$year <- scarbonyr$year
## merge A thickness calculated using pivot tables and horizon data excel file
s$A_thickness <- scarbonyr$A_thick

## simple box plots to look at distribution of A horizons per grouped year
boxplot(s$A_thickness ~ s$yr)
boxplot(s$A_thickness ~ s$yr)

## ##########################################Challenge 2: Need better R skills to clean these data#################################
## Until then; we subset bad ones after analyzing the profile plot
## pedon 71270 has bad horizonation## apparently the first code didnt fix it; had overlapping boundaries Challenge 3
s2 <- subsetProfiles(s, s = 'pedon_key != 71270')
## analysis shows that pre-1960 pedons have a multitude of issues with incomplete data
## this subset is used for the rest of the analysis (s3)
s3 <- subsetProfiles(s2, s = 'yr != "pre-1960"')
## I was curious how Tamas only sampled in Iowa compared to the entire population; did not use in analysis
SIA <- subsetProfiles(s3, s = 'state == "Iowa"')
## other subsetting that was not used; pedons in Iowa; bottom depth less than 200 cm
SIA2 <- subsetProfiles(SIA, s = 'soil.depth <= 200')
## initially tried to analyze pedons that had soc data; this subset is inefficient
s5 <- subsetProfiles(s4, h = 'oc > 0')
## here's a better method; which means "not equal to NA value"
s3_no_soc <- s3[which(!is.na(s3$oc)), ]

#######################################Depth Function Portion##########################################################################
## s3 is the cleaned dataset; now we slice SOC, clay content and pH 1:1 into 1 cm depth increments to 200 cm 
ssl <- slice(s3, fm= 0:200 ~ oc + clay + ph_h2o)
## Slabbing groups these depth slices based on fm = yr ~; yr is the grouped year class
Tamaslb <- slab(ssl, fm= yr ~ oc + clay + ph_h2o)

## Double-check profile plot for any disparities before continuing
plot(s3, name='hzn_desgn', cex.names=0.85, axis.line.offset=-4, color='oc')

## profiles can also be grouped by year; my year variable is the absolute year (1976)
groupedProfilePlot(SIA2, 'year', name = 'hzn_desgn', label='pedon_id', id.style='side')

## Plots the depth function by grouped by year class; median is bounded by 25th and 75th percentiles
xyplot(top ~ p.q50 | variable, groups=yr,  data=Tamaslb, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       ### bottom ylim of graphic goes to 150 cm 
       lower=Tamaslb$p.q25, upper=Tamaslb$p.q75, ylim=c(150,-2),
       panel=panel.depth_function,
       ##contributing fraction is what percentage of data came from the pedon population cf.interval = 10 means percentage contribution        ## per 10 cm
       alpha=0.15, sync.colors=TRUE, cf=Tamaslb$contributing_fraction, cf.interval=10,
       ## Four grouped time classed; so we need 4 colors
       par.settings=list(superpose.line=list(lwd=2, col=c('Green', 'Blue', 'Black', 'Red'))),
       prepanel=prepanel.depth_function,
       ## 3 soil variables so 3 columns and 1 row
       layout=c(3,1), strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=4, alternating=3, relation='free')),
       ## 4 year groups; 4 columns for legend at top of graphic
       auto.key=list(columns=4, lines=TRUE, points=FALSE)
)

####################################Making pretty boxplots with ggplot2################################################################
### So my groups wouldn't show on the graph in Temporal order; so i had to make an ordered factor attribute called yr2
s$yr2<- factor(s$yr, levels = c("pre-1960", "'64 to '69", "'71 to '76", "'78 to '83", "'84 to '88"))
## anytime you use title = that becomes the title; so you can't have multiple title objects unless you say titlename <- title = "Title"
title = "Tama A Horizons from NCSS Characterization Database"
## set base theme
theme = theme_set(theme_minimal())
## This creates the legend; still a work in progress; title isn't working 12/13/2018
theme = theme_update(legend.position="top", legend.title="Grouped Year", panel.grid.major.x=element_blank())
## call variables for the boxplot i.e. A horizon thickness and grouped year
boxplot = ggplot(s@site, mapping=aes_string(y = "A_thickness", x = "yr2")) + ggtitle(title) + ylab("A horizon (cm)")
## This controls the colors and geometry for the boxplot; but i'm not entirely sure how to customize yet
boxplot = boxplot + geom_boxplot(outlier.colour = NULL, aes_string(colour="yr", fill="yr")) +
stat_summary(geom = "crossbar", width=0.75, fatten=3, color="Gray", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })
## set theme
theme = theme_set(theme_minimal())
## I don't need grouped year labels; this removes them on the x axis
theme = theme_update(axis.title.x=element_blank())

## take a look at the pretty boxplot
boxplot

## Grays all the content
boxplot + theme(
  plot.title = element_text(color="Dark Gray", size=14, face="bold"),
  axis.title.y = element_text(color="Dark Gray", size=14, face="bold"),
  axis.text.y = element_text(color="Dark Gray", size=12, face="bold"),
  axis.text.x = element_text(color="Dark Gray", size=12, face="bold")
  
)

boxplot
## ################################Actual analysis for paper Pre-1976 Pedons versus Post-1976 Pedons##########################

## Work flow is the same as above; I reclassified the pedons in excel for year populations
s$yr3 <- scarbonyr3$yr3
s$yr3 <- factor(s$yr3, levels = c("pre-1976", "post-1976"))
s3$yr3 <- factor(s3$yr3, levels = c("pre-1976", "post-1976"))


## Slice and Slab to 200 cm
ssl2 <- slice(s3, fm= 0:200 ~ oc + clay + ph_h2o)
## group between pre and post 1976
Tamaslb2 <- slab(ssl2, fm= yr3 ~ oc + clay + ph_h2o)

## quick profile plot check for errors
plot(s3, name='hzn_desgn', cex.names=0.85, axis.line.offset=-4, color='oc')
groupedProfilePlot(Apost1976, 'year', name = 'hzn_desgn', label='pedon_id', id.style='side')

xyplot(top ~ p.q50 | variable, groups= yr3,  data=Tamaslb2, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=Tamaslb2$p.q25, upper=Tamaslb2$p.q75, ylim=c(200,-2),
       panel=panel.depth_function,
       alpha=0.15, sync.colors=TRUE, cf=Tamaslb2$contributing_fraction, cf.interval=10,
       ##2 grouped years; 2 colors
       par.settings=list(superpose.line=list(lwd=2, col=c('Blue', 'Red'))),
       prepanel=prepanel.depth_function,
       layout=c(3,1), strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=4, alternating=3, relation='free')),
       ##2 items in legend; 2 columns
       auto.key=list(columns=2, lines=TRUE, points=FALSE)
)

title = "Tama A Horizons from NCSS Characterization Database"
theme2 = theme_set(theme_minimal())
#theme = theme_update(legend.position="top", legend.title="Grouped Year", panel.grid.major.x=element_blank())
boxplot2 = ggplot(s@site, mapping=aes_string(y = "A_thickness", x = "yr3")) + ggtitle(title) + ylab("A horizon (cm)")
boxplot2 = boxplot2 + geom_boxplot(outlier.colour = NULL, aes_string(colour="yr3", fill="yr3")) +
stat_summary(geom = "crossbar", width=0.75, fatten=3, color="Gray", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })
theme2 = theme_set(theme_minimal())
theme2 = theme_update(axis.title.x=element_blank())
boxplot2 + theme(
  plot.title = element_text(color="Dark Gray", size=14, face="bold"),
  axis.title.y = element_text(color="Dark Gray", size=14, face="bold"),
  axis.text.y = element_text(color="Dark Gray", size=12, face="bold"),
  axis.text.x = element_text(color="Dark Gray", size=12, face="bold")
)

boxplot2
###################################Statistical Test (Welch's T-test)############################################################
## subset to get the vector list for statistical comparison
## this is not the most efficient way to subset to vector because it creates a new SPC from which I subset again in the $site column to ##get the vector
Apre1976 <- s3[s3$A_thickness]
Apre1976 <- s3[which(s3$yr3 =='pre-1976' &  !is.na(s3$A_thickness)), ]
Apost1976 <- s3[which(s3$yr3 =='post-1976' & !is.na(s3$A_thickness)), ]

## make vectors
Apre76 <- Apre1976$A_thickness
Apos76 <- Apost1976$A_thickness

## Welch's t; unequal observations; unequal variances; two-tailed test(higher or lower)
ttest<- t.test(Apre76, Apos76, alternative = "two.sided", var.equal = FALSE)

### summary stats; i should code this to make a data table
descpre76 <- describe(Apre76)
descpos76 <- describe(sqrt(Apos76))

## checking normality and square-root transformation
hist(sqrt(Apos76))
hist(sqrt(Apre76))

## Written to csv to analyze the slab information; I averaged medians by group class for better discussion in the paper
## groups (0 to 10 cm , 10 to 20 cm, 30 to 50 cm, 50 to 100 cm, 100 to 150 cm, 150 to 200 cm). 
csvpre <- write.csv(Tamaslb2, "Tamaslb2.csv")
