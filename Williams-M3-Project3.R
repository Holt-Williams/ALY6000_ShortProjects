#G. Holt Williams

#PROJECT 3 R-script
#Print your name at the top of the script   
print("G. Holt Williams")
def_par<- par()

# IMPORT Libraries: FSA, FSAdata, magrittr, dplyr, tidyr, plyr, tidyverse
# install.packages("FSA")
# install.packages("FSAdata")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("plyr")
# install.packages("tidyverse")
# 
library(FSA)
library(FSAdata)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)

#A. CREATE 4 vectors height, weight, sex, married. The values for the 
#vectors: 
#age: (65, 72, 78, 69, 70) ##I assume this is height in inches?,
#weight: (150, 120, 250, 175, 180)
#sex: ("M", "F", "M","M", "M")
#married: (TRUE, FALSE, FALSE, TRUE, #TRUE)

height<-c(65, 72, 78, 69, 70)
weight<-c(150, 120, 250, 175, 180)
sex<-c("M", "F", "M","M", "M")
married<-c(TRUE, FALSE, FALSE, TRUE, TRUE)

#B. CREATE and DISPLAY a vector < age > 30, 22,45,27,33 with associated 
# labels Jim, Sally, Paul, Sam and John 
#Your display should appear as 
#Jim Sally  Paul   Sam  John 
#30        22     45      27      33
labels<- c("Jim", "Sally", "Paul", "Sam" ,"John")
age <- c(30,22,45,27,33)
names(age)<- labels
age

#C  SELECT and DISPLAY the second entry in < age > vector
age[2]

#D SELECT and DISPLAY the 1st and 5th elements in < age >
age[c(1,5)]

#E. SUBTRACT Sally's age from the value 42.	
42-age["Sally"]
 
#J. SUBTRACT Sallys' age from 42 but #don't print out Sally's name 
as.numeric(42-age["Sally"])

#K. DISPLAY the data type class of the vector is sex?
class(sex)

#L. CREATE a vector named < sexF > which is a factor.
sexF<-as.factor(sex)

#M. DISPLAY the levels for the factor sexF
levels(sexF)

#N. DISPLAY out your working directory Load the following libraries: 
setwd("C:/Users/Holt/Documents/NorthEastern/NorthEasternCLASSES/ALY 6000 Intro to Analytics/R")
#The above will cause an error on other machines
getwd()
print("Libraries At Top")

#O IMPORT inchBio.csv and name the table < bio >
bio<-read.csv("inchBio.csv")

#P DISPLAY the head, tail and structure of < bio >
head(bio)
tail(bio)
str(bio)
summary(bio)

#Q CREATE an object < counts > that counts and lists all the Species record 
counts<- count(bio, var=bio$species)
counts
table(bio$species)
#R Display just the 8 levels (names) of the species

levels(bio$species)
#or
counts[1]

#S CREATE a < tmp > object that displays the different species 
#and the number of records of each species in the dataset
tmp<- counts
names(tmp)<- c("species","freq")

#T CREATE SUBSET < tmp2 > of just the species variable and display the first #five records
tmp2<- tmp[1]
head(tmp2,5)

#U Print you last name
print("Williams")

#V CREATE a TABLE named < w > of the species variable. Display class of w
w<-table(bio$species)
class(w)

#W CONVERT < w > to a data frame, name the data frame < t > and display the #results
t<-data.frame(w)
t

#X EXTRACT and display the  frequency values from the < t > data frame
t[2]


#Y CREATE a table named < cSpec > from the bio species attribute (variable). 
#and confirm you created a table which displays the number of each species in
#the dataset < bio >

#cSpec<- count(bio$species)
cSpec<- table(bio$species)
class(cSpec)
#Z CREATE a table named  < cSpecPct > that displaces the species 
#and percentage of the records for each species.Confirm you created a table 
#class

cSpecPct<- prop.table(cSpec)
class(cSpecPct)

#AA CONVERT the table < cSpecPct > to a data frame named < u >
#and confirm that < u > is a data frame
u<- data.frame(cSpecPct)
class(u)

#BB PLOT a BARPLOT of < cSpec >with 
# y label: "COUNTS", 
# color: "Light #Green", 
# title "Fish Count",  
# y axis values: rotated to horizontal,   
# x axis font #magnification: at 60% of nominal,

barplot(cSpec, 
        col="light green",
        main="Fish Count", 
        ylab="Counts",
       cex.names = 0.6,
       las=2
       )

#CC PLOT a BARPLOT of  < cSpecPct > with 
# y axis limits: 0 to .4, 
# y axis label: %, 
#color: "Light Blue, 
#Title: "Fish Relative Frequency"
barplot(cSpecPct, 
        col="light blue",
        main="Fish Relative Frequency", 
        ylab="%",
        cex.names = 0.6,
        las=2,
        ylim=c(0,.4)
        )

#DD REARRANGE the  < u > cSpecPct data frame in descending order of #relative frequency. 
#SAVE the rearraged data frame as the object < d >

d<- u%>%arrange(desc(Freq))

#EE RENAME the < d > columns Var1 to Species, Freq to Relative Freq 

names(d)<- c("Species", "Relative Freq")
names(d)

#FF ADD NEW VARIABLES to the < d > data frame:  cumfreq, counts, #cumcounts
d$cumfreq<- cumsum(d$`Relative Freq`)
d$counts<-((d$`Relative Freq`))*sum(counts$n)
d$cumcounts<- cumsum(d$`counts`)

#GG CREATE a parameter variable < def_par > to stor par values

#at top def_par<- par()
par(def_par)
#HH-1 CREATE the BARPLOT <pc> with the following spec:
#d$counts, width: 1, space:.15, border: NA, axes:F, yaxis limitL # 0,3.05*max(d$counts, na.rm: is TRUE, 
#y label: cumulative Counts, scale x axis #to 70%, names.arg: d$Species, Title: Species Pareto, las: 2)              las=2)

par(mar=c(6,4,4,3))
pc<-barplot(d$counts,
             width=1, 
             border = NA, 
             axes = F, 
             ylim = c(0,3.05*max(d$counts,na.rm=TRUE)),
             ylab = "Cumulative Counts",
             main="Species Pareto",
             las=2,
             cex.names =.7,
             names.arg = d$Species,
             bg="grey60")

#HH-2 ADD a Cumulative counts line to the < pc > plot with 
#spec line type: b, scale plotting text at .7, data values: solid circles, color: #cyan4,

lines(x=pc,d$cumcounts,
              type="b",
              col="cyan4",
              pch=16,
              cex.axis=.7)

#HH-3 PLACE a grey62 box around the pareto plot

box(col="grey62")

#HH-4 https://www.statmethods.net/advgraphs/parameters.html
##JJ ADD left side AXIS details, spec: side 2, at cumcounts, horizontal values at #tick marks, color: grey62,  color of axis: grey62, axis font scaled to 80% of #nominal

axis(side=2, 
     at=c(0,d$cumcounts),
     las=2,
     col = "grey62",
     col.ticks = "grey62",
     cex.axis=(.8),
     col.axis="grey62")

#HH-5 https://www.statmethods.net/advgraphs/axes.html
#ADD AXIS details on right side of box. 
# spec: side:4, 
# tick marks at: #cumcounts, 
# #labels: 0 to cumfreq with %,  
# axis color: cyan5, 
# label color: #cyan4, 
# axis font scaled to 80% of nominal 

axis(side=4, 
     at=c(0,d$cumcounts),
     labels=paste(as.character(round(c(0,d$cumfreq)*100)),"%", sep=""),
     las=2,
     col = "cyan4",
     col.ticks = "cyan4",
     cex.axis=(.8),
     col.axis="cyan4")


par(def_par)
