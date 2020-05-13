#G. Holt Williams

#PROJECT 2 R-script
#1. PRINT  "Plotting Basics: Your last name"  
print("Plotting Basics: Williams")

#2 IMPORT Libraries: FSA, FSAdata, magrittr,  dplyr, plotrix, ggplot2, moments
# install.packages("FSA")
# install.packages("FSAdata")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("plotrix")
# install.packages("ggplot2")
# install.packages("moments")
# 
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

#3.LOAD The dataset BullTroutRML2  using command data("BullTroutRML2")  Note the dataset is already imported into your project when you added  the libraries FSA and FSAdata. You only need to load the dataset.
data(BullTroutRML2)
df<- BullTroutRML2

#4.PRINT  first and last 3 records dataset BullTroutRML2
head(df,3)
tail(df,3)
#5 REMOVE all the records from BullTroutRML2 EXCEPT those from Harrison Lake 
df<- df %>% filter(lake == "Harrison")
#6 DISPLAY again the first and last 5 records from dataset BullTroutRML2
head(df,5)
tail(df,5)
#7DISPLAY the structure of the filtered BullTroutRML2 dataset
str(df)
#8 DISPLAY the summary of the filtered BullTroutRML2 dataset
summary(df)

#9 PLOT A SCATTERPLOT ( The spec:  age (y variable) and fl (x variable) from the "filtered-Harrison " BullTroutRML2. Set limits on x axis 0,500 and y axis 0,15.  #Title is "Plot1: Harrison Lake Trout Scatter"
#Label the y axis "Age (yrs)" and x axis "Fork Length (mm)", 
#Use a filled small circle for the plotted data points)

plot(x=df$fl, y =df$age, pch=16,
     xlab = "Fork Length (mm)", 
     ylab="Age (yrs)",
     main= "Plot1: Harrison Lake Trout Scatter")


#10 PLOT2 HISTOGRAM: a BullTroutRML2 age histogram with y axis label: #Frequency, x axis label:Age (yrs),  Title: Plot2: Harrison Fish Age Histogram
#Both x and y axis limits 0 15

hist(df$age, col = " light blue",
     xlab = "Age (yrs)", 
     ylab="Frequency",
     main= "Plot2: Harrison Fish Age Histogram",
     xlim=range(0:15),
     ylim=range(0:15))

#11 PLOT3 OVERDENSE SCATTERPLOT: Solution: use 2 levels of shading with 2	 
#levels of green data points, y axis limits:0 to 15, x axis limits 0 to 500, y axis label: 
#Age (yr), X axis label: Fork Length (mm), Title: Plot3: Harrison Density Shaded By 
#Era, plot solid circles as data points

#I'm a little confused if this is the answer you are lookign for. It's not "shaded," just two diffrent colors.
plot(x=df$fl, y =df$age, pch=19, col = c("green", "light green")[df$era],
     xlab = "Fork Length (mm)", 
     ylab="Age (yrs)",
     main= "Plot3: Harrison Density Shaded By Era",
     xlim=range(0:500),
     ylim=range(0:15))

#12 CREATE tmp object with the first 3 and last 3 records of BullTroutRML2 and 
nrow(df)
tmp<-df[c(1:3,59:61),]
# or df[c(1:3,(nrow(df)-2):nrow(df)),]

#13 DISPLAY the "era" column (variable) of the tmp object
tmp$era

#14 CREATE a pchs vector with numerical  arguments for + and x 
pchs<- c(3,4)

#14 CREATE a cols vector with the two elements: "red" and "gray60" 
cols<- c("red", "gray60")

#15 CONVERT the tmp era values to numeric
tmp$era<- as.numeric(tmp$era)

#16 INITIALIZE the cols vector with tmp era values 

# I'm Confused on what that means or what is happening here
cols<- cols[as.numeric(df$era)]
pchs<- pchs[as.numeric(df$era)]
#17 NOW PLOT4: (The spec: age(y variable) versus fl (x variable), Title Plot:"Plot4: Symbol & #Color By Era, Set x variable limits 0, 500 and y variable limits 0, 15, Set y axis label equal to "Age" and x axis label equal to Fork Length (mm). set pch equal to pchs era values and col equal to cols era values

#The colors and tick marks are different then the example
plot(x=df$fl, y =df$age,pch=pchs, col = cols,
     xlab = "Fork Length (mm)", 
     ylab="Age (yrs)",
     main= "Plot4: Symbol & Color By Era",
     xlim=range(0:500),
     ylim=range(0:15))

#18 PLOT a regression line overlay on PLOT4:  Title PLOT5: "Plot5: Regression Overlay

plot(x=df$fl, y =df$age, pch=pchs, col = cols,
     xlab = "Fork Length (mm)", 
     ylab="Age (yrs)",
     main= "Plot5: Regression Overlay",
     xlim=range(0:500),
     ylim=range(0:15),
     abline(lm(df$age~df$fl))) 


#19 PLOT6: Place a Legend overlay on PLOT5
plot(x=df$fl, y =df$age, pch=pchs, col = cols,
     xlab = "Fork Length (mm)", 
     ylab="Age (yrs)",
     main= "Plot6: Legend Overlay",
     xlim=range(0:500),
     ylim=range(0:15),
     abline(lm(df$age~df$fl), col="light blue", lty=2, lwd=2)) 
legend("topleft", title="ERA", levels(df$era), pch=c(3,4),col=c("red", "gray60"), border="white", bty="n")



