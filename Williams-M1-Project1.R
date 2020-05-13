
# G. Holt Williams R Project 1
#Print file name
Author <- "G. Holt Williams"
file<- "Williams-M1-Project1"
print(Author)
print(file)


#Install and load packages (Commented out so as not too install package twice)
#install.packages("vcd")
library(vcd)

# Create vectors for data analysis
sales <-  c(8,11,15,20,21,11,18,10,6,22)
temp <- c(69,80,77,84,80,77,87,70,65,90)

#create Scatter plot
plot(sales,temp)

#mean of temp
mean(temp)

#Removes third value (15), then inserts the value 16 into that position
sales<- sales[-3]
sales
sales<- append(sales,16,after=2)
sales

#Creates names vector
names<- c("Tom","Dick", "Harry")

#creates blank matrix
matrix(nrow=5, ncol=2)
#creates matrix with 10 numbers sampled from 1:100
matrix(sample.int(100,10),nrow=5, ncol=2)

#makes data frame with previous vectors: sales and temp
icSales<- data.frame(sales,temp)
icSales
#shows Structure of data.frame
str(icSales)
#Summarizes data.frame
summary(icSales)


#reads in and saves text file: May need to setwd() to where ever the file is stored
studentgrades<- read.delim(file="studentgrades.txt", sep=",")

#option 2 using professor provided files
#student<- read.csv(file="Student(1).csv", header=TRUE)

#structure of Student Grades
str(studentgrades)
summary(studentgrades)
#mean of math grades
mean(studentgrades[,4])

#save studentgrades as a CSV
#write.csv(studentgrades,"studentgrades.csv")

#Example of data.frame help 
A<- c(rnorm(5))
A
B<- c("a","b","c","d","e")
B
df<- data.frame(A,B)
class(df)
