# From 2001 to 2013 Two Parties of Govt ruled during this periods
# source of the Data from World Bank Website
# Required installation Packages for this Analysis:
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('tidyr')
install.packages('GGlly') 
install.packages("eeptools")
install.packages ("showtext")
install.packages('RJSONIO')
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(GGally)   
library(showtext) 

#Step-1
India_Edu<-read.csv(file.choose(), header =TRUE, sep=",")
India_Edu
is.data.frame(India_Edu)
head(India_Edu,5)
tail(India_Edu,5)
#ls()

# TO know the total no of Rows and Columns data frame
dim(India_Edu)
str(India_Edu)
# Variables is in rows 1421,  Years is in columns 
#A1<-melt(India_FDI, id.varse=c("CountryName", "IndicatorName"),
#  variable.names("year",value.name="Year"))
#tail(A1, 10)

# Selecting all Financial variables (in Row) and no of years of values (Columns)
India_Edua<-na.omit(India_Edu)               
India_Edua

#Step-2
# Extracting India's Gross enrolment ratio, pre-primary, both sexes (%) variables 
# year 1960 to 2014
India_Edu2 <- India_Edu[513:540,c(0:60)]
India_Edu2
Output for Step-2
India_Edu2 <- India_Edu[513:540,c(0:60)]
India_Edu2  #It displays the content in India_Edu2 file
 
#Step-3
#  Government expenditure on education as % of GDP (%)
# #Extracting Data from year 2009 to 2011
India_Edu3 <- India_Edu[513:513,c(55:57)]
India_Edu3
is.data.frame(India_Edu3)

India_Edu3M<-data.matrix(India_Edu3)
India_Edu3M
is.numeric(513)
is.matrix(India_Edu3M)
mean(India_Edu3M)
sd(India_Edu3M)

India_Edu3M<-data.matrix(India_Edu3)
India_Edu3M

is.numeric(513)
is.matrix(India_Edu3M)
mean(India_Edu3M)
sd(India_Edu3M)

# Step-3B   Summarizing  
India_Edu3M_Summary<-summary(India_Edu3M[1:3])
India_Edu3M_Summary
#Step-3P  Virtualization - Plotting Graghics
# Step-3C-H , Histragram:
hist(India_Edu3M, main="Government Expenditure on  Education" )
dev.off()

# Step-3P-B  Barplot:
barplot(India_Edu3M,
main = "India - Enrolment in  Secondary Education",
xlab="years",
ylab= "Enrolment in % ", col = "yellow",
pch=9)
dev.off()


#Step-4
# Gross enrolment ratio, primary, both sexes (%)
# #Extracting Data from year 2009 to 2011

India_Edu4 <- India_Edu[531:531,c(55:57)]
India_Edu4
India_Edu4M<-data.matrix(India_Edu4)
India_Edu4M
mean(India_Edu4M)
sd(India_Edu4M)
#	Output of Step-4
India_Edu4 <- India_Edu[531:531,c(55:57)]
India_Edu4
India_Edu4M<-data.matrix(India_Edu4)
India_Edu4M
mean(India_Edu4M)
sd(India_Edu4M)

# Step-4B   summarizing
India_Edu4M_Summary<-summary(India_Edu4M[1:3])
India_Edu4M_Summary
#Step-4P  Virtualization - Plotting Graphics
# Step-4C-H , Histragram:
hist(India_Edu4M, main="Enrolment in  primary Eduction" )
dev.off()

# Step-4P-B  Barplot:
barplot(India_Edu4M,
main = "India - Enrolment in primary Education",
xlab="years",
ylab= "Enrolment in % ", col = "yellow",
pch=9)
dev.off()


#Step-5
##  Gross enrolment ratio, secondary, both sexes 
# Step-5A   Extracting Data from year 2009 to 2011
India_Edu5 <- India_Edu[539:539,c(55:57)]
India_Edu5
India_Edu5M<-data.matrix(India_Edu5)
India_Edu5M 
mean(India_Edu5M)
sd(India_Edu5M)
##  Gross enrolment ratio, secondary, both sexes
# Step-5A   Extracting Data from year 2009 to 2011
India_Edu5 <- India_Edu[539:539,c(55:57)]
India_Edu5
India_Edu5M<-data.matrix(India_Edu5)
India_Edu5M
mean(India_Edu5M)
sd(India_Edu5M)

# Step-5B   Summarizing 
India_Edu5M_Summary<-summary(India_Edu5M[1:3])
India_Edu5M_Summary

#Step-5P  Virtualization - Plotting Graphics
# Step-5C-H , Histragram:
hist(India_Edu5M, main="Enrolment in  Secondary Education" )
dev.off()

# Step-5P-B  Barplot:
barplot(India_Edu5M,
main = "India - Enrolment in  Secondary Education",
xlab="years",
ylab= "Enrolment in % ", col = "yellow",
pch=9)
dev.off()

#	Step-6
# 	Simple Regression and correlation Analysis
#	Merging data frame –Secondary enrolment & Govt Spending on Education
bind_Matrix<-rbind(India_Edu5M,India_Edu4M, India_Edu3M)
bind_Matrix
is.matrix(bind_Matrix)
bind_Dataframe<- rbind.data.frame(India_Edu5M,India_Edu4M, India_Edu3M)
bind_Dataframe
is.data.frame(bind_Dataframe)
a<-t(bind_Matrix)
a

# 	Covariance’s and correlations
cov(a)
cor(a,  method="spearman") 

# Step-7
# Enrolment in Secondary School
x<-India_Edu5M[1:3]
x

# Govt Expend
y<-India_Edu3M[1:3]
y

cor(x, y)
cor(x,y, method="spearman")
cor(x,y,use = "everything", method ="kendall")
relation<- lm(x~y)
relation
# Displays detailed results for the fitted model
summary(relation)

#   Lists the predicted values in a fitted model
fitted(relation)
#   Lists the residual values in a fitted mo#Step -8
#   plot the Chart
plot(x,y, col="blue", main="Govt Investment in % of GDP Vs Growth in School Enrolment ",
abline(relation),cex=1.3,pch=16,xlab = "Enrolment in School %", ylab = " Investment in % GDP " )   
dev.off()
del
residuals(relation)

#Step -23
#   plot the Chart
plot(x,y, col="blue", main="Govt Investment in % of GDP Vs Growth in School Enrolment ",
abline(relation),cex=1.3,pch=16,xlab = "Enrolment in School %", ylab = " Investment in % GDP " )   
dev.off()







