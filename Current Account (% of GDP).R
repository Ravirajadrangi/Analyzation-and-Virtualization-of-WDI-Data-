rm(list=ls(all=T))          #Removes the previous working data(clearing)
               #packages
library(WDI)
library(ggplot2)
                 #Load the csv data using below command
wdi<- read.csv(file.choose(), header = TRUE)
head(wdi)                 #head of the data
tail(wdi)                    #tail of the data
#summary of the data
summary(wdi)
# Creating a new dataset without missing data
#mydata1 <- na.omit(myda nta)
#summary(mydata1)
#sorting
#mydata1.sorted <- mydata1[order(mydata1$Country.Name),]
#summary(mydata1.sorted)
# Extracting Number of countries in Data
unique(mydata1$country)
# extracting only India data from WDI
#IND<-mydata[170522:171941,]
names(wdi)      #columns names
data.frame(wdi)   #making Data frame
# select countries and variables 
list.countries <- c("IND", "CHN", "DEU", "JPN", "USA","GBR","ARB","BRA","FRA","RUS")
#Select  current Account (percent of GDP ) indicator code
list.variables <- c("BN.CAB.XOKA.GD.ZS") #, "NE.GDI.FTOT.ZS", "NY.GDS.TOTL.ZS")
#Extract current Account (percent of GDP ) from wdi
wdi_sub <- wdi[wdi$Indicator.Code %in% list.variables,]
# Extract countries data from wdi
wdi_sub <- wdi[wdi$Country.Code %in% list.countries & wdi$Indicator.Code %in% list.variables,]
# convert to ts 
convert_to_ts <- function(dataset) {
  ts_data <- ts(
t(dataset[,5:ncol(dataset)-1]),  # transpose: ts expects data in columns
    # note that it treats the row names as a column here
start=1960, 
frequency=1
  )
  # label with country names 
colnames(ts_data) <- as.character(dataset$Country.Code)
return(ts_data)
}
ts<- convert_to_ts(wdi_sub)
ts<- window(ts, start=2000,end=2014)
df<- data.frame(ts)
summary(ts)
head(ts)
#plotting the current  account(percent of GDP) of countries
par(fin=c(7,5))
colseq = c("blue3","orange", "red", "black", "green3", "blue", "purple","yellow","green1","gray")
plot(ts,
     plot.type='single',
mgp=c(2.75,1,0),
mai=c(1,1,1,1),
lwd=4, 
     xlab="", ylab="Current Account (percent of GDP)", main="",
col=colseq)
)
abline(a=0, b=0)  

mtext("Source: World Bank, World Development Indicators, May 2016", side=1, line=2.5, cex=.6, adj=0)
legend("topleft", legend=colnames(ts), cex=0.90, lwd=4, col=colseq, 
       y.intersp=1, x.intersp=0.5, ncol=2, bty="n"
)
dev.print(device=pdf, file="BCH_CA_WDI.pdf", width=8, height=6)

