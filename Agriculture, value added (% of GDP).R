list.countries <- c("IND", "CHN", "DEU", "JPN", "USA","GBR","ARB","BRA","FRA","RUS")
list.variables <- c("NV.AGR.TOTL.ZS") 
wdi_sub <- wdi[wdi$Indicator.Code %in% list.variables,]
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
par(fin=c(7,5))
#colseq = c("blue3","orange", "red", "black", "green3", "blue", "purple","yellow","green1","gray")
plot(ts,
    # plot.type='single',
mgp=c(2.75,1,0),
mai=c(1,1,1,1),
lwd=4, 
     xlab="", ylab="Agriculture, value added (% of GDP) ",
     #col=colseq 
)
abline(a=0, b=0)  
mtext("Source: World Bank, World Development Indicators, June 2016", side=1, line=2.5, cex=.6, adj=0)
legend("topleft", legend=colnames(ts), cex=0.90, lwd=4, col=colseq, 
       y.intersp=1, x.intersp=0.5, ncol=2, bty="n"
)
dev.print(device=pdf, file="BCH_CA_WDI.pdf", width=8, height=6)

barplot(ts, main="Agriculture, value add(% of GDP) from 2000 to 2015 ", horiz=TRUE, las=1, cex.names=0.5, border=NA)
