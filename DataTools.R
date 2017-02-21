#Summary of Data analysis tools in R:
#Setup--------------------------------------------------------------------------------------
getwd()
setwd('/Users/ThomasPfeiffer/Google Drive/Programming/R Programming')
#Import Data--------------------------------------------
data <- read.csv('') #data separated with , or ;
d <- read.delim('/Users/ThomasPfeiffer/Downloads/stfis14_1a.txt', sep = '\t' , header = TRUE) #import data separated with a tab
library(XLConnect) #import data from the web
tmp = tempfile(fileext = ".xls")
download.file(url = "https://nces.ed.gov/ccd/tables/xls/ACGR_RE_Characteristics_2014-15.xlsx",destfile = tmp, mode = "wb")
d <- readWorksheetFromFile(file = tmp, sheet = "014-15 ACGR",header=TRUE,startRow=3,endRow=62,startCol=1,endCol=12)
#Data Structure-----------------------------------------------------------------------------
names(edbuild)
str(edbuild)
head(edbuild)
summary(edbuild$col1)
#Data Manipulation--------------------------------------------------------------------------
data <- sapply(data, function(x)as.numeric(gsub(",", "", x))) #As number, without commas
data <- as.data.frame(data) #as.Date(format = "%m/%d/%y"), as.integer, as.factor; as.numeric(as.character(data))
data[data=='-'] <- '0' #replace data points according to criteria with a replacement 
data <- droplevels(data) #remove unused factor levels
slices <- lapply(formulaType, sum)
names(data)[3:6] <- c('','','') #or: setNames(data[3:6], c('','',''))
spread(data, colToColTitles, values) #gather(data, col1Name, col2Name, desiredColumnsToGather) *tidyr package*
cbind(data1,data2) #rbind()
newdata <- subset(mydata, sex=="m" & age > 25, select=col1:col5) #subset and then keep only columns of interest
#Data Visualization-------------------------------------------------------------------------
library(ggplot2)
plot(x,y, type = 'p') #type = points, lines
hist(dataDistribution, breaks=10, main='title')
#barplot
ttlStudentExp <- d$PPE15
names(ttlStudentExp) <- d$STNAME
ttlStudentExp <- sort(ttlStudentExp)
par(las=2, mar=c(11,4,1,1), cex=.5)#margin: bottom,left,top,right; cex:font size
barplot(ttlStudentExp, main = "Per Pupil Expenditure")
#boxplot
boxplot(x ~ y)
par(mfrow = c(1,2)) #multiple graphs
#GGplot library(ggplot2)
ggplot(totals, aes(Fecha, values, col=factor(cd_or_loan, labels = c("Loans", "CD")))) + geom_line() +
        labs(title ="Total Balances", x = "Date", y = "Balance (millions)", color = "CD or Loan") +
        theme( plot.title = element_text( size = rel( 2 ), hjust = 0.5 ), legend.key = element_rect( color = "black" ) )
ggplot( data=clusterData, aes( x = reorder(factor(state), funding), y = funding)) + 
        geom_bar(stat="identity", aes(fill = fundType)) + labs(title="Public School Funding Sources", x='State',y='Funding Percent of Total',fill="") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5),
              legend.position="top")
ggplot(d, aes(x=School.and.Grade)) + geom_bar(aes(fill=BOY.Proficiency)) +
        labs(title='Beginning of Year Analysis', x='School and Grade',y='Student Count', fill="Proficiency Level") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5))
ggplot(d, aes(x=School.and.Grade)) + geom_bar(aes(fill=factor(BOY.Proficiency, 
                                                              levels = c("Advanced", "Proficient","Below Proficient",
                                                                         "Remedial", "NA")))) +
        labs(title='Beginning of Year Analysis', x='School and Grade',y='Student Count', fill="Proficiency Level") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5))
#Data Creation------------------------------------------------------------------------------
IQR(vector) #compute interquartile range i.e. 3rd quartile - 1st quartile
rnorm(number, mean, sd) #generate random normal data
sd(data)#standard deviation
min(data) #max(data)
#Forecasting library(forecast)--------------------------------------------------------------
auto.arima()
ets()
forecast(model, 10)
acf() #pacf()
Box.test() #Ljung-Box test independence of errors; H0: independent errors


#output csv file----------------------------------------------------------------------------
write.csv(edbuild, 'edbuild') 
#is this there


