#install.package(c('data.table'))
library(readxl)
library(data.table)
#2.1
raw = read_excel('R Exercises/lab2_data.xlsx',
                 sheet = 'New Search Criteria',
                 range = 'A1:F629')
#2.2
tempData = na.omit(setDT(raw))
head(tempData)
#2.3
setnames(tempData, c('Name', 'Ticker', 'Exchange', 'shsOut', 'clsgPrice', 'pE'))

Data = tempData[, .(Ticker,
                    lnSize = log(shsOut * clsgPrice),
                    eP = 1 / pE) ]
par(mfrow = c(1,2))
#2.4
hist(Data[, lnSize],
     xlab = 'lnSize',
     main = 'Histogram of log Size')

hist(Data[, eP],
     xlab = 'eP',
     main = 'Histogram of E/P ratio')

par(mfrow = c(1, 1))

with(Data, list(mean = c(mean(lnSize), mean(eP)),
                median = c(median(lnSize), median(eP)),
                variance = c(var(lnSize), var(eP)),
                Stdev = c(sd(lnSize), sd(eP)),
                quantile = quantile(lnSize,
                                    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    type = 2)))
     sapply(Data[,.(lnSize,eP)],mean)     
#2.5(a)
     
     par(mfrow = c(1,2))
     
     boxplot(Data[, lnSize],
             range = 2, ylim = c(12, 25),
             main = 'Boxplot for log Size',
             xlab = 'lnSize')
     boxplot(Data[, eP],
             range = 2, ylim = c(-40,5),
             main = 'Boxplot for E/P ratio',
             xlab = 'eP')
     
    par(mfrow = c(1, 1))
    
#2.5(b)
is.out = function(x) abs(x - mean(x)) > 3 * sd(x)

sapply(Filter(is.numeric, Data),
       
       function(x) c(out = sum(is.out(x))))
       
Data[!is.out(lnSize), lnSize1 := lnSize]

Data[!is.out(eP), eP1 := eP]
par(mfrow = c(1, 2))

#2.6
hist(Data[, lnSize1],
     breaks = 20, xlim = c(10, 26),
     xlab = 'lnSize',
     main = 'Histogram of log size')


hist(Data[, eP1],
     breaks = 20, xlim = c(-40, 40),
     xlab = 'eP',
     main = 'Histogram of E/P ratio')

boxplot(Data[, lnSize1],
        range = 2, ylim = c(12, 25),
        main = 'Boxplot for log size',
        xlab = 'lnSize')
boxplot(Data[, eP1],
        range = 2, ylim = c(-40, 5),
        main = 'boxplot for E/P ratio',
        xlab = 'ep')

Data[, zlnSize1 := scale(lnSize1)]

Data[, zeP1 := scale(eP1)]
#The meaning of standardlization is easy for comparizon between different data sets.
#par pictures and all the belows.
#scale to find the z score of certain variables 
