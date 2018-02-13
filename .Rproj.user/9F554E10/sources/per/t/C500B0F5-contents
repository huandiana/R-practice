#764 Exercise 2#
install.packages('readxl')
install.packages('data.table')
library(readxl)
library(data.table)

#problem 2.1
lab2 <- read_excel('Desktop/R practice/lab2_data.xlsx')
head(lab2)
tail(lab2)

#problem 2.2
lab2_1 <- na.omit(lab2)   #na.omit will omit all the NA in rows
head(lab2_1) 
tail(lab2_1)

#problem 2.3
lab2_2 <- setnames(lab2_1, old=c('Shares Outstanding (mil) - Yearly Year2004', 
                       'Daily Closing Price 2004-12-31 USD', 'P/E - Daily 2004-12-31'), 
                 new=c('shsOut', 'clsgPrice', 'pE'))
lab2_2$lnSize <- log(lab2_2$shsOut * lab2_2$clsgPrice)
lab2_2$eP <- 1/lab2_2$pE

#plot to visualize 
hist(lab2_2$lnSize, main='distribution of log capitalization',
                    xlab='lnSize number')
hist(lab2_2$eP, main='distribution of eP', xlab='eP number')

#problem 2.4 -- calculate the basic descriptive statistics 
#(a) mean and median 
lnSize_mean <- mean(lab2_2$lnSize)
lnSize_median <- median(lab2_2$lnSize)

eP_mean <- mean(lab2_2$eP)
ep_median <- median(lab2_2$eP)

#(b)variance, standard deviation, interquartile range 
lnSize_var <- var(x=lab2_2$lnSize, y = NULL)
lnSize_stdv <- sqrt(lnSize_var)
lnSize_interquartile <- IQR(lab2_2$lnSize)

eP_var <- var(x=lab2_2$eP, y=NULL)
eP_stdv <- sqrt(eP_var)
eP_interquartile <- IQR(lab2_2$eP)

#(c)distribution or shape:
lnSize_dis <- quantile(lab2_2$lnSize, c(0.05, 0.25, 0.5, 0.75, 0.95))
eP_dis <- quantile(lab2_2$eP, c(0.05, 0.25, 0.5, 0.75, 0.95))

#problem 2.5
#set a new data frame 
lab2_3 <- na.omit(setDT(lab2_2))

#with outliers -- lnSize
lnSize_box <- boxplot(lab2_3$lnSize, main='lnSize with Outliers',
                      range=2,xlab='lnSize')
lnSize_outlier <- boxplot.stats(lab2_3$lnSize)$out
length(lnSize_outlier)  

#with outliers -- eP
eP_box <- boxplot(lab2_3$eP, main='eP with Outliers')
eP_outlier <- boxplot.stats(lab2_3$eP)$out
length(eP_outlier) 

#######dealing with outliers#######
is.out = function(x) abs(x - mean(x)) > 3*sd(x)

sapply(Filter(is.numeric, lab2_3),
       function(x) c(out = sum(is.out(x))))

lab2_3[!is.out(lnSize), lnSize1 := lnSize]
lab2_3[!is.out(eP), eP1 := eP]

lnSize1 = na.omit(lab2_3$lnSize1)
eP1 = na.omit(lab2_3$eP1)

boxplot(lnSize1,
        range = 2, ylim = c(12,25),
        main = 'Boxplot for lnSize (without outliers)',
        xlab = 'lnSize')

boxplot(eP1,
        range = 2, ylim = c(-40,5),
        main = 'Boxplot for E/P (without outliers)',
        xlab = 'eP')

hist(lnSize1)
hist(eP1) 

### There are 5 outliers in lnSize and 1 outlier in eP. #####

#problem 2.6
#With outliers version
lab2_2$lnSize_z <- (lab2_2$lnSize-lnSize_mean)/lnSize_stdv
plot(lab2_2$lnSize_z)

lab2_2$eP_z <- (lab2_2$eP-eP_mean)/eP_stdv
plot(lab2_2$eP_z)
#The meaning of standardlization is easy for comparizon between different data sets.
#scale to find the z score of certain variables 
