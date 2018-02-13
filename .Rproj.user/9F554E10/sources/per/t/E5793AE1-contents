#lab exercise 3
library(readxl)
library(data.table)
library(PerformanceAnalytics)
library(psych)
library(readxl)
library(zoo)
library(ggplot2)

#read lab 3 excel file
data <- read_excel('Desktop/R practice/lab3_data.xlsx')

#3.1
#clean data 
temp <- na.omit(as.data.table(data))

#3.2 
#remove outliers in variables
is.out = function(x) abs(x - mean(x)) > 3 * sd(x)

setnames(temp, c('name', 'ticker', 'exchange', 'be', 
                 'clsgPrice', 'size', 'cP', 'eP', 'ebitdaP',
                 'fcfP', 'sP', 'yr1Ret'))

head(temp)

sumData = sapply(temp[, .(cP, eP, ebitdaP,fcfP, sP, yr1Ret)], 
                 function(x) c(mean = mean(x), sd = sd(x), nOut = sum(is.out(x))))
sumData

outliersIndexA = sapply(temp[, .(cP, eP, ebitdaP,fcfP, sP)], is.out)
outliersIndexB = apply(outliersIndexA, 1, any)

(Nout = sum(outliersIndexB))
data_noout = temp[!outliersIndexB, ]

rm(sumData, outliersIndexA, outliersIndexB, Nout)

#3.3
data_noout = data_noout[be > 0 & eP > 0 & clsgPrice > 5 & size > 100000000, ]
#don't match 1198... don't know why

#3.4 
#3.4(a)
chart.Correlation(as.matrix(data_noout[, .(cP, eP, ebitdaP, fcfP, sP)]),
                  histogram = TRUE, pch = 11, method = "pearson",
                  main = "Correlation Chart of Price Ratios")

#3.4(b)
correlation = corr.test(data_noout[, .(cP, eP, ebitdaP, fcfP, sP)])
correlation[["r"]]

#3.4(c)
correlation[["p"]]

#3.5
#3.5(a)
chart.Correlation(as.matrix(data_noout[, .(cP, eP, ebitdaP, fcfP, sP, yr1Ret)]),
                  histogram = TRUE, pch = 11, method = "pearson",
                  main = "Correlation Chart of Price Ratios and Returns")

#3.5(b)
correlation1 = corr.test(data_noout[, .(cP, eP, ebitdaP, fcfP, sP, yr1Ret)])
correlation1[["r"]]

#3.5(c)
correlation1[['p']]

#3.6
zscore = sapply(data_noout[, .(cP, eP, ebitdaP, fcfP, sP)], scale)

correlationZ = corr.test(zscore)
correlationZ[["r"]]

zAggr = apply (zscore, 1, mean)
cor(data_noout[, yr1Ret], zAggr)

#3.7 
(CI = round(correlation[["ci"]], 4))

correlation1[["ci"]][c("cP-ebtdP",
                       "cP-yr1RT",
                       "ebtdP-yr1Rt")]
correlation2 = corr.test(data_noout[, .(cP, eP, ebitdaP, fcfP, sP)])
correlation2[["ci"]]

model = lm(yr1Ret ~ ebitda)

#3.8
