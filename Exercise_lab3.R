#Dan Song 114904546
#Mengya Cao 114970572
#He Diao 114833662

#install.packages('PerformanceAnalytics')
#install.packages('psych')
#install.packages('fmsb')
library(readxl)
library(data.table)
library(PerformanceAnalytics)
library(psych)
library(fmsb)
raw = read_excel('/Users/simon/Documents/764/lab3_data.xlsx',
                 sheet = 1, col_names = TRUE, 
                 col_types = NULL, 
                 na = "", 
                 skip = 0)

# Problem 3.1
# Data Cleaning
raw = na.omit(setDT(raw))

# Housekeeping
setnames(raw, c('Name','Ticker', 'Exchange', 'BE', 'close', 
            'MCap','CP','EP','EBITDAP','FCFP','SP','Return'))
x = raw[, .SD, .SDcols = - c('Name','Exchange')]


# Problem 3.2
is.out = function(x) abs(x - mean(x)) > 3 * sd(x)

sumx = sapply(x[, .(CP, EP, EBITDAP, FCFP, SP, Return)],
              function(x) c(mean = mean(x), sd = sd(x), 
                            nOut = sum(is.out(x))))
sumx

outliersA = sapply(x[, .(CP, EP, EBITDAP, FCFP, SP, Return)], is.out)
outliersB = apply(outliersA, 1, any)

(Nout = sum(outliersB))
x = x[!outliersB]


# Problem 3.3
y = x[BE>0 & EP>0 & close>5 & MCap>100000000]


# Problem 3.4
# a. Correlation Coefficients Matrix
chart.Correlation(as.matrix(y[, .(CP, EP, EBITDAP, FCFP, SP)],
                            histogram = T, pch = 19, method = "pearson",
                            main = "Correlation chart of price ratios and returns"))

# b. test of CC
correlation = corr.test(y[, .(CP, EP, EBITDAP, FCFP, SP)])
correlation[["r"]]
#To figure out if there is strong correlation between factors.

# c. p-value of CC
correlation[["p"]]
which(correlation[["p"]] < 0.05, arr.ind = T)
# The results of t-test show there are significant correlations between 
# each pairs of variables since p-values are lower than 0.05. 


# Problem 3.5
# a.
chart.Correlation(as.matrix(y[, .(CP, EP, EBITDAP, FCFP, SP, Return)],
                            histogram = T, pch = 19, method = "pearson",
                            main = "Correlation chart of price ratios and returns"))

# b.
correlation1 = corr.test(y[, .(CP, EP, EBITDAP, FCFP, SP, Return)])
correlation1[["r"]]

# c.
correlation1[["p"]]
which(correlation1[["p"]] < 0.05, arr.ind = T)
# The t-test shows that there are significant correlation in CP-Return 
# and EBITDAP-Return.


# Problem 3.6
zscore = scale(y[, .(CP, EP, EBITDAP, FCFP, SP)])
correlationZ = corr.test(zscore)
correlationZ[["r"]]
# The results of z-score won't change answer in 3.4 since the distribution
# won't change and the relationship will not be affected.

ZAggr = apply(zscore, 1, mean)
cor(y[,Return], ZAggr)
# The results show weak correlation between the aggregation of z-score and explained
# variable. This correlation does not present useful interpretation, it is not an indicative
# parameter.

# Problem 3.7
(CI = round(correlation[["ci"]],4))
# FCFP might introduce multicollinearity to our regression 
# since its significantly corellated to EBITDA/P.
# To correct the multicollinearity, we either omit the variable from our model,
# or we can expand the sample size to eliminate the effect of multicollinearity.


# Problem 3.8
# Based on the results of tests above, we should pick CP and EBITDA/P as our explanatory
# variable instead of incorporate all 5 variables. This not only reduces multicollinearity in the model, 
# prevent overfitting, but also makes the model more reliable.

correlation1[["ci"]][c("CP-EBITD",
                       "CP-Retrn",
                       "EBITD-Retrn"), ]
correlation2 = corr.test(y[, .(CP, EBITDAP, Return)])
correlation2[["ci"]]

model = lm(Return ~ EBITDAP, data = y)
summary(model)

# To prevent overfitting, we can
# (1) simply collect more data
# (2) use ensembling methods that “average” models
# (3) choose simpler models

#This is our process of perfect model and variables selection, using stepwise regression method

#min.model = lm(Return ~ 1, y)

#biggest <- formula(lm(Return~.(EBITDAP，EP，CP，SP，BE), y))
#biggest

#fwd.model = step(min.model, direction='forward', scope=biggest)
#fwd.model

#VIF to detect multicolinearity
#VIF(fwd.model)
