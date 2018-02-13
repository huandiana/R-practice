#install.packages("readxl")
#install.packages('data.table')

# question 2.1
library(readxl)
library(data.table)

x = read_excel('R Exercises/lab2_data.xlsx', 
               sheet = 1, col_names = TRUE, 
               col_types = NULL, 
               na = "", 
               skip = 0)

# question 2.2
# cleaning data
print(any(is.na(x)))    
print(any(is.null(x))) 
x1 = na.omit(setDT(x))

# housekeeping
colname = c('Name','Ticker', 'Exchange', 'shsOut', 'clsgPrice', 'pE')
colnames(x1) <- colname

# question 2.3
x1$lnSize = log(x1$shsOut*x1$clsgPrice)
x1$eP = 1/x1$pE
y <- x1
hist(y$lnSize)
hist(y$eP)


# question 2.4 
# descriptive statistics
# a.measures of location and central tendency
lnSize_mean <- mean(y$lnSize)
lnSize_median <- median(y$lnSize)
eP_mean <- mean(y$eP)
eP_median <- median(y$eP)

# b.measures of scale or dispersion
lnSize_var <- var(y$lnSize)
lnSize_sd <- sd(y$lnSize)
lnSize_IQR <- IQR(y$lnSize)
eP_var <- var(y$eP)
eP_sd <- sd(y$eP)
eP_IQR <- IQR(y$eP)

# c.measure of distribution or shape
lnSize_quan <- quantile(y$lnSize, c(.05, .25, .50, .75, .95)) 
eP_quan <- quantile(y$eP, c(.05, .25, .50, .75, .95)) 

# question 2.5
# histgram & box plot
boxplot(y$lnSize,
        range = 2, ylim = c(12,25),
        main = 'Boxplot for lnSize',
        xlab = 'lnSize')
boxplot(y$eP,
        range = 2, ylim = c(-40,5),
        main = 'Boxplot for E/P',
        xlab = 'eP')

par(mfrow = c(1,1))

# outliers
is.out = function(a) abs(a-mean(a)) > 3 * sd(a)

sapply(Filter(is.numeric, y),
       function(a) c(out = sum(is.out(a))))

      #There are 5 outliers in lnSize and 1 in eP.

y[!is.out(lnSize), lnSize1 := lnSize]
y[!is.out(eP), eP1 := eP]

lnSize1 = na.omit(y$lnSize1)
eP1 = na.omit(y$eP1)

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

# The distribution of eP1 is looks more like a normal distribution after we omit the outlier.
# The distribution of lnSize also looks better.

# question 2.6
lnSize_z = scale(y$lnSize)
eP_z = scale(y$eP)
plot(lnSize_z)
plot(eP_z)

# By standardizing factor exposures, their affects are dropped and comparisons become easier. 
