## Data Preprocessing

# Reading in Data
e <- read.csv('HemosiderinStatistics_EARLI_20x.csv')
n <- read.csv('HemosiderinStatistics_NYMH_20x.csv')

# Seems like there's a variable in e that's not in n: 'Total Stain'
colnames(e) #7
colnames(n)
# Removing that column:
e <- e[,-7]

## Data Analysis

# t.test by variable

# creating matrix to record t.test results
A_ttest <- matrix(c("",""),
                  nrow=1,
                  ncol=4)
colnames(A_ttest) <- c("Variable","t-stat","p-value","estimated mean of differences")

# filling matrix with t-test for each variable
for(i in 2:ncol(e)){
  I<-i
  X<-t.test(e[,I],n20[,I],paired = FALSE)
  A_ttest <- rbind(A_ttest,c(colnames(e)[I],c(X[c(1,3)],(X$estimate[1]-X$estimate[2])))
}

# removing first row
A_ttest <- A_ttest[-1,]

# looking at matrix
A_ttest <- data.frame(A_ttest)
A_ttest

# organizing matrix by p-value (statistical significance)
A_ttest <- A_ttest[order(as.numeric(A_ttest[,3]),decreasing=FALSE),]

# removing the cases where p>.05 and the null hypothesis cannot be rejected
# these remaining variables are different between the two datasets.
A_ttest <- A_ttest[A_ttest[,3]<0.05,]
A_ttest
A_ttest

