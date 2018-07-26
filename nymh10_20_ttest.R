require(ggplot2)

## Data Preprocessing

# Reading in Data
d10 <- read.csv('HemosiderinStatistics_NYMH_10x.csv')
d20 <- read.csv('HemosiderinStatistics_NYMH_20x.csv')

# Seems like there's an entry in d10 that's not in d20: #17
# Removing row 17:
d10 <- d10[-17,]

# Seems like there's a variable in d10 that's not in d20: 'Total Stain'
# Removing that column:
colnames(d10) #7
d10 <- d10[,-7]

## Data Analysis

# t.test by variable

# Creating matrix to record t.test results
A_ttest <- matrix(c("",""),
                    nrow=1,
                    ncol=4)
colnames(A_ttest) <- c("Variable","t-stat","p-value","estimated mean of differences")

# filling matrix with t-test for each variable
for(i in 2:ncol(d20)){
  I<-i
  X<-t.test(d10[,I],d20[,I],paired = TRUE)
  A_ttest <- rbind(A_ttest,c(colnames(d10)[I],X[c(1,3,5)]))
}

# removing first row
A_ttest <- A_ttest[-1,]

# looking at matrix
A_ttest

# organizing matrix by p-value (statistical significance)
A_ttest <- A_ttest[order(as.numeric(A_ttest[,3]),decreasing=FALSE),]

######################### NON-WORKING CODE
# plotting matrix
A_ttest_df <- A_ttest[,c(1,3)]
colnames(A_ttest_df)
A_ttest_df <- as.data.frame(A_ttest_df)
A_ttest_df
ggplot(A_ttest_df, aes(Variable,pvalue)) + geom_bar(stat='identity')
ggplot(mpg, aes(class,hwy)) + geom_bar(stat='identity') #goals. i tried making $Variable a character like $class is to no avail
###############################

# removing the cases where p>.05 and the null hypothesis cannot be rejected
# these remaining variables are different between the two datasets.
A_ttest <- A_ttest[A_ttest[,3]<0.05,]
View(A_ttest)
A_ttest[1]
