library(haven)
library(expss)

# Unit 1 - Data Activity 1
## 1.1
### Read file
crime<-read_sav("data/Crime.sav")

## 1.2
### Create a summary statistic of variable 'antisocx'
summary(crime$antisocx)

#####################

# Unit 2 - Data Activity 2
### Explore whether survey respondents experienced any crime in the 12 months prior to survey with the variable 'bcsvictim' & create frequency table
View(crime)
table(crime$bcsvictim)

### Assess the results and decide if results should be changed to factor
as_factor(crime$bcsvictim)

#####################

# Unit 3 - Data Activity 3
### Create a subset of individuals belonging to 75+ age group and who were a victim of crime
crime_75victim<-subset(crime, crime$bcsvictim==1 & crime$agegrp7==7)

#####################

# Unit 5 - Data Activity 4
### Boxplot for variable antisocx with title and different boxplot and outlier color
boxplot(crime$antisocx, main="Levels of anti-social behaviour in neighbourhood ‘antisocx’", col="purple", outcol="blue")

### Bar plot with bscvictim with title and color
barplot(table(crime$bcsvictim), main="Experienced crime in the 12 months prior to the survey", col="orange")

#####################

# Unit 7 - Data Activity 5
### Mean median mode of sbp, dbp and income
health<-read_sav("data/Health.sav")

mean(health$sbp)
mean(health$dbp)
mean(health$income)

median(health$sbp)
median(health$dbp)
median(health$income)

mode <-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} # by https://www.tutorialspoint.com/r/r_mean_median_mode.htm

mode(health$sbp)
mode(health$dbp)
mode(health$income)

### Five fig summary of income + boxplot
summary(health$income)
boxplot(health$income)
