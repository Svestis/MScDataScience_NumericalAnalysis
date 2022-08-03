# Unit 1
## 1.1
### Read file
crime<-read_sav("data/Crime.sav")

## 1.2
### Create a summary statistic of variable 'antisocx'
summary(crime$antisocx)

# Unit 2
### Explore whether survey respondents experienced any crime in the 12 months prior to survey with the variable 'bcsvictim'
table(crime$bcsvictim)
