library(haven)
library(dplyr)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmodena <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

data = read_sav("data/Health_finalUnit.sav")

# Descriptive Statistics
print("Number of observations")
print(nrow(data))

print("Ensuring unique individuals through serial number")
print(nrow(unique(data["pserial"])))

print("Percentage of people drinking alcohol")
prop.table(table(as_factor(data$dnnow)))*100

print("Percentage of women")
prop.table(table(as_factor(data$Sex)))*100

print("Education level")
prop.table(table(as_factor(data$topqual3)))*100

print("Marital status")
prop.table(table(as_factor(data$marstatc)))*100

# Mean
mean(data$HHSize)
mean(data$bmival, na.rm=TRUE)
mean(data$Age)

# Mean
median(data$HHSize)
median(data$bmival, na.rm=TRUE)
median(data$Age)

# Mode
getmode(data$HHSize)
getmodena(data$bmival)
getmode(data$Age)

# Min
min(data$HHSize)
min(data$bmival, na.rm=TRUE)
min(data$Age)

# Max
max(data$HHSize)
max(data$bmival, na.rm=TRUE)
max(data$Age)

# Range
range(data$HHSize)
range(data$bmival, na.rm=TRUE)
range(data$Age)

# SD
sd(data$HHSize)
sd(data$bmival, na.rm=TRUE)
sd(data$Age)

#t-test
t.test(data$Sex, data$htval) #Height
t.test(data$Sex, data$whval) # Weight

#correlation
print(cor(data[, c('dnnow','totinc','Age','Sex')], use="pairwise.complete.obs"))
