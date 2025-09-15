ddt <- read.csv("DDT.csv")
ddt
##Quiz

#Transform WEIGHT to z. Find all the WEIGHTS that are considered as outliers and enter the largest WEIGHT outlier below!!
W <- ddt$WEIGHT
z <- scale(W)[,1]
W[abs(z) > 3]

#Determine all the possible WEIGHT outliers and of these enter the smallest
W <- ddt$WEIGHT
z <- scale(W)[,1]
W[abs(z) >=2 & abs(z) <= 3]

#Subset the DDT data frame so that only CCATFISH measurements are present. Now find all CCATFISH with LENGTHS that are possible outliers of the CCATFISH (z is calculated from the subset not the original data), from these possible outliers submit the value of the smallest MILE (Hint: you will be dealing with data frames)
ddtc <- ddt[ddt$SPECIES == "CCATFISH",]
L <- ddtc$LENGTH
z <- scale(L)[,1]
ddtc[abs(z) >= 2 & abs(z) <= 3, ]


##In class
library(Intro2R)
library(dplyr)
head(ddt)

ddtLM <- ddt %>% filter(SPECIES == "LMBASS") %>% mutate(z = scale(LENGTH)[,1])
ddtLM

library(ggplot2)
# aes is aestetics
g <- ggplot(ddt, aes(x=LENGTH, y=WEIGHT))
g <- g + geom_point()
g

L <- ddt$LENGTH
len <- scale(L)[,1]
L[len>15.5]

ddt[ddt$LENGTH > 15.5,]
#or
ddttest <- ddt %>% filter(LENGTH > 15.5)
ddttest

df <- ddt[ddt$WEIGHT > 50 & ddt$WEIGHT < 2300,]
df
#or
ddtL <- ddt %>% filter(WEIGHT > 50) %>% filter(WEIGHT < 2300)
ddtL

ddtL2 <- ddt %>% filter(SPECIES == "LMBASS") %>% filter(WEIGHT > 700) %>% filter(DDT < 300)
ddtL2

library(ggplot2)
g=ggplot(ddt, aes(x=SPECIES, y=WEIGHT , fill = RIVER))
g+geom_boxplot()

#How many fish have a length bigger than 50 units?
ddtlen <- ddt %>% filter(LENGTH > 50)
ddtlen

#How many fish are LMBASS and come from the TRM river and have a DDT bigger than 2 ?
ddtfind <- ddt %>% filter(SPECIES == "LMBASS") %>% filter(RIVER == "TRM") %>% filter(DDT > 2)
ddtfind

#How many fish are categorized as outliers based solely on their LENGTH?
df <- ddt %>% mutate(z = scale(LENGTH)[,1])
#Gets TRUEs and FALSEs
abs(df$z) > 3
sum(abs(df$z) > 3)

#How many CCATFISH are outliers with respect to DDT?
df <- ddt %>% mutate(z = scale(DDT)[,1]) %>% filter(SPECIES == "CCATFISH")
sub(abs(df$z) > 3)

