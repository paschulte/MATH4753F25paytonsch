
ddt <- read.csv("DDT.csv")
L <- ddt$LENGTH
z <- (L - mean(L))/(sd(L))
z
# Outliers
ddt[z > 3,]
# MUST USE abs()
ddt[abs(z) > 3,]
# Possible outliers
ddt[abs(z) > 2 & abs(z) <=3,]

