BDPI <- as.data.table(BDPI)
install.packages("zoo")
library("zoo") #rollmax

BDPI$HIGHMX = rollmax(x=BDPI$HIGH, k =  4, fill = NA, align = "right")
BDPI$HIGHMXL1 = shift(x = BDPI$HIGHMX, n = 1, fill = NA, type = "lag")
BDPI$LOW_MN = rollapply(BDPI$LOW, 4, min, fill = NA, align = "right")
BDPI$LOW_MN = rollapplyr(data = BDPI$LOW, width = 4, FUN = min, fill = NA) #mejor solución
BDPI$LOWMNL1 = shift(x = BDPI$LOW_MN, n = 1, fill = NA, type = "lag") #mejor solución

PR2 = as.data.table((frollapply(x = c(BDPI$HIGH), n = c(4, 3), FUN = min)))
frollmean(BDPI[, V1], 3)