x <- c(1,2,3)
y <- c(1,2,3)

FF <- matrix(NA, 3, 3)

#FF[1,] <- x[1] * y
#FF[2,] <- x[2] * y
#FF[3,] <- x[3] * y


TT <- paste0("FF[", x, ",]"," <- ", "x[", x, "] * y")
# T1 <- paste0("FF[", x, ",]")
# T2 <- paste0("x[", x, "] * y")

eval(parse(text = TT))


FF <- function(BD,FI,FR) {
  BDP_IX_RX <- BD[match(c('DATE','DATEFRAME','VOLUME','OPEN','HIGH','LOW','CLOSE',FI ,FR), colnames(BD))]
  return(BDP_IX_RX)
}

VFF <- Vectorize(FF, vectorize.args = c("FI", "FR"))

BDPS_List <- VFF(BDPI, Senales$FI, Senales$FIR)






