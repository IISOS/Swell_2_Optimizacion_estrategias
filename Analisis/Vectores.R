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

# --------------------------------------------------------------------

FF <- function(BD,FI,FR) {
  BDP_IX_RX <- BD[match(c('DATE','DATEFRAME','VOLUME','OPEN','HIGH','LOW','CLOSE',FI ,FR), colnames(BD))]
  return(BDP_IX_RX)
}

l1 <- FF(BDPI, "FI1", "FIR1")

VFF <- Vectorize(FF, vectorize.args = c("FI", "FR"))

BDPS_Listt <- VFF(BDPI, Senales$FI[1], Senales$FIR[1])


DF <- function(DF) {
  return(DF)
}

DF1 <- data.frame(a=c(1,2,3), b=c(10,11,12))
DF2 <- data.frame(a=c(11,12,13), b=c(20,21,22))

DFInsumo <- list(DF1, DF2)

Resultado <- DF(DFInsumo)

# --------------------------------------------------------------------

FunBDPSList <- function(BD, FI, FIR) {
  BDP_IX_RX <- BDPI[match(c('DATE',
                            'DATEFRAME',
                            'VOLUME',
                            'OPEN',
                            'HIGH',
                            'LOW',
                            'CLOSE',
                            FI ,
                            FIR
  ), 
  colnames(BDPI)
  )
  ]
  return(BDP_IX_RX)
}

BDPSs <- lapply(rep("BDPI", NFrac^2), get)

BDPSList <- FunBDPSList(BDPSs, Senales$FI, Senales$FIR)

VFunBDPSList <- Vectorize(FunBDPSList, vectorize.args = c("FI", "FIR"))
VBDPSList <- VFunBDPSList ("BDPI", Senales$FI, Senales$FIR)
VBDPSList <- VFunBDPSList (BDPI, Senales$FI, Senales$FIR)
VBDPSList <- VFunBDPSList (rep("BDPI", NFrac^2), Senales$FI, Senales$FIR)
VBDPSList <- VFunBDPSList (BDPSs, Senales$FI, Senales$FIR)

VFunBDPSList <- Vectorize(FunBDPSList, vectorize.args = c("BD", "FI", "FIR"))
VBDPSList <- VFunBDPSList ("BDPI", Senales$FI, Senales$FIR)
VBDPSList <- VFunBDPSList (BDPI, Senales$FI, Senales$FIR)
VBDPSList <- VFunBDPSList (rep("BDPI", NFrac^2), Senales$FI, Senales$FIR)
VBDPSList <- VFunBDPSList (BDPSs, Senales$FI, Senales$FIR)

MBDPSList <- mapply(FunBDPSList, rep("BDPI", NFrac^2), Senales$FI, Senales$FIR)
MBDPSList <- mapply(FunBDPSList, "BDPI", Senales$FI, Senales$FIR)
MBDPSList <- mapply(FunBDPSList, BDPI, Senales$FI, Senales$FIR)
MBDPSList <- mapply(FunBDPSList, BDPSs, Senales$FI, Senales$FIR)

# --------------------------------------------------------------------

library(dplyr)
X <- data.frame(dia=c(1,1,1,2,2,2,2,3,3,3), hora=c(1,2,3,1,2,3,4,1,2,3))
View(X)

Xnew <- X %>% group_by(dia) %>% slice_max(hora, n = 1)
View(Xnew)
