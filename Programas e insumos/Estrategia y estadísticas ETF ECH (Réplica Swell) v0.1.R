###  Estrategia y estadísticas ETF ECH (Réplica Swell)  ###
###                     2021-03-01                      ###
###                     Version 0.0                     ###  
###          Authors: Olga Serna / Ivan Serrano         ###


# 0. DESCRIPCIÓN E INSTRUCCIONES ##############################################

# 0.1 Objetivo

# Calcular la señal de la estrategia y sus estadísticas de retorno y riesgo a 
# partir de serie histórica OHLC (apertura, máximo, mínimo y cierre) y un árbol
# de decisión de estrategia.

# 0.2 Supuestos

# 0.2.1 Se realizan operaciones a las que haya lugar después de formación de la
#       vela de mercado para cada franja horaria, teniendo en cuenta el periodo
#       de tiempo permitido de operación definido para cada día, el cual excluye
#       las franjas horarias que se consideran de baja liquidez.

# 0.2.2 Todas las franjas horarias se tienen en cuenta para el cálculo de la
#       señal.

# 0.2.3 Se consigue operar en el mercado a los precios de referencia de los 
#       umbrales o precios de entrada definidos en el árbol de decisión, con
#       costos transaccionales asociados a un bid-ask spread nulo.

# 0.2.3 No se considera la dinámica de los flujos de capital, pues  no se 
#       tienen en cuenta los faltantes o excedentes de efectivo que se pueden
#       presentar al cerrar y abrir posiciones.

# 0.2.4 Para los cálculos de utilidad (pérdida o ganancia en términos 
#       monetarios), se tiene en cuenta si la posición es larga o corta.

# 0.2.4 La hora del archivo de insumo OHLC corresponde a la de Santiago y por
#       lo tanto el código considera las fluctuaciones horarias con relación
#       a la hora del este de Estados Unidos.

# 0.3 Instrucciones

# 0.3.1 Guardar con el nombre "Data NOMBREACTIVO.xlsx" el archivo que contiene
#       el histórico de precios OHLC en la misma ruta donde se encuentra ubicado
#       el código "Estrategia y estadísticas NOMBREACTIVO (Réplica Swell).R". Se
#       debe verificar que el archivo solo contenga la información mencionada y
#       nada adicional (incluso celdas borradas).

# 0.3.2 Guardar con el nombre "Parametros fractales.xlsx" el archivo que contiene
#       las reglas y variables que se emplean para las condiciones de compra y
#       venta de cada una de las estrategias diarias e intradiarias, así como 
#       de la estrategia intradiaria de referencia, que debe quedar en el última
#       fila del archivo. Se debe verificar que el archivo solo contenga la 
#       información mencionada y nada adicional (incluso celdas borradas).

# 1. PAQUETES Y CONFIGURACIONES ###############################################

# Paquetes
Libraries <- c("readxl",      # read_excel
               "rstudioapi",  # getActiveDocumentContext
               "lubridate",   # makedatetime, year, month,.., second
               "ggplot2",     # ggplot  
               "data.table",  # Data manipulation
               "zoo",          # rollapplyr
               "tidyr")       # fill 

# Instalación/cargue de paquetes
for (L in Libraries) {
  eval(parse(text = (paste0("if (!require(",
                            L,
                            ")) install.packages('",
                            L,
                            "')
                            library(",
                            L,
                            ")"
                            )
                     )
             )
       )
}

# Ubicación archivos de origen
BaseDirPath = dirname(getActiveDocumentContext()$path)
setwd(BaseDirPath)

#Plantilla gráficos


# 2. LECTURA Y PREPARACIÓN DE DATOS ###########################################

# Lectura de archivo histórico de precios 
ArchivoCargue <- "Data ETF ECH.xlsx"
BDPI <- read_excel(ArchivoCargue, sheet = "DATOS ECH")

# Asignación de títulos a las columnas
colnames(BDPI) <- c("DATE","FRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")

# Reorganizacion de fechas y franjas horarias
BDPI$DATEFRAME <- make_datetime(year = year(BDPI$DATE), 
                                month = month(BDPI$DATE), 
                                day = day(BDPI$DATE), 
                                hour = hour(BDPI$FRAME), 
                                min = minute(BDPI$FRAME), 
                                sec = second(BDPI$FRAME)
                               )

BDPI[,c("DATE","FRAME")] <- NULL
BDPI <- BDPI[,c("DATEFRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")]

# Tamaño y ordenamiento de datos
N <- length(BDPI$DATEFRAME)
BDPI <- BDPI[order(BDPI$DATEFRAME),]


# 3. CODIFICACIÓN DE FRACTALES INTRADIARIOS ##################################

# Insumos parametrización FI
ArchivoFractales <- "Parametros fractales.xlsx"
ParamFracI <- read_excel(ArchivoFractales, sheet = "Parametros fractales")
NFrac <- length(ParamFracI$Estrategia) - 1 # Número de fractales (se asume que la última fila corresponde a FIRX)
ParamFracI <- ParamFracI[-(NFrac+1),] # Eliminación de parámetros del fractal intradiario de referencia

# Variables de codificación de fractales intradiarios
ParamFracI$VentanaMovil <- ParamFracI$`Periodo fin` - ParamFracI$`Periodo inicio` + 1
ParamFracI$Desfase <- -ParamFracI$`Periodo fin`
ParamFracI$Fractal <- paste0("FI", ParamFracI$Estrategia)
ParamFracI$NombreRefCompra <- paste0(ParamFracI$Fractal,
                                     "_B_",
                                     ParamFracI$`Función compra`,
                                     "_",
                                     ParamFracI$`Ref. compra`,
                                     "_",
                                     -ParamFracI$`Periodo inicio`,
                                     "_",
                                     -ParamFracI$`Periodo fin`
                                    )
ParamFracI$NombreRefVenta <- paste0(ParamFracI$Fractal,
                                    "_S_",
                                    ParamFracI$`Función venta`,
                                    "_",
                                    ParamFracI$`Ref. venta`,
                                    "_",
                                    -ParamFracI$`Periodo inicio`,
                                    "_",
                                    -ParamFracI$`Periodo fin`
                                   )
ParamFracI$VariableCompra <- paste0("BDPI$",ParamFracI$`Variable compra`)
ParamFracI$RefCompra <- paste0("BDPI$",ParamFracI$`Ref. compra`)
ParamFracI$VariableVenta <- paste0("BDPI$",ParamFracI$`Variable venta`)
ParamFracI$RefVenta <- paste0("BDPI$",ParamFracI$`Ref. venta`)


# 4. CÁLCULO SEÑALES DE FRACTALES INTRADIARIOS ################################

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# intradiario de compra.Ejemplo FI1:
# BDPI$FI1_B_max_HIGH_4_1 <- rollapplyr(data = BDPI$HIGH, width = 4, FUN = max, fill = NA)
FunFracCompra <- paste0("BDPI$", ParamFracI$NombreRefCompra, 
                   " <- rollapplyr(data = ", ParamFracI$RefCompra, 
                   ", width = ", ParamFracI$VentanaMovil,
                   ", FUN = ", ParamFracI$`Función compra`, ", fill = NA)"
                  )
eval(parse(text = FunFracCompra))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# intradiario de compra.Ejemplo FI1:
# BDPI$FI1_B_max_HIGH_4_1 <- shift(x = BDPI$FI1_B_max_HIGH_4_1, n = 1, fill = NA)
FunDesfaseCompra <- paste0("BDPI$", ParamFracI$NombreRefCompra,
                      " <- shift(x = ", "BDPI$", ParamFracI$NombreRefCompra, 
                      ", n = ", ParamFracI$Desfase, ", fill = NA)"
                     )
eval(parse(text = FunDesfaseCompra))

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# intradiario de venta. Ejemplo FI1:
# BDPI$FI1_S_min_LOW_4_1 <- rollapplyr(data = BDPI$LOW, width = 4, FUN = min, fill = NA)
FunFracVenta <- paste0("BDPI$", ParamFracI$NombreRefVenta, 
                       " <- rollapplyr(data = ", ParamFracI$RefVenta, 
                       ", width = ", ParamFracI$VentanaMovil,
                       ", FUN = ", ParamFracI$`Función venta`, ", fill = NA)"
                      )
eval(parse(text = FunFracVenta))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# intradiario de venta. Ejemplo FI1:
# BDPI$FI1_S_min_LOW_4_1 <- shift(x = BDPI$FI1_S_min_LOW_4_1, n = 1, fill = NA)
FunDesfaseVenta <-paste0("BDPI$", ParamFracI$NombreRefVenta,
                         " <- shift(x = ", "BDPI$", ParamFracI$NombreRefVenta, 
                         ", n = ", ParamFracI$Desfase, ", fill = NA)"
                        ) 
eval(parse(text = FunDesfaseVenta))

# Señal para condiciones de "BUY" o "SELL" en cada fractal intradiario. Ejemplo FI1:
# BDPI$FI1 <- ifelse (BDPI$CLOSE > BDPI$FI1_B_max_HIGH_4_1, "BUY", ifelse(BDPI$CLOSE < BDPI$FI1_S_min_LOW_4_1, "SELL", NA))
FunSenFrac <- paste0("BDPI$", ParamFracI$Fractal,
                     " <- ifelse(",
                                 ParamFracI$VariableCompra, ParamFracI$`Criterio compra`, "BDPI$", ParamFracI$NombreRefCompra, 
                                 ", 'BUY', ",
                                 "ifelse(",
                                         ParamFracI$VariableVenta, ParamFracI$`Criterio venta`, "BDPI$", ParamFracI$NombreRefVenta, 
                                         ", 'SELL', ",
                                         "NA",
                                        ")",
                                ")"
                    )
eval(parse(text = FunSenFrac))

# Señal para cada fractal intradiario cuando no se cumplen las condiciones de "BUY" o "SELL".
BDPI <- fill(data = BDPI, ParamFracI$Fractal, .direction = "down")


# 5. VELA DIARIA #############################################################

BDPD <- BDPI
BDPD$DATE <- as.Date(BDPD$DATEFRAME)
BDPD <- BDPD[,c("DATE","DATEFRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")]
BDPD <- as.data.table(BDPD)
BDPD <- BDPD[, 
             .(VOLUME = sum(VOLUME),
               OPEN = first(OPEN),
               HIGH = max(HIGH),
               LOW = min(LOW),
               CLOSE = last(CLOSE)
             ),
             by = "DATE"
]


# 6. CODIFICACIÓN DE FRACTALES DIARIOS (INCLUYE FIRX) #########################

# Variables de codificación de fractales diarios
ParamFracD <- read_excel(ArchivoFractales, sheet = "Parametros fractales")

# Insumos parametrización FIR
ParamFracD[(NFrac+2):(NFrac*2),] <- ParamFracD[(NFrac+1),] # Réplica de parámetros de FIR
LetraFIR <- "R" # Letra para identificación de fractales intradiarios de referencia
ParamFracD$Estrategia[(NFrac+1):(NFrac*2)] <- paste0(LetraFIR, (1:NFrac)) # Asignación de nombres de estrategias de referencia
ParamFracD$`Periodo inicio`[(NFrac+1):(NFrac*2)] <- ParamFracD$`Periodo inicio`[1:NFrac] # Réplica de periodo inicio de estrategia
ParamFracD$`Periodo fin`[(NFrac+1):(NFrac*2)] <- ParamFracD$`Periodo fin`[1:NFrac] # Réplica de periodo fin de estrategia

ParamFracD$VentanaMovil <- ParamFracD$`Periodo fin` - ParamFracD$`Periodo inicio` + 1
ParamFracD$Desfase <- -ParamFracD$`Periodo fin`
ParamFracD$Fractal <- paste0("FD", ParamFracD$Estrategia)
ParamFracD$NombreRefCompra <- paste0(ParamFracD$Fractal,
                                     "_B_",
                                     ParamFracD$`Función compra`,
                                     "_",
                                     ParamFracD$`Ref. compra`,
                                     "_",
                                     -ParamFracD$`Periodo inicio`,
                                     "_",
                                     -ParamFracD$`Periodo fin`
                                    )
ParamFracD$NombreRefVenta <- paste0(ParamFracD$Fractal,
                                    "_S_",
                                    ParamFracD$`Función venta`,
                                    "_",
                                    ParamFracD$`Ref. venta`,
                                    "_",
                                    -ParamFracD$`Periodo inicio`,
                                    "_",
                                    -ParamFracD$`Periodo fin`
                                   )
ParamFracD$VariableCompra <- paste0("BDPD$",ParamFracD$`Variable compra`)
ParamFracD$RefCompra <- paste0("BDPD$",ParamFracD$`Ref. compra`)
ParamFracD$VariableVenta <- paste0("BDPD$",ParamFracD$`Variable venta`)
ParamFracD$RefVenta <- paste0("BDPD$",ParamFracD$`Ref. venta`)

# Separación entre parámetros para FIR y para FD
ParamFracR <- ParamFracD[match(paste0(LetraFIR,(1:NFrac)),ParamFracD$Estrategia),]
ParamFracR$Fractal <- ParamFracR$Estrategia # Réplica de nombres para FIR
ParamFracD <- ParamFracD[-match(paste0(LetraFIR,(1:NFrac)),ParamFracD$Estrategia),] # Eliminación parámetros creados para FIRX.
ParamFracR$VariableCompra <- paste0("BDPI$",ParamFracD$`Variable compra`)
ParamFracR$VariableVenta <- paste0("BDPI$",ParamFracD$`Variable venta`)


# 7. CÁLCULO SEÑALES DE FRACTALES DIARIOS #####################################

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# diario de compra.Ejemplo FD1:
# BDPD$FD1_B_max_HIGH_4_1 <- rollapplyr(data = BDPD$HIGH, width = 4, FUN = max, fill = NA)
FunFracCompra <- paste0("BDPD$", ParamFracD$NombreRefCompra, 
                        " <- rollapplyr(data = ", ParamFracD$RefCompra, 
                        ", width = ", ParamFracD$VentanaMovil,
                        ", FUN = ", ParamFracD$`Función compra`, ", fill = NA)"
                       )
eval(parse(text = FunFracCompra))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# diario de compra.Ejemplo FD1:
# BDPD$FD1_B_max_HIGH_4_1 <- shift(x = BDPD$FD1_B_max_HIGH_4_1, n = 1, fill = NA)
FunDesfaseCompra <- paste0("BDPD$", ParamFracD$NombreRefCompra,
                           " <- shift(x = ", "BDPD$", ParamFracD$NombreRefCompra, 
                           ", n = ", ParamFracD$Desfase, ", fill = NA)"
                          )
eval(parse(text = FunDesfaseCompra))

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# diario de venta. Ejemplo FD1:
# BDPD$FD1_S_min_LOW_4_1 <- rollapplyr(data = BDPD$LOW, width = 4, FUN = min, fill = NA)
FunFracVenta <- paste0("BDPD$", ParamFracD$NombreRefVenta, 
                       " <- rollapplyr(data = ", ParamFracD$RefVenta, 
                       ", width = ", ParamFracD$VentanaMovil,
                       ", FUN = ", ParamFracD$`Función venta`, ", fill = NA)"
                      )
eval(parse(text = FunFracVenta))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# diario de venta. Ejemplo FD1:
# BDPD$FD1_S_min_LOW_4_1 <- shift(x = BDPD$FD1_S_min_LOW_4_1, n = 1, fill = NA)
FunDesfaseVenta <-paste0("BDPD$", ParamFracD$NombreRefVenta,
                         " <- shift(x = ", "BDPD$", ParamFracD$NombreRefVenta, 
                         ", n = ", ParamFracD$Desfase, ", fill = NA)"
                        ) 
eval(parse(text = FunDesfaseVenta))

# Señal para condiciones de "BUY" o "SELL" en cada fractal diario. Ejemplo FD1:
# BDPD$FD1 <- ifelse (BDPD$CLOSE > BDPD$FD1_B_max_HIGH_4_1, "BUY", ifelse(BDPD$CLOSE < BDPD$FD1_S_min_LOW_4_1, "SELL", NA))
FunSenFrac <- paste0("BDPD$", ParamFracD$Fractal,
                     " <- ifelse(",
                                 ParamFracD$VariableCompra, ParamFracD$`Criterio compra`, "BDPD$", ParamFracD$NombreRefCompra, 
                                 ", 'BUY', ",
                                 "ifelse(",
                                         ParamFracD$VariableVenta, ParamFracD$`Criterio venta`, "BDPD$", ParamFracD$NombreRefVenta, 
                                         ", 'SELL', ",
                                         "NA",
                                       ")",
                               ")"
                    )
eval(parse(text = FunSenFrac))

# Señal para cada fractal diario cuando no se cumplen las condiciones de "BUY" o "SELL".
BDPD <- fill(data = BDPD, ParamFracD$Fractal, .direction = "down")


# 8. CÁLCULO SEÑALES DE FRACTALES INTRADIARIOS DE REFERENCIA ##################

# AQUI VAMOS ##################################################################
# Vamos a calcular los FIR en BDPD y los vamos importar BDPI.

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# diario de referencia de compra.Ejemplo FD1:
# BDPD$FIR1_B_max_HIGH_4_1 <- rollapplyr(data = BDPD$HIGH, width = 4, FUN = max, fill = NA)
FunFracCompra <- paste0("BDPD$", ParamFracR$NombreRefCompra, 
                        " <- rollapplyr(data = ", ParamFracR$RefCompra, 
                        ", width = ", ParamFracR$VentanaMovil,
                        ", FUN = ", ParamFracR$`Función compra`, ", fill = NA)"
)
eval(parse(text = FunFracCompra))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# diario de referencia de compra.Ejemplo FD1:
# BDPD$FD1_B_max_HIGH_4_1 <- shift(x = BDPD$FD1_B_max_HIGH_4_1, n = 1, fill = NA)
FunDesfaseCompra <- paste0("BDPD$", ParamFracD$NombreRefCompra,
                           " <- shift(x = ", "BDPD$", ParamFracD$NombreRefCompra, 
                           ", n = ", ParamFracD$Desfase, ", fill = NA)"
)
eval(parse(text = FunDesfaseCompra))

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# diario de referencia de venta. Ejemplo FD1:
# BDPD$FD1_S_min_LOW_4_1 <- rollapplyr(data = BDPD$LOW, width = 4, FUN = min, fill = NA)
FunFracVenta <- paste0("BDPD$", ParamFracD$NombreRefVenta, 
                       " <- rollapplyr(data = ", ParamFracD$RefVenta, 
                       ", width = ", ParamFracD$VentanaMovil,
                       ", FUN = ", ParamFracD$`Función venta`, ", fill = NA)"
)
eval(parse(text = FunFracVenta))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# diario de referencia de venta. Ejemplo FD1:
# BDPD$FD1_S_min_LOW_4_1 <- shift(x = BDPD$FD1_S_min_LOW_4_1, n = 1, fill = NA)
FunDesfaseVenta <-paste0("BDPD$", ParamFracD$NombreRefVenta,
                         " <- shift(x = ", "BDPD$", ParamFracD$NombreRefVenta, 
                         ", n = ", ParamFracD$Desfase, ", fill = NA)"
) 
eval(parse(text = FunDesfaseVenta))














# Identificación de fecha
BDPI$DATE <- NA
BDPI$DATE <- make_datetime(year = year(BDPI$DATEFRAME), 
                           month = month(BDPI$DATEFRAME), 
                           day = day(BDPI$DATEFRAME), 
                          )

# Importe a la base de datos intradiaria de la función sobre la referencia 
# correspondiente a cada fractal diario de compra. Ejemplo FIR1: 
# BDPI$FIR1_B_max_HIGH_4_1 <- BDPD$FD1_B_max_HIGH_4_1[match(as.Date(BDPI$DATE), BDPD$DATE)]


















# Señal para condiciones de "BUY" o "SELL" en cada fractal intradiario. Ejemplo FI1:
# BDPI$FIR1 <- ifelse (BDPI$CLOSE > BDPI$FIR1_B_max_HIGH_4_1, "BUY", ifelse(BDPI$CLOSE < BDPI$FIR1_S_min_LOW_4_1, "SELL", NA))
FunSenFrac <- paste0("BDPI$", ParamFracI$Fractal,
                     " <- ifelse(",
                     ParamFracI$VariableCompra, ParamFracI$`Criterio compra`, "BDPI$", ParamFracI$NombreRefCompra, 
                     ", 'BUY', ",
                     "ifelse(",
                     ParamFracI$VariableVenta, ParamFracI$`Criterio venta`, "BDPI$", ParamFracI$NombreRefVenta, 
                     ", 'SELL', ",
                     "NA",
                     ")",
                     ")"
)
eval(parse(text = FunSenFrac))

# Señal para cada fractal intradiario cuando no se cumplen las condiciones de "BUY" o "SELL".
BDPI <- fill(data = BDPI, ParamFracI$Fractal, .direction = "down")















# 4. POSICIÓN Y VALORACIÓN ####################################################

# Volumen de posición de entrada (VOL_PE),  límites de la estrategia y % de cierre parcial
VOL_ENTRADA <- 100         # Volumnen de posición de entrada
LminPA_V_PE <- 0.025       # Límite mínimo de precio apertura (PA) frente a precio entrada
LminPmin_V_PE <- 0.025     # Límite mínimo de precio mínimo (Pmin) frente a precio entrada 
LmedioPA_V_PE <- 0.015     # Límite medio de precio apertura (PA) frente a precio entrada
LmedioPmin_V_PE <- 0.015   # Límite medio de precio mínimo (Pmin) frente a precio entrada
LmedioPmax_V_PE <- 0.015   # Límite medio de precio máximo (Pmax) frente a precio entrada
PORC_CIERRE_PARCIAL <- 0.5 # Porcentaje de cierre parcial de posiciones

# Signos de posiciones cortas o largas para las señales
BDP$SENALSIGNO[BDP$SENAL=="BUY"] <- 1
BDP$SENALSIGNO[BDP$SENAL=="SELL"] <- -1

# Columnas para precio de entrada (P_ENTRADA), volumen y valor de posición inicial y final
# (VOL_POSINICIAL, VOL_POSFINAL, VAL_POSINICIAL y VAL_POSFINAL), volumen y valor de compras
# y ventas (VOL_COMPRAS, VOL_VENTAS, VAL_COMPRAS y VAL_VENTAS)
BDP$P_ENTRADA <- c(BDP$CLOSE[1], rep(0, (N-1)))
BDP$VOL_POSINICIAL <- NA
BDP$VAL_POSINICIAL <- NA
BDP$VOL_COMPRAS <- c((VOL_ENTRADA), rep(NA, (N-1)))
BDP$VAL_COMPRAS <- c((VOL_ENTRADA*BDP$P_ENTRADA[1]), rep(NA, (N-1)))
BDP$VOL_VENTAS <- c(0, rep(NA, (N-1)))
BDP$VAL_VENTAS <- c(0, rep(NA, (N-1)))
BDP$VOL_POSFINAL <- c(VOL_ENTRADA, rep(NA, (N-1)))
BDP$VAL_POSFINAL <- c((VOL_ENTRADA*BDP$CLOSE[1]), rep(NA, (N-1)))
BDP$EFECTIVO <- c(0, rep(NA, (N-1)))
BDP$VAL_PORT <- c((BDP$VOL_POSFINAL[1]*BDP$CLOSE[1]), rep(NA, (N-1)))

# Cálculo de posición y valoración inicial y final, volumen y valoración de ventas y compras,
# precio de entrada y valoración de efectivo y portafolio
for (d in 2:N) {
  
  BDP$VOL_POSINICIAL[d] <- BDP$VOL_POSFINAL[d-1]
  BDP$VAL_POSINICIAL[d] <- BDP$VAL_POSFINAL[d-1]
  
  if (BDP$SENAL[d] != BDP$SENAL[d-1]) { # Si la señal cambia:
    
    BDP$VOL_VENTAS[d] <- BDP$VOL_POSINICIAL[d]
    BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * BDP$CLOSE[d]
    BDP$P_ENTRADA[d] <- BDP$CLOSE[d]
    BDP$VAL_COMPRAS[d] <- BDP$EFECTIVO[d-1] + BDP$VAL_VENTAS[d]
    BDP$VOL_COMPRAS[d] <- BDP$VAL_COMPRAS[d] / BDP$P_ENTRADA[d]
    
  } else { # Si la señal NO cambia:
    
    if (BDP$VOL_POSINICIAL[d] != 0) {# Si hay posición
      
      BDP$VAL_COMPRAS[d] <- 0
      BDP$VOL_COMPRAS[d] <- 0
      
      if (((BDP$OPEN[d]/BDP$P_ENTRADA[d-1])-1) <= -LminPA_V_PE) { # Si PA≤0,975*PE:
        # Corto SiPA>=1.025*PE 
        
        BDP$VOL_VENTAS[d] <- BDP$VOL_POSINICIAL[d]   # Cierro toda la posición
        BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * BDP$OPEN[d]   # Cierro a PA
        BDP$P_ENTRADA[d] <- NA
        
      } else { # Si PA>0,975*PE:
        
        if (((BDP$OPEN[d]/BDP$P_ENTRADA[d-1])-1) <= -LmedioPA_V_PE) {# Si PA≤0,985*PE:
          
          BDP$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDP$VOL_POSINICIAL[d]  #Cierro parcial (50%)
          BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * BDP$OPEN[d]   #Cierro a PA    
          BDP$P_ENTRADA[d] <- BDP$P_ENTRADA[d-1]
          
          if (((BDP$LOW[d]/BDP$P_ENTRADA[d-1])-1) <= -LminPmin_V_PE) {# Si PMin≤0,975*PE:
            
            BDP$VOL_VENTAS[d] <- BDP$VOL_VENTAS[d] + (1 - PORC_CIERRE_PARCIAL) * BDP$VOL_POSINICIAL[d]    # Termino de cerrar posición
            BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * ((1-LminPmin_V_PE) * BDP$P_ENTRADA[d])  # Cierro a PE
            BDP$P_ENTRADA[d] <- NA   
            
          } else {# Si PMin>0,975*PE:
            
            if (((BDP$HIGH[d]/BDP$P_ENTRADA[d-1])-1) >= LmedioPmax_V_PE) {# Si Pmax≥1,015*PE 
              
              BDP$VOL_VENTAS[d] <- BDP$VOL_VENTAS[d] + (1 - PORC_CIERRE_PARCIAL) * BDP$VOL_POSINICIAL[d]   # Termino de cerrar posición
              BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * ((1+LmedioPmax_V_PE) * BDP$P_ENTRADA[d])   # Cierro a PE
              BDP$P_ENTRADA[d] <- NA
              
            } else {# Si Pmax<1,015*PE, se mantiene 50% de posición abierta
              
              # No se requiere registrar nada más.
              
            }
            
          }
          
        } else {
          
          if (((BDP$OPEN[d]/BDP$P_ENTRADA[d-1])-1) <= LmedioPA_V_PE) {# Si PA≤1,015*PE 
            
            if (((BDP$LOW[d]/BDP$P_ENTRADA[d-1])-1) <= -LminPmin_V_PE) {# Si PMin≤0,975*PE 
              
              BDP$VOL_VENTAS[d] <- BDP$VOL_POSINICIAL[d]   
              BDP$VAL_VENTAS[d] <- 0.5 * BDP$VOL_VENTAS[d] * ((1-LmedioPmin_V_PE) * BDP$P_ENTRADA[d])
              + 0.5 * BDP$VOL_VENTAS[d] * ((1-LminPmin_V_PE) * BDP$P_ENTRADA[d])
              BDP$P_ENTRADA[d] <- NA
              
            } else {# Si PMin>0,975*PE
              
              if (((BDP$LOW[d]/BDP$P_ENTRADA[d-1])-1) <= -LmedioPmin_V_PE) {# Si PMin≤0,985*PE
                
                BDP$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDP$VOL_POSINICIAL[d]
                BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * ((1-LmedioPmin_V_PE) * BDP$P_ENTRADA[d])
                BDP$P_ENTRADA[d] <- BDP$P_ENTRADA[d-1]
                
              } else {# Si PMin>0,985*PE
                
                if (((BDP$HIGH[d]/BDP$P_ENTRADA[d-1])-1) >= LmedioPmax_V_PE) {# Si Pmax≥1,015*PE
                  
                  BDP$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDP$VOL_POSINICIAL[d]
                  BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * ((1+LmedioPmax_V_PE) * BDP$P_ENTRADA[d])
                  BDP$P_ENTRADA[d] <- BDP$P_ENTRADA[d-1]
                  
                } else {# Si Pmax<1,015*PE, se mantiene 100% de posición abierta
                  
                  BDP$VOL_VENTAS[d] <- 0
                  BDP$VAL_VENTAS[d] <- 0
                  BDP$P_ENTRADA[d] <- BDP$P_ENTRADA[d-1]
                  
                }
                
              }
              
            }
            
          } else {
            
            BDP$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDP$VOL_POSINICIAL[d]
            BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * BDP$OPEN[d]
            BDP$P_ENTRADA[d] <- BDP$P_ENTRADA[d-1]
            
            if (BDP$LOW[d] <= BDP$P_ENTRADA[d]) {# Si PMin≤PE:
              
              BDP$VOL_VENTAS[d] <- BDP$VOL_VENTAS[d] + (1 - PORC_CIERRE_PARCIAL) * BDP$VOL_POSINICIAL[d]
              BDP$VAL_VENTAS[d] <- BDP$VOL_VENTAS[d] * BDP$P_ENTRADA[d]
              BDP$P_ENTRADA[d] <- NA
              
            } else {# Si PMin>PE, se mantiene 50% de posición abierta:
              
              # No se requiere registrar nada más.
              
            }
            
          }
          
        }
        
      }
      
    } else {
      
      BDP$VAL_COMPRAS[d] <- 0
      BDP$VOL_COMPRAS[d] <- 0
      BDP$VOL_VENTAS[d] <- 0
      BDP$VAL_VENTAS[d] <- 0
      BDP$P_ENTRADA[d] <- BDP$P_ENTRADA[d-1]
      
    }
    
  }
  
  BDP$VOL_POSFINAL[d] <- BDP$VOL_POSINICIAL[d] + BDP$VOL_COMPRAS[d] - BDP$VOL_VENTAS[d]
  BDP$VAL_POSFINAL[d] <- BDP$VOL_POSFINAL[d] * BDP$CLOSE[d]
  BDP$EFECTIVO[d] <- BDP$EFECTIVO[d-1] + BDP$VAL_VENTAS[d] - BDP$VAL_COMPRAS[d]
  BDP$VAL_PORT[d] <- BDP$VAL_POSFINAL[d] + BDP$EFECTIVO[d]
  
}


# 5. CÁLCULO ESTADÍSTICAS #####################################################

BDP$RET <- c(0, rep(NA, (N-1)))
BDP$RET_ACUM <- c(0, rep(NA, (N-1)))
BDP$VAL_PORT_ACUM_B100 <- c(100, rep(NA, (N-1)))
BDP$MAX_PERD_ACUM <- c(0, rep(NA, (N-1)))

BDP$RET <- (BDP$VAL_PORT / c(NA, BDP$VAL_PORT[1:N-1]) -1) * c(NA, BDP$SENALSIGNO[1:N-1]) 
BDP$RET_ACUM <- c(NA, cumprod(1 + BDP$RET[2:N]))
RET_ACUM <- BDP$RET_ACUM[N] - 1
NDIAS <- as.numeric(BDP$DATE[N] - BDP$DATE[1])
RET_ACUM_ANUAL <- (1 + RET_ACUM)^(365/(NDIAS-1))-1


# 5. RESUMEN ESTADÍSTICAS  ####################################################


# 6. OBSERVACIONES ############################################################

#    1. Apalancamiento mal
#    2. Retorno mal
