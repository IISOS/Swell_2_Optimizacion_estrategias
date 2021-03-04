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

# 0.3.1 Guardar con el nombre "Data ETF ECH.xlsx" el archivo que contiene el
#       histórico de precios OHLC en la misma ruta donde se encuentra ubicado
#       el código "Estrategia y estadísticas ETF ECH (Réplica Swell).R". Se
#       debe verificar que el archivo solo contenga la información mencionada y
#       nada adicional (incluso celdas borradas).


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
                                sec = second(BDPI$FRAME))

BDPI[,c("DATE","FRAME")] <- NULL
BDPI <- BDPI[,c("DATEFRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")]

# Tamaño y ordenamiento de datos
N <- length(BDPI$DATEFRAME)
BDPI <- BDPI[order(BDPI$DATEFRAME),]


# 3. CÁLCULO VELA DIARIA ######################################################

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

# 3. CÁLCULO SEÑALES ##########################################################

# Parametrizacion fractales
ArchivoFractales <- "Parametros fractales.xlsx"
ParamFrac <- read_excel(ArchivoFractales, sheet = "Parametros fractales")

ParamFrac$VentanaMovil <- ParamFrac$`Periodo fin` - ParamFrac$`Periodo inicio` + 1
ParamFrac$Desfase <- -ParamFrac$`Periodo fin`
ParamFrac$NombreRefCompra <- paste0(ParamFrac$`Función compra`,
                                              ParamFrac$`Ref. compra`,
                                              "_",
                                              -ParamFrac$`Periodo inicio`,
                                              "_",
                                              -ParamFrac$`Periodo fin`)
ParamFrac$NombreRefVenta <- paste0(ParamFrac$`Función venta`,
                                              ParamFrac$`Ref. venta`,
                                              "_",
                                              -ParamFrac$`Periodo inicio`,
                                              "_",
                                              -ParamFrac$`Periodo fin`)
ParamFrac$FractalI <- substr(ParamFrac$Estrategia, 1, 3)
ParamFrac$FractalD <- substr(ParamFrac$Estrategia, 7, 9)
ParamFrac$VariableCompraI <- paste0("BDPI$",ParamFrac$`Variable compra`)
ParamFrac$VariableCompraD <- paste0("BDPD$",ParamFrac$`Variable compra`)
ParamFrac$RefCompraI <- paste0("BDPI$",ParamFrac$`Ref. compra`)
ParamFrac$RefCompraD <- paste0("BDPD$",ParamFrac$`Ref. compra`)
ParamFrac$VariableVentaI <- paste0("BDPI$",ParamFrac$`Variable venta`)
ParamFrac$VariableVentaD <- paste0("BDPD$",ParamFrac$`Variable venta`)
ParamFrac$RefVentaI <- paste0("BDPI$",ParamFrac$`Ref. venta`)
ParamFrac$RefVentaD <- paste0("BDPD$",ParamFrac$`Ref. venta`)



#Función para Cálculo de señales de un fractal
SenalFractal <- function(Fractal,BD,Parametros) {
  #PENDIENTE INCLUIR EJEMPLO Y GENERALIZAR
}

FractalIntradiario <- "FI1"
eval(parse(text = y))

#Para FI1 (CLOSE > max(HIGH) entre -4 y -1):

BDPI$MAX_HIGH_4_1 <- rollapplyr(data = BDPI$HIGH, width = 4, FUN = max, fill = NA)
BDPI$MAX_HIGH_4_1 <- shift(x = BDPI$MAX_HIGH_4_1, n = 1, fill = NA)

BDPI$MIN_LOW_4_1 <- rollapplyr(data = BDPI$LOW, width = 4, FUN = min, fill = NA)
BDPI$MIN_LOW_4_1 <- shift(x = BDPI$MIN_LOW_4_1, n = 1, fill = NA)

BDPI$FI1 <- ifelse (BDPI$CLOSE > BDPI$MAX_HIGH_4_1, 
                    "BUY", 
                    ifelse(BDPI$CLOSE < BDPI$MIN_LOW_4_1,
                           "SELL",
                           NA
                           )
                    )

BDPI <- fill(data = BDPI, FI1, .direction = "down")





f <- 1


# Cálculo de la funcion sobre la referencia para el fractal de compra.
# Ejemplo FI1:
# BDPI$MAX_HIGH_4_1 <- rollapplyr(data = BDPI$HIGH, width = 4, FUN = max, fill = NA)
eval(parse(text = paste0("BDPI$", ParamFrac$NombreRefCompra, 
                         " <- rollapplyr(data = ", ParamFrac$RefCompraI, 
                         ", width = ", ParamFrac$VentanaMovil,
                         ", FUN = ", ParamFrac$`Función compra`, ", fill = NA)"
                         )
           )
     )

# Aplicación del desfase de la ventana móvil a la serie.
# Ejemplo FI1:
# BDPI$MAX_HIGH_4_1 <- shift(x = BDPI$MAX_HIGH_4_1, n = 1, fill = NA)
eval(parse(text = paste0("BDPI$", ParamFrac$NombreRefCompra,
                         " <- shift(x = ", "BDPI$", ParamFrac$NombreRefCompra, 
                         ", n = ", ParamFrac$Desfase, ", fill = NA)"
                        )
          )
    )

# Cálculo de la funcion sobre la referencia para el fractal de venta.
# Ejemplo FI1:
# BDPI$MIN_LOW_4_1 <- rollapplyr(data = BDPI$LOW, width = 4, FUN = min, fill = NA)
eval(parse(text = paste0("BDPI$", ParamFrac$NombreRefVenta, 
                         " <- rollapplyr(data = ", ParamFrac$RefVentaI, 
                         ", width = ", ParamFrac$VentanaMovil,
                         ", FUN = ", ParamFrac$`Función venta`, ", fill = NA)"
                        )
          )
    )

# Aplicación del desfase de la ventana móvil a la serie.
# Ejemplo FI1:
# BDPI$MIN_LOW_4_1 <- shift(x = BDPI$MIN_LOW_4_1, n = 1, fill = NA)
eval(parse(text = paste0("BDPI$", ParamFrac$NombreRefVenta,
                         " <- shift(x = ", "BDPI$", ParamFrac$NombreRefVenta, 
                         ", n = ", ParamFrac$Desfase, ", fill = NA)"
                        )
          )
    )

# Señal del fractal.
# Ejemplo FI1:
# BDPI$FI1 <- ifelse (BDPI$CLOSE > BDPI$MAX_HIGH_4_1, "BUY", ifelse(BDPI$CLOSE < BDPI$MIN_LOW_4_1, "SELL", NA))
eval(parse(text = paste0("BDPI$", ParamFrac$FractalI,
                         " <- ifelse(",
                                     ParamFrac$VariableCompraI, ParamFrac$`Criterio compra`, "BDPI$", ParamFrac$NombreRefCompra, 
                                     ", 'BUY', ",
                                     "ifelse(",
                                             ParamFrac$VariableVentaI, ParamFrac$`Criterio venta`, "BDPI$", ParamFrac$NombreRefVenta, 
                                             ", 'SELL', ",
                                             "NA",
                                           ")",
                                   ")"
                        )
          )
    )



BDPI$FI1 <- ifelse (BDPI$CLOSE > BDPI$MAX_HIGH_4_1, 
                    "BUY", 
                    ifelse(BDPI$CLOSE < BDPI$MIN_LOW_4_1,
                           "SELL",
                           NA
                           )
                    )

BDPI <- fill(data = BDPI, FI1, .direction = "down")










# Seleccionar fractal diario (FD1,FD2,FD3,...,FDX)
FractalDiario <- "FD1"

# Seleccionar fractal intradiario (FI1,FI2,FI3,...,FIX)
FractalIntradiario <- "FI1"







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
