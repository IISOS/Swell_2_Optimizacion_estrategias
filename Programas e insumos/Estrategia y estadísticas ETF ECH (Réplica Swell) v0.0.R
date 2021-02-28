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
               "lubridate")   # makedatetime, year, month,.., second

# Instalación/cargue de paquetes
for (L in Libraries) {
  eval(parse(text = (paste0("if (!require(",
                            L,
                            ")) install.packages('",
                            L,
                            "')
                            library(",
                            L,
                            ")"))))
}

# Ubicación archivos de origen
BaseDirPath = dirname(getActiveDocumentContext()$path)
setwd(BaseDirPath)

#Plantilla gráficos


# 2. LECTURA Y PREPARACIÓN DE DATOS ###########################################

# Lectura de archivo histórico de precios 
ArchivoCargue <- "Data ETF ECH.xlsx"
BDPS <- read_excel(ArchivoCargue, sheet = "DATOS ECH")

# Asignación de títulos a las columnas
colnames(BDPS) <- c("DATE","FRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")

# Reorganizacion de fechas y franjas horarias
BDPS$DATEFRAME <- make_datetime(year = year(BDPS$DATE), 
                                month = month(BDPS$DATE), 
                                day = day(BDPS$DATE), 
                                hour = hour(BDPS$FRAME), 
                                min = minute(BDPS$FRAME), 
                                sec = second(BDPS$FRAME))

BDPS[,c("DATE","FRAME")] <- NULL
BDPS <- BDPS[,c("DATEFRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")]

# Tamaño y ordenamiento de datos
N <- length(BDPS$DATE)
BDPS <- BDPS[order(c(BDPS$DATE,BDPS$FRAME)),]


# 3. CÁLCULO SEÑALES ##########################################################


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
BDPS$SENALSIGNO[BDPS$SENAL=="BUY"] <- 1
BDPS$SENALSIGNO[BDPS$SENAL=="SELL"] <- -1

# Columnas para precio de entrada (P_ENTRADA), volumen y valor de posición inicial y final
# (VOL_POSINICIAL, VOL_POSFINAL, VAL_POSINICIAL y VAL_POSFINAL), volumen y valor de compras
# y ventas (VOL_COMPRAS, VOL_VENTAS, VAL_COMPRAS y VAL_VENTAS)
BDPS$P_ENTRADA <- c(BDPS$CLOSE[1], rep(0, (N-1)))
BDPS$VOL_POSINICIAL <- NA
BDPS$VAL_POSINICIAL <- NA
BDPS$VOL_COMPRAS <- c((VOL_ENTRADA), rep(NA, (N-1)))
BDPS$VAL_COMPRAS <- c((VOL_ENTRADA*BDPS$P_ENTRADA[1]), rep(NA, (N-1)))
BDPS$VOL_VENTAS <- c(0, rep(NA, (N-1)))
BDPS$VAL_VENTAS <- c(0, rep(NA, (N-1)))
BDPS$VOL_POSFINAL <- c(VOL_ENTRADA, rep(NA, (N-1)))
BDPS$VAL_POSFINAL <- c((VOL_ENTRADA*BDPS$CLOSE[1]), rep(NA, (N-1)))
BDPS$EFECTIVO <- c(0, rep(NA, (N-1)))
BDPS$VAL_PORT <- c((BDPS$VOL_POSFINAL[1]*BDPS$CLOSE[1]), rep(NA, (N-1)))

# Cálculo de posición y valoración inicial y final, volumen y valoración de ventas y compras,
# precio de entrada y valoración de efectivo y portafolio
for (d in 2:N) {
  
  BDPS$VOL_POSINICIAL[d] <- BDPS$VOL_POSFINAL[d-1]
  BDPS$VAL_POSINICIAL[d] <- BDPS$VAL_POSFINAL[d-1]
  
  if (BDPS$SENAL[d] != BDPS$SENAL[d-1]) { # Si la señal cambia:
    
    BDPS$VOL_VENTAS[d] <- BDPS$VOL_POSINICIAL[d]
    BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * BDPS$CLOSE[d]
    BDPS$P_ENTRADA[d] <- BDPS$CLOSE[d]
    BDPS$VAL_COMPRAS[d] <- BDPS$EFECTIVO[d-1] + BDPS$VAL_VENTAS[d]
    BDPS$VOL_COMPRAS[d] <- BDPS$VAL_COMPRAS[d] / BDPS$P_ENTRADA[d]
    
  } else { # Si la señal NO cambia:
    
    if (BDPS$VOL_POSINICIAL[d] != 0) {# Si hay posición
      
      BDPS$VAL_COMPRAS[d] <- 0
      BDPS$VOL_COMPRAS[d] <- 0
      
      if (((BDPS$OPEN[d]/BDPS$P_ENTRADA[d-1])-1) <= -LminPA_V_PE) { # Si PA≤0,975*PE:
        # Corto SiPA>=1.025*PE 
        
        BDPS$VOL_VENTAS[d] <- BDPS$VOL_POSINICIAL[d]   # Cierro toda la posición
        BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * BDPS$OPEN[d]   # Cierro a PA
        BDPS$P_ENTRADA[d] <- NA
        
      } else { # Si PA>0,975*PE:
        
        if (((BDPS$OPEN[d]/BDPS$P_ENTRADA[d-1])-1) <= -LmedioPA_V_PE) {# Si PA≤0,985*PE:
          
          BDPS$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDPS$VOL_POSINICIAL[d]  #Cierro parcial (50%)
          BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * BDPS$OPEN[d]   #Cierro a PA    
          BDPS$P_ENTRADA[d] <- BDPS$P_ENTRADA[d-1]
          
          if (((BDPS$LOW[d]/BDPS$P_ENTRADA[d-1])-1) <= -LminPmin_V_PE) {# Si PMin≤0,975*PE:
            
            BDPS$VOL_VENTAS[d] <- BDPS$VOL_VENTAS[d] + (1 - PORC_CIERRE_PARCIAL) * BDPS$VOL_POSINICIAL[d]    # Termino de cerrar posición
            BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * ((1-LminPmin_V_PE) * BDPS$P_ENTRADA[d])  # Cierro a PE
            BDPS$P_ENTRADA[d] <- NA   
            
          } else {# Si PMin>0,975*PE:
            
            if (((BDPS$HIGH[d]/BDPS$P_ENTRADA[d-1])-1) >= LmedioPmax_V_PE) {# Si Pmax≥1,015*PE 
              
              BDPS$VOL_VENTAS[d] <- BDPS$VOL_VENTAS[d] + (1 - PORC_CIERRE_PARCIAL) * BDPS$VOL_POSINICIAL[d]   # Termino de cerrar posición
              BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * ((1+LmedioPmax_V_PE) * BDPS$P_ENTRADA[d])   # Cierro a PE
              BDPS$P_ENTRADA[d] <- NA
              
            } else {# Si Pmax<1,015*PE, se mantiene 50% de posición abierta
              
              # No se requiere registrar nada más.
              
            }
            
          }
          
        } else {
          
          if (((BDPS$OPEN[d]/BDPS$P_ENTRADA[d-1])-1) <= LmedioPA_V_PE) {# Si PA≤1,015*PE 
            
            if (((BDPS$LOW[d]/BDPS$P_ENTRADA[d-1])-1) <= -LminPmin_V_PE) {# Si PMin≤0,975*PE 
              
              BDPS$VOL_VENTAS[d] <- BDPS$VOL_POSINICIAL[d]   
              BDPS$VAL_VENTAS[d] <- 0.5 * BDPS$VOL_VENTAS[d] * ((1-LmedioPmin_V_PE) * BDPS$P_ENTRADA[d])
              + 0.5 * BDPS$VOL_VENTAS[d] * ((1-LminPmin_V_PE) * BDPS$P_ENTRADA[d])
              BDPS$P_ENTRADA[d] <- NA
              
            } else {# Si PMin>0,975*PE
              
              if (((BDPS$LOW[d]/BDPS$P_ENTRADA[d-1])-1) <= -LmedioPmin_V_PE) {# Si PMin≤0,985*PE
                
                BDPS$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDPS$VOL_POSINICIAL[d]
                BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * ((1-LmedioPmin_V_PE) * BDPS$P_ENTRADA[d])
                BDPS$P_ENTRADA[d] <- BDPS$P_ENTRADA[d-1]
                
              } else {# Si PMin>0,985*PE
                
                if (((BDPS$HIGH[d]/BDPS$P_ENTRADA[d-1])-1) >= LmedioPmax_V_PE) {# Si Pmax≥1,015*PE
                  
                  BDPS$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDPS$VOL_POSINICIAL[d]
                  BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * ((1+LmedioPmax_V_PE) * BDPS$P_ENTRADA[d])
                  BDPS$P_ENTRADA[d] <- BDPS$P_ENTRADA[d-1]
                  
                } else {# Si Pmax<1,015*PE, se mantiene 100% de posición abierta
                  
                  BDPS$VOL_VENTAS[d] <- 0
                  BDPS$VAL_VENTAS[d] <- 0
                  BDPS$P_ENTRADA[d] <- BDPS$P_ENTRADA[d-1]
                  
                }
                
              }
              
            }
            
          } else {
            
            BDPS$VOL_VENTAS[d] <- PORC_CIERRE_PARCIAL * BDPS$VOL_POSINICIAL[d]
            BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * BDPS$OPEN[d]
            BDPS$P_ENTRADA[d] <- BDPS$P_ENTRADA[d-1]
            
            if (BDPS$LOW[d] <= BDPS$P_ENTRADA[d]) {# Si PMin≤PE:
              
              BDPS$VOL_VENTAS[d] <- BDPS$VOL_VENTAS[d] + (1 - PORC_CIERRE_PARCIAL) * BDPS$VOL_POSINICIAL[d]
              BDPS$VAL_VENTAS[d] <- BDPS$VOL_VENTAS[d] * BDPS$P_ENTRADA[d]
              BDPS$P_ENTRADA[d] <- NA
              
            } else {# Si PMin>PE, se mantiene 50% de posición abierta:
              
              # No se requiere registrar nada más.
              
            }
            
          }
          
        }
        
      }
      
    } else {
      
      BDPS$VAL_COMPRAS[d] <- 0
      BDPS$VOL_COMPRAS[d] <- 0
      BDPS$VOL_VENTAS[d] <- 0
      BDPS$VAL_VENTAS[d] <- 0
      BDPS$P_ENTRADA[d] <- BDPS$P_ENTRADA[d-1]
      
    }
    
  }
  
  BDPS$VOL_POSFINAL[d] <- BDPS$VOL_POSINICIAL[d] + BDPS$VOL_COMPRAS[d] - BDPS$VOL_VENTAS[d]
  BDPS$VAL_POSFINAL[d] <- BDPS$VOL_POSFINAL[d] * BDPS$CLOSE[d]
  BDPS$EFECTIVO[d] <- BDPS$EFECTIVO[d-1] + BDPS$VAL_VENTAS[d] - BDPS$VAL_COMPRAS[d]
  BDPS$VAL_PORT[d] <- BDPS$VAL_POSFINAL[d] + BDPS$EFECTIVO[d]
  
}


# 5. CÁLCULO ESTADÍSTICAS #####################################################

BDPS$RET <- c(0, rep(NA, (N-1)))
BDPS$RET_ACUM <- c(0, rep(NA, (N-1)))
BDPS$VAL_PORT_ACUM_B100 <- c(100, rep(NA, (N-1)))
BDPS$MAX_PERD_ACUM <- c(0, rep(NA, (N-1)))

BDPS$RET <- (BDPS$VAL_PORT / c(NA, BDPS$VAL_PORT[1:N-1]) -1) * c(NA, BDPS$SENALSIGNO[1:N-1]) 
BDPS$RET_ACUM <- c(NA, cumprod(1 + BDPS$RET[2:N]))
RET_ACUM <- BDPS$RET_ACUM[N] - 1
NDIAS <- as.numeric(BDPS$DATE[N] - BDPS$DATE[1])
RET_ACUM_ANUAL <- (1 + RET_ACUM)^(365/(NDIAS-1))-1


# 5. RESUMEN ESTADÍSTICAS  ####################################################


# 6. OBSERVACIONES ############################################################

#    1. Apalancamiento mal
#    2. Retorno mal
