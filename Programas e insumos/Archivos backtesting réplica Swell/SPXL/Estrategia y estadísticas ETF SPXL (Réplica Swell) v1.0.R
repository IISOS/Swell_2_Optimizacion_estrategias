###  Estrategia y estadísticas ETF ECH (Réplica Swell)  ###
###                     2021-03-22                      ###
###                     Version 1.0                     ###  
###          Authors: Olga Serna / Ivan Serrano         ###


# 00. DESCRIPCIÓN E INSTRUCCIONES ##############################################

# 0.1 Objetivo

# Calcular la señal de la estrategia (óptima o deseada) y sus estadísticas de 
# retorno y riesgo a partir de serie histórica OHLC (apertura, máximo, mínimo y
# cierre) y un árbol de decisión de estrategia.

# 0.2 Supuestos

# 0.2.1 Se realizan operaciones a las que haya lugar después de formación de la
#       vela de mercado para cada franja horaria (FH), teniendo en cuenta el periodo
#       de tiempo permitido de operación definido para cada día, el cual excluye
#       las franjas horarias que se consideran de baja liquidez.

# 0.2.2 Todas las franjas horarias se tienen en cuenta para el cálculo de la
#       señal intradiaria pero no para el cálculo de la vela diaria y por lo tanto 
#       de los fractales intradiarios de referencia.

# 0.2.3 Se consigue operar en el mercado a los precios de cierre de la franja,
#       con costos transaccionales asociados a un bid-ask spread nulo.

# 0.2.3 No se considera la dinámica de los flujos de capital, pues  no se 
#       tienen en cuenta los faltantes o excedentes de efectivo que se pueden
#       presentar al cerrar y abrir posiciones.

# 0.2.4 Para los cálculos de utilidad (pérdida o ganancia en términos 
#       monetarios), se tiene en cuenta si la posición es larga o corta.

# 0.3 Instrucciones

# 0.3.1 Guardar con el nombre "Data NOMBREACTIVO.xlsx" el archivo que contiene
#       los insumos en la misma ruta donde se encuentra ubicado el código 
#       "Estrategia y estadísticas NOMBREACTIVO (Réplica Swell).R". Estos
#       insuoms incluyen:
#       - Hoja "OHLC": histórico de precios OHLC.
#       - Hoja "N_FH_Cierre_Descartadas": número de FH descartadas para negociar
#                                         antes del cierre.
#       - Hoja "U_MDD_Objetivo": valor objetivo de la razón U/MDD empleado para
#                                solo visualizar la información de las estrategias
#                                que cumplan con esta condición
#       El nombre del archivo de Excel debe asignarse a la variable "ArchivoCargue",
#       modificando la primera línea de código en la sección "02. LECTURA Y 
#       PREPARACIÓN DE DATOS". Se debe verificar que el archivo solo contenga la
#       información mencionada y nada adicional (incluso celdas borradas).

# 0.3.2 Guardar con el nombre "Parametros fractales.xlsx" el archivo que contiene
#       las reglas y variables que se emplean para las condiciones de compra y
#       venta de cada una de las estrategias diarias e intradiarias, así como 
#       de la estrategia intradiaria de referencia, que debe quedar en el última
#       fila del archivo. Se debe verificar que el archivo solo contenga la 
#       información mencionada y nada adicional (incluso celdas borradas).

# 0.3.3 La última sección del código permite visualizar la información y 
#       estadísticas de una estrategia en particular.

# 01. PAQUETES Y CONFIGURACIONES ###############################################

# Paquetes
Libraries <- c("readxl",      # read_excel
               "rstudioapi",  # getActiveDocumentContext
               "lubridate",   # makedatetime, year, month,.., second
               "ggplot2",     # ggplot  
               "data.table",  # Data manipulation
               "zoo",         # rollapplyr
               "tidyr",       # fill 
               "dplyr")       # group_by, slice_max 

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

# Borrado de información
rm(list = ls())

#Plantillas para gráficos
PlantillaG <- theme(plot.title = element_text(color = "grey20", angle = 0, hjust = 0.5, vjust = 0.5, face = "bold", margin = margin(b = 20)),
                    plot.subtitle = element_text(color = "grey20", angle = 0, hjust = 0.5, vjust = 0.5, face = "bold", margin = margin(b = 20)),
                    axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain", margin = margin(t = 5)),
                    axis.text.y = element_text(color = "grey20", angle = 0, hjust = 0.5, vjust = 0.5, face = "plain", margin = margin(r = 5)),  
                    axis.title.x = element_text(color = "grey20", angle = 0, hjust = 0.5, vjust = 0.5, face = "plain", margin = margin(t = 10)),
                    axis.title.y = element_text(color = "grey20", angle = 90, hjust = 0.5, vjust = 0.5, face = "plain", margin = margin(r = 10)),
                    axis.ticks = element_blank(),
                    legend.position = "bottom", legend.title = element_text(face = "bold"), legend.text = element_text(size = 15), #legend.direction = "vertical", legend.box = "horizontal", #legend.key.size = unit(1, "cm"),
                    panel.background = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_line(size=0.5, color = "lightgrey"),
                    panel.grid.minor.y = element_blank(),
                    panel.border = element_blank(),
                    axis.line.x = element_line(size=0.5, color = "grey")
                   )


# 02. LECTURA Y PREPARACIÓN DE DATOS ###########################################

# Lectura de archivo histórico de precios 
ArchivoCargue <- "Data ETF ECH.xlsx"
BDPI <- read_excel(ArchivoCargue, sheet = "OHLC")

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


# 03. CODIFICACIÓN DE FRACTALES INTRADIARIOS ##################################

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


# 04. CÁLCULO SEÑALES DE FRACTALES INTRADIARIOS ################################

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
FunSenFIX <- paste0("BDPI$", ParamFracI$Fractal,
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
eval(parse(text = FunSenFIX))

# Señal para cada fractal intradiario cuando no se cumplen las condiciones de "BUY" o "SELL".
BDPI <- fill(data = BDPI, ParamFracI$Fractal, .direction = "down")


# 05. ELIMINACIÓN FH DESCARTADAS ##############################################

BDPI$DATE <- NA
BDPI$DATE <- make_datetime(year = year(BDPI$DATEFRAME), 
                           month = month(BDPI$DATEFRAME), 
                           day = day(BDPI$DATEFRAME), 
                          )

# Número de FH descartadas para negociar antes del cierre
NFHC <- as.numeric(read_excel(ArchivoCargue, 
                              sheet = "N_FH_Cierre_Descartadas",
                              col_names = FALSE
                              )
                  )

if (NFHC == 0) {
  
  # No se elimina nada.
  
} else {
  
  # FH del cierre de cada día seleccionadas para su eliminación
  BD_Descartada <- BDPI %>% group_by(DATE) %>% slice_max(DATEFRAME, n = NFHC)
  
  # Eliminación de FH seleccionadas
  ID_FH_Descartadas <- match(BD_Descartada$DATEFRAME, BDPI$DATEFRAME)
  BDPI <- BDPI[-ID_FH_Descartadas,]
  
}

N <- length(BDPI$DATEFRAME)


# 06. VELA DIARIA #############################################################

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


# 07. CODIFICACIÓN DE FRACTALES DIARIOS (INCLUYE FIRX) #########################

# Variables de codificación de fractales diarios
ParamFracD <- read_excel(ArchivoFractales, sheet = "Parametros fractales")

# Insumos parametrización FIR
ParamFracD <- ParamFracD[-(NFrac+1),] # Eliminación de parámetros del fractal intradiario de referencia
ParamFracD[(NFrac+1):(NFrac*2),] <- ParamFracD[(1:NFrac),] # Réplica de parámetros de FIR

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
LetraFIR <- "R" # Letra para identificación de fractales intradiarios de referencia
ParamFracD$Estrategia[(NFrac+1):(NFrac*2)] <- paste0(LetraFIR, (1:NFrac)) # Asignación de nombres de estrategias de referencia
ParamFracR <- ParamFracD[match(paste0(LetraFIR,(1:NFrac)),ParamFracD$Estrategia),]
ParamFracR$Fractal <- paste0("FI", ParamFracR$Estrategia) # Asignación de de nombres para FIRX
ParamFracD <- ParamFracD[-match(paste0(LetraFIR,(1:NFrac)),ParamFracD$Estrategia),] # Eliminación parámetros creados para FIRX en ParamFracD.
ParamFracR$VariableCompra <- paste0("BDPI$",ParamFracD$`Variable compra`)
ParamFracR$VariableVenta <- paste0("BDPI$",ParamFracD$`Variable venta`)


# 08. CÁLCULO SEÑALES DE FRACTALES DIARIOS #####################################

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
FunDesfaseVenta <- paste0("BDPD$", ParamFracD$NombreRefVenta,
                          " <- shift(x = ", "BDPD$", ParamFracD$NombreRefVenta, 
                          ", n = ", ParamFracD$Desfase, ", fill = NA)"
                         ) 
eval(parse(text = FunDesfaseVenta))

# Señal para condiciones de "BUY" o "SELL" en cada fractal diario. Ejemplo FD1:
# BDPD$FD1 <- ifelse (BDPD$CLOSE > BDPD$FD1_B_max_HIGH_4_1, "BUY", ifelse(BDPD$CLOSE < BDPD$FD1_S_min_LOW_4_1, "SELL", NA))
FunSenFDX <- paste0("BDPD$", ParamFracD$Fractal,
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
eval(parse(text = FunSenFDX))

# Señal para cada fractal diario cuando no se cumplen las condiciones de "BUY" o "SELL".
BDPD <- fill(data = BDPD, ParamFracD$Fractal, .direction = "down")


# 09. CÁLCULO FUNCIONES SOBRE REFERENCIAS DE FRACTALES DIARIOS DE REFERENCIA ####

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# diario de referencia de compra.Ejemplo FDR1:
# BDPD$FDR1_B_max_HIGH_4_1 <- rollapplyr(data = BDPD$HIGH, width = 4, FUN = max, fill = NA)
FunFracCompra <- paste0("BDPD$", ParamFracR$NombreRefCompra, 
                        " <- rollapplyr(data = ", ParamFracR$RefCompra, 
                        ", width = ", ParamFracR$VentanaMovil,
                        ", FUN = ", ParamFracR$`Función compra`, ", fill = NA)"
                       )
eval(parse(text = FunFracCompra))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# diario de referencia de compra.Ejemplo FDR1:
# BDPD$FDR1_B_max_HIGH_4_1 <- shift(x = BDPD$FD1_B_max_HIGH_4_1, n = 1, fill = NA)
FunDesfaseCompra <- paste0("BDPD$", ParamFracR$NombreRefCompra,
                           " <- shift(x = ", "BDPD$", ParamFracR$NombreRefCompra, 
                           ", n = ", ParamFracR$Desfase, ", fill = NA)"
                          )
eval(parse(text = FunDesfaseCompra))

# Cálculo de la función sobre la referencia correspondiente a cada fractal
# diario de referencia de venta. Ejemplo FDR1:
# BDPD$FDR1_S_min_LOW_4_1 <- rollapplyr(data = BDPD$LOW, width = 4, FUN = min, fill = NA)
FunFracVenta <- paste0("BDPD$", ParamFracR$NombreRefVenta, 
                       " <- rollapplyr(data = ", ParamFracR$RefVenta, 
                       ", width = ", ParamFracR$VentanaMovil,
                       ", FUN = ", ParamFracR$`Función venta`, ", fill = NA)"
                      )
eval(parse(text = FunFracVenta))

# Aplicación del desfase de la ventana móvil correspondiente a cada fractal
# diario de referencia de venta. Ejemplo FDR1:
# BDPD$FDR1_S_min_LOW_4_1 <- shift(x = BDPD$FDR1_S_min_LOW_4_1, n = 1, fill = NA)
FunDesfaseVenta <-paste0("BDPD$", ParamFracR$NombreRefVenta,
                         " <- shift(x = ", "BDPD$", ParamFracR$NombreRefVenta, 
                         ", n = ", ParamFracR$Desfase, ", fill = NA)"
                        ) 
eval(parse(text = FunDesfaseVenta))


# 10. CÁLCULO SEÑALES DE FRACTALES INTRADIARIOS DE REFERENCIA ##################

# Importe a la base de datos intradiaria de la función sobre la referencia 
# correspondiente a cada fractal diario de referencia de venta. Ejemplo FDR1: 
# BDPI$FDR1_B_max_HIGH_4_1 <- BDPD$FDR1_B_max_HIGH_4_1[match(as.Date(BDPI$DATE), BDPD$DATE)]
FunImporteFDRCompra <- paste0("BDPI$", ParamFracR$NombreRefCompra,
                              " <- BDPD$", ParamFracR$NombreRefCompra,
                              "[match(as.Date(BDPI$DATE), BDPD$DATE)]"
                             )
eval(parse(text = FunImporteFDRCompra))

# Importe a la base de datos intradiaria de la función sobre la referencia 
# correspondiente a cada fractal diario de referencia de compra. Ejemplo FDR1: 
# BDPI$FDR1_S_min_LOW_4_1 <- BDPD$FDR1_S_min_LOW_4_1[match(as.Date(BDPI$DATE), BDPD$DATE)]
FunImporteFDRVenta <- paste0("BDPI$", ParamFracR$NombreRefVenta,
                             " <- BDPD$", ParamFracR$NombreRefVenta,
                             "[match(as.Date(BDPI$DATE), BDPD$DATE)]"
                            )
eval(parse(text = FunImporteFDRVenta))

# Señal para condiciones de "BUY" o "SELL" en cada fractal intradiario de
# referencia. Ejemplo FIR1:
# BDPI$FIR1 <- ifelse (BDPI$CLOSE > BDPI$FDR1_B_max_HIGH_4_1, "BUY", ifelse(BDPI$CLOSE < BDPI$FDR1_S_min_LOW_4_1, "SELL", NA))
FunSenFIRX_BS <- paste0("BDPI$", ParamFracR$Fractal,
                     " <- ifelse(",
                                 ParamFracR$VariableCompra, ParamFracR$`Criterio compra`, "BDPI$", ParamFracR$NombreRefCompra, 
                                 ", 'BUY', ",
                                 "ifelse(",
                                         ParamFracR$VariableVenta, ParamFracR$`Criterio venta`, "BDPI$", ParamFracR$NombreRefVenta, 
                                         ", 'SELL', ",
                                         "NA",
                                       ")",
                               ")"
                    )
eval(parse(text = FunSenFIRX_BS))

# Aplicación de un desfase a cada fractal diario para su asignación a FIRX
# cuando no se cumplen las condiciones de "BUY" o "SELL". Ejemplo FD1_NoBS:
# BDPD$FD1_NoBS <- shift(x = BDPD$FD1, n = DesfaseNoBS, fill = NA)
DesfaseNoBS <- 1
FunSenFDX_NoBS <- paste0("BDPD$", ParamFracD$Fractal, "_NoBS",
                         " <- shift(",
                                    "x = ", "BDPD$", ParamFracD$Fractal,
                                    ", ",
                                    "n = ", DesfaseNoBS,
                                    ", ",
                                    "fill = NA",
                                  ")"
                        ) 
eval(parse(text = FunSenFDX_NoBS))

# Señal para cada fractal intradiario de referencia cuando no se cumplen las
# condiciones de "BUY" o "SELL". 

# Importe de señal diaria empleada cuando no se cumplen las condiciones de
# "BUY" o "SELL", desde base de datos diaria a base de datos intradiaria.
# Ejemplo importe FD1_NoBS:
# BDPI$FD1_NoBS <- BDPD$FD1_NoBS[match(BDPI$DATE, BDPD$DATE)]
FunSenFDX_NoBS_I <- paste0("BDPI$", ParamFracD$Fractal, "_NoBS",
                           " <- BDPD$", ParamFracD$Fractal, "_NoBS",
                           "[match(as.Date(BDPI$DATE), BDPD$DATE)]"
                          )
eval(parse(text = FunSenFDX_NoBS_I))
# Ejemplo asignación a FIR1 de señal diaria empleada cuando no se cumplen las
# condiciones de "BUY" o "SELL":
# BDPI$FIR1 <- ifelse(is.na(BDPI$FIR1), BDPI$FD1_NoBS, BDPI$FIR1)
FunSenFIRX <- paste0("BDPI$", ParamFracR$Fractal,
                     " <- ifelse(",
                                 "is.na(", "BDPI$", ParamFracR$Fractal, ")",
                                 ", ",
                                 "BDPI$", ParamFracD$Fractal, "_NoBS",
                                 ", ",
                                 "BDPI$", ParamFracR$Fractal,
                               ")"
                    )
eval(parse(text = FunSenFIRX))


# 11. CÁLCULO SEÑAL INTRADIARIA FINAL (SIF) ###################################

# Listado de señales y nombres de sus posibles combinaciones
Senales <- data.frame(NombreBD = rep(NA, NFrac^2),
                      I = rep(1:NFrac,rep(NFrac, NFrac)),
                      R = rep(1:NFrac, NFrac),
                      FI = paste0("FI",rep(1:NFrac,rep(NFrac, NFrac))),
                      FIR = paste0("FIR", rep(1:NFrac, NFrac))
                     )
Senales$NombreBD <- paste0("BDP_I", Senales$I, "_R", Senales$R)

# Creación de base de datos para cada combinación de señales posible
FunBDPSList <- paste0(Senales$NombreBD,
                      " <- BDPI[",
                                "match(",
                                       "c('DATE',", 
                                          "'DATEFRAME',", 
                                          "'VOLUME',", 
                                          "'OPEN',", 
                                          "'HIGH',", 
                                          "'LOW',", 
                                          "'CLOSE',",
                                          "'", Senales$FI, "' ," ,
                                          "'",  Senales$FIR, "'",
                                        ")",
                                       ",",
                                       " colnames(BDPI)",
                                     ")",
                              "]"
                     )
eval(parse(text = FunBDPSList))
BDPSList <- lapply(Senales$NombreBD, get)
names(BDPSList) <- Senales$NombreBD
rm(list = Senales$NombreBD)

# Cálculo de señal Intradiaria Final (SIF) 

FunSIF <- function(BD) {
  colnames(BD)[match("FI", substr(colnames(BD),1,2))] <- "FI"
  colnames(BD)[match("FIR", substr(colnames(BD),1,3))] <- "FIR"
  BD$SIF <- ifelse(BD$FI == BD$FIR,BD$FI,"NONE")
  BD$SIF[is.na(BD$SIF)] <- "NONE"
  return(BD)
}
BDPSList <- lapply(BDPSList, FunSIF)


# 12. DECISIÓN DE INVERSIÓN ####

# Determinación de la decisión de inversión con base en los cambios en SIF
FunDecision <- function(BD) {
  BD$DECISION <- ifelse((BD$SIF == shift(BD$SIF, n=1, fill = NA)),
                        ifelse((BD$SIF == "BUY" & shift(BD$SIF, n=1, fill = NA) == "BUY") | (BD$SIF == "SELL" & shift(BD$SIF, n=1, fill = NA) == "SELL"),
                               "HOLD POSITION",
                               "NO POSITION"
                               ),
                        ifelse((shift(BD$SIF, n=1, fill = NA) == "NONE"),
                               "OPEN",
                               ifelse((BD$SIF == "NONE"),
                                      "CLOSE",
                                      "CLOSE-OPEN"
                                      )
                               )
                        )
  return(BD)
}
BDPSList <- lapply(BDPSList, FunDecision)


# 13. CÁLCULO ESTADÍSTICAS RETORNO Y RIESGO ###################################

Fun_Est_U_MPA <- function(BD) {
  
  # Asignación de precio de apertura (PA)
  BD$PA <- ifelse((BD$DECISION == "OPEN"),
                  BD$CLOSE,
                  ifelse((BD$DECISION == "CLOSE-OPEN"),
                         BD$CLOSE,
                         NA
                        )
                  )
  BD$PA <- na.locf(BD$PA, na.rm = FALSE) # Arrastre donde no es OPEN o CLOSE-OPEN
  ID_CLOSEOPEN <- which(BD$DECISION=="CLOSE-OPEN")
  BD$PA[ID_CLOSEOPEN] <- shift(BD$PA, n=1, fill=NA)[ID_CLOSEOPEN] # Para CLOSE-OPEN se deja el PA de la posición cerrada.
  BD$PA[which(BD$DECISION == "NO POSITION")] <- NA # No aplica si no hay posición
  
  # Asignación de precio de cierre (PC)
  BD$PC <- ifelse((BD$DECISION == "CLOSE"),
                  BD$CLOSE,
                  ifelse((BD$DECISION == "CLOSE-OPEN"),
                         BD$CLOSE,
                         NA
                         )
                  )
  
  # Cálculo de utilidad de cada negociación suponiendo solo posiciones largas
  BD$UTILIDAD <- BD$PC - BD$PA
  
  # Asignación de signo según señal para identificar posiciones largas y cortas
  BD$SENALSIGNO <- ifelse((BD$DECISION == "OPEN") | (BD$DECISION == "HOLD POSITION"),
                          ifelse((BD$SIF == "BUY"),
                                 1,
                                 -1
                                ),
                          ifelse((BD$DECISION == "CLOSE") | (BD$DECISION == "CLOSE-OPEN"),
                                 ifelse((shift(BD$SIF, n=1, fill=NA) == "BUY"),
                                        1,
                                        -1
                                        ),
                                 NA # Cuando BD$DECISION == "NO POSITION"
                                )
                          ) 
  
  # Cálculo de utilidad de cada negociación según posiciones (largas o cortas)
  BD$UTILIDAD <- BD$UTILIDAD * BD$SENALSIGNO
  
  # Cálculo de utilidad acumulada
  BD$UTILIDADACUM <- BD$UTILIDAD
  BD$UTILIDADACUM[is.na(BD$UTILIDAD)] <- 0
  BD$UTILIDADACUM <- cumsum(BD$UTILIDADACUM)
  
  # Cálculo pérdida acumulada
  BD$PERDACUM <- BD$UTILIDADACUM - cummax(BD$UTILIDADACUM)
  BD$MAXPERDACUM <- cummin(BD$PERDACUM)
  
  # Estadisticas finales utilidad y máxima pérdida acumulada
  N <- length(BD$DATEFRAME)
  UTILIDADACUM <- BD$UTILIDADACUM[N]
  MAXPERDACUM <- BD$MAXPERDACUM[N]
  UA_MPA <- UTILIDADACUM / -MAXPERDACUM
  
  # Gráfico utilidad acumulada
  G_UTILIDADACUM <- ggplot(BD, aes(x = DATE, y = UTILIDADACUM)) +
                      geom_line() +
                      ggtitle("Utilidad acumulada estrategia") + 
                      xlab("Fecha") + ylab("Utilidad acumulada") +
                      expand_limits(x =BD$DATE[1]) +
                      expand_limits(y = 0) +
                      PlantillaG
  
  #Resultados
  BD_Est_U_MPA <- list(BD, UTILIDADACUM, MAXPERDACUM, UA_MPA, G_UTILIDADACUM)
  names(BD_Est_U_MPA) <- c("BDPS", "UtilidadAcum", "MaxPerdAcum", "UA_MPA", "Graf_UtilidadAcum")
  
  return(BD_Est_U_MPA)
  
}

BDPSList <- lapply(BDPSList, Fun_Est_U_MPA)


# 14. RESUMEN ESTADÍSTICAS ESTRATEGIAS ########################################

# Función para obtención de estadísticas retorno y riesgo de cada estrategia
Fun_UA_MPA <- function(List) {
  UA_MPA <- data.frame(UtilidadAcum = List$UtilidadAcum, 
                       MaxPerdAcum = List$MaxPerdAcum, 
                       UA_MPA = List$UA_MPA
                      )
  return(UA_MPA)
}

# Obtención de estadísticas retorno y riesgo de cada estrategia
Senales <- cbind(Senales, t(sapply(BDPSList, Fun_UA_MPA)))
Senales$UtilidadAcum <- unlist(Senales$UtilidadAcum)
Senales$MaxPerdAcum <- unlist(Senales$MaxPerdAcum)
Senales$UA_MPA <- unlist(Senales$UA_MPA)
rownames(Senales) <- paste0("I", Senales$I, "R", Senales$R)

# Gráfico Utilidad/MDD por estrategia
ggplot(Senales, aes(x = rownames(Senales), y = UA_MPA)) +
  geom_col() +
  ggtitle("Utilidad/MDD por estrategia") + 
  xlab("Estrategia") + ylab("Utilidad/MDD") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG

# Gráfico Utilidad/MDD para estrategias por encima del promedio
Est_may_media <-  Senales %>% filter(UA_MPA > mean(UA_MPA)) 
ggplot(Est_may_media, aes(x = reorder(rownames(Est_may_media), UA_MPA), y = UA_MPA)) +
  geom_col() +
  ggtitle("Utilidad/MDD por estrategia") + 
  xlab("Estrategia") + ylab("Utilidad/MDD") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG

U_MDD_Objetivo <- as.numeric(read_excel(ArchivoCargue, 
                                        sheet = "U_MDD_Objetivo",
                                        col_names = FALSE
                                        )
                            )
Est_may_Sup <-  Senales %>% filter(UA_MPA > U_MDD_Objetivo)

ggplot(Est_may_Sup, aes(x = reorder(rownames(Est_may_Sup), UA_MPA), y = UA_MPA)) +
  geom_col() +
  ggtitle("Utilidad/MDD por estrategia") + 
  xlab("Estrategia") + ylab("Utilidad/MDD") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG


# 15. ESTRATEGIA ÓPTIMA #######################################################

NEstrategiaOpt <- which.max(Senales$UA_MPA)
EstrategiaOpt <- rownames(Senales)[NEstrategiaOpt]
Nombre_BD_EstrategiaOpt <- Senales$NombreBD[NEstrategiaOpt]
paste0("La estrategia óptima es ", EstrategiaOpt)
View(BDPSList[[Nombre_BD_EstrategiaOpt]]$BDPS)
BDPSList[[Nombre_BD_EstrategiaOpt]]$Graf_UtilidadAcum +
  ggtitle(paste0("Utilidad acumulada estrategia ", EstrategiaOpt))

# 16. ESTRATEGIAS ADICIONALES #################################################

# Para visualizar la información y estadísticas de una estrategia en particular,
# a continuación asigne a la variable "Estrategia" la cambinación deseada de 
# fractales intradiario y de referencia con el formato "I#R#". Por ejemplo, para
# el fractal intradiario 2 y el fractal intradiario de referencia 8 use "I2R8":

Estrategia <- "I5R9"
NEstrategia <- which(rownames(Senales) == Estrategia)
Nombre_BD_Estrategia <- Senales$NombreBD[NEstrategia]
paste0("La estrategia seleccionada es ", Estrategia)
View(BDPSList[[Nombre_BD_Estrategia]]$BDPS)
BDPSList[[Nombre_BD_Estrategia]]$Graf_UtilidadAcum +
  ggtitle(paste0("Utilidad acumulada estrategia ", Estrategia))

