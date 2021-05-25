###  Estrategia y estadísticas ETF ECH (Réplica Swell vs Propuesto)  ###
###                            2021-05-25                            ###
###                           Version 0.3                            ###  
###                Authors: Olga Serna / Ivan Serrano                ###


# 00. DESCRIPCIÓN E INSTRUCCIONES #############################################

# 0.1 Objetivo

# Calcular la señal de la estrategia (óptima o deseada) y sus estadísticas de 
# retorno y riesgo a partir de serie histórica OHLC (apertura, máximo, mínimo y
# cierre) y un árbol de decisión de estrategias. El algoritmo de decisiones toma
# como base las señales calculadas bajo un principio de monto de inversión
# constante con apalancamiento en caso de requerirse. 

# 0.2 Supuestos

# 0.2.1 Se realizan operaciones a las que haya lugar después de formación de la
#       vela de mercado para cada franja horaria (FH), teniendo en cuenta el 
#       periodo de tiempo permitido de operación definido para cada día, el cual
#       excluye las franjas horarias que se consideran de baja liquidez.

# 0.2.2 Todas las franjas horarias se tienen en cuenta para el cálculo de la
#       señal intradiaria pero no para el cálculo de la vela diaria y por lo tanto 
#       de los fractales intradiarios de referencia.

# 0.2.4 Para los cálculos de valoración, siempre se asumen posiciones largas.

# 0.2.5 Para los cálculos de retornos, se tiene en cuenta si la posición 
#       realmente es larga o corta.

# 0.2.6 Se consigue operar en el mercado a los precios de cierre de la franja,
#       con costos transaccionales asociados a un bid-ask spread nulo.

# 0.2.7 El retorno objetivo empleado para el cálculo de la razón de Sortino
#       corresponde al de la tasa libre de riesgo (US Treasury 1-3y).

# 0.3 Instrucciones

# 0.3.1 Guardar con el nombre "Data NOMBREACTIVO.xlsx" el archivo que contiene
#       los insumos en la misma ruta donde se encuentra ubicado el código 
#       "Estrategia y estadísticas NOMBREACTIVO (propuesto).R". Estos
#       insumos incluyen:
#       - Hoja "OHLC Activo": histórico de precios OHLC del activo.
#       - Hoja "OHLC libre de riesgo": histórico de precios OHLC del activo
#                                      empleado para calcular el retorno objetivo
#                                      en el cálculo de la razón de Sharpe 
#                                      (y como retorno objetivo en la razón de Sortino).
#       - Hoja "N_FH_por_dia": número de FH de negociación a considerar en cada día.
#       - Hoja "N_FH_Cierre_Descartadas": número de FH descartadas para negociar
#                                         antes del cierre.
#       - Hoja "U_MDD_Objetivo": valor objetivo de la razón Utilidad/MDD empleado
#                                para visualizar solo la información de las 
#                                estrategias que cumplan con esta condición.
#       - Hoja "RA_MDD_Objetivo": valor objetivo de la razón RetornoAnual/MDD 
#                                 empleado para visualizar solo la información 
#                                 de las estrategias que cumplan con esta condición.
#       - Hoja "VentanaMovilVol": periodo de tiempo (en días) a considerar para
#                                 el cálculo de las volatilidades.
#       - Hoja "Significancia": nivel de siginificancia deseado para los cálculos
#                               de VaR y CVaR.
#       El nombre del archivo de Excel debe asignarse a la variable "ArchivoCargue",
#       modificando la primera línea de código en la sección "02. LECTURA Y 
#       PREPARACIÓN DE DATOS". Se debe verificar que las hojas solo contengan la
#       información mencionada y nada adicional (incluso celdas borradas).

# 0.3.2 Guardar en la misma ruta y con el nombre con el nombre "Fractales.xlsx",
#       el archivo que contiene las reglas y variables que se emplean para las
#       condiciones de compra y venta de cada una de las estrategias diarias e 
#       intradiarias, así comode la estrategia intradiaria de referencia, que 
#       debe quedar en el última fila del archivo. Se debe verificar que el 
#       archivo solo contenga la información mencionada y nada adicional 
#       (incluso celdas borradas).

# 0.3.3 La última sección del código permite visualizar la información y 
#       estadísticas de una estrategia en particular.

# 01. PAQUETES Y CONFIGURACIONES ##############################################

# Paquetes
Libraries <- c("readxl",      # read_excel
               "openxlsx",    # createWorkbook, addWorksheet, writeData, saveWorkbook
               "rstudioapi",  # getActiveDocumentContext
               "lubridate",   # makedatetime, year, month,.., second
               "ggplot2",     # ggplot  
               "data.table",  # Data manipulation (aggregation with . operator)
               "zoo",         # rollapplyr, rollapply
               "tidyr",       # fill 
               "dplyr",       # group_by, select, mutate, slice_max 
               "na.tools"     # na.replace
              ) 

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
BaseDirPath <- dirname(getActiveDocumentContext()$path)
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
                    legend.position = "bottom", legend.title = element_text(face = "bold"), legend.text = element_text(size = 10), #legend.direction = "vertical", legend.box = "horizontal", #legend.key.size = unit(1, "cm"),
                    panel.background = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_line(size=0.5, color = "lightgrey"),
                    panel.grid.minor.y = element_blank(),
                    panel.border = element_blank(),
                    axis.line.x = element_line(size=0.5, color = "grey")
                   )


# 02. LECTURA Y PREPARACIÓN DE DATOS ##########################################

# Archivos de insumo
ArchivoCargue <- "Data NOMBREACTIVO.xlsx"
ArchivoFractales <- "Fractales.xlsx"
ArchivoInsumos <- "Insumos.xlsx"

# Lectura de archivo histórico de precios 
BDPI <- read_excel(ArchivoCargue, sheet = "OHLC Activo")

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


# 04. CÁLCULO SEÑALES DE FRACTALES INTRADIARIOS ###############################

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
NFHC <- as.numeric(read_excel(ArchivoInsumos, 
                              sheet = "Insumos",
                              range = "B3",
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


# 07. CODIFICACIÓN DE FRACTALES DIARIOS (INCLUYE FIRX) ########################

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


# 08. CÁLCULO SEÑALES DE FRACTALES DIARIOS ####################################

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


# 09. CÁLCULO SEÑALES DE FRACTALES INTRADIARIOS DE REFERENCIA #################

# Importe a la base de datos intradiaria de la función sobre la referencia 
# correspondiente a cada fractal diario de referencia de venta. Ejemplo FD1: 
# BDPI$FD1_B_max_HIGH_4_1 <- BDPD$FD1_B_max_HIGH_4_1[match(as.Date(BDPI$DATE), BDPD$DATE)]
FunImporteFDCompra <- paste0("BDPI$", ParamFracR$NombreRefCompra,
                             " <- BDPD$", ParamFracR$NombreRefCompra,
                             "[match(as.Date(BDPI$DATE), BDPD$DATE)]"
)
eval(parse(text = FunImporteFDCompra))

# Importe a la base de datos intradiaria de la función sobre la referencia 
# correspondiente a cada fractal diario de referencia de compra. Ejemplo FD1: 
# BDPI$FD1_S_min_LOW_4_1 <- BDPD$FD1_S_min_LOW_4_1[match(as.Date(BDPI$DATE), BDPD$DATE)]
FunImporteFDVenta <- paste0("BDPI$", ParamFracR$NombreRefVenta,
                            " <- BDPD$", ParamFracR$NombreRefVenta,
                            "[match(as.Date(BDPI$DATE), BDPD$DATE)]"
)
eval(parse(text = FunImporteFDVenta))

# Señal para condiciones de "BUY" o "SELL" en cada fractal intradiario de
# referencia. Ejemplo FIR1:
# BDPI$FIR1 <- ifelse (BDPI$CLOSE > BDPI$FD1_B_max_HIGH_4_1, "BUY", ifelse(BDPI$CLOSE < BDPI$FD1_S_min_LOW_4_1, "SELL", NA))
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


# 10. CÁLCULO SEÑAL INTRADIARIA FINAL (SIF) ###################################

# Listado de señales y nombres de sus posibles combinaciones
Senales <- data.frame(NombreBD = rep(NA, NFrac^2),
                      I = rep(1:NFrac,rep(NFrac, NFrac)),
                      R = rep(1:NFrac, NFrac),
                      FI = paste0("FI",rep(1:NFrac,rep(NFrac, NFrac))),
                      FIR = paste0("FIR", rep(1:NFrac, NFrac))
)
Senales$NombreBD <- paste0("BDP_I", Senales$I, "_R", Senales$R)
rownames(Senales) <- paste0("I", Senales$I, "R", Senales$R)

# Creación de base de datos para cada combinación de señales posible
FunBDList <- paste0(Senales$NombreBD,
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
eval(parse(text = FunBDList))
BDList <- lapply(Senales$NombreBD, get)
names(BDList) <- Senales$NombreBD
rm(list = Senales$NombreBD)

# Cálculo de señal Intradiaria Final (SIF) 

FunSIF <- function(BD) {
  colnames(BD)[match("FI", substr(colnames(BD),1,2))] <- "FI"
  colnames(BD)[match("FIR", substr(colnames(BD),1,3))] <- "FIR"
  BD$SIF <- ifelse(BD$FI == BD$FIR,BD$FI,"NONE")
  BD$SIF[is.na(BD$SIF)] <- "NONE"
  return(BD)
}
BDList <- lapply(BDList, FunSIF)


# 11. DECISIÓN DE INVERSIÓN ÚNICAMENTE CON BASE EN SIF ####

# Determinación de la decisión de inversión con base en los cambios en SIF

FunDecision_SIF <- function(BD) {
  BD$DECISION_SIF <- ifelse((BD$SIF == shift(BD$SIF, n=1, fill = NA)),
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

BDList <- lapply(BDList, FunDecision_SIF)


# 12. DECISIÓN DE INVERSIÓN FINAL CONSIDERANDO SL Y TP ########################

# Cargue de zonas de pérdidas y ganancias (P&G) para el cierre de posiciones

Zona_PG_Cierre <- read_excel(ArchivoInsumos, 
                             sheet = "Zona_PG_Cierre",
                             range = "A1:D5",
                             col_names = TRUE
                             )
Zona_PG_Cierre$MaximoRazon[1] <- Inf
Zona_PG_Cierre$MinimoRazon[length(Zona_PG_Cierre$MinimoRazon)] <- -Inf

# Asignación de PENTRADA, PCIERRE y signos de cortos/largos únicamente con base
# en SIF

Fun_PE_PC_SENALSIGNO <- function(BD) {
  
  # Asignación de precio de entrada (PENTRADA)
  BD$PENTRADA <- ifelse((BD$DECISION_SIF == "OPEN"),
                        BD$CLOSE,
                        ifelse((BD$DECISION_SIF == "CLOSE-OPEN"),
                               BD$CLOSE,
                               NA
                               )
                        )
  
  BD$PENTRADA <- na.locf(BD$PENTRADA, na.rm = FALSE) # Arrastre donde no es OPEN o CLOSE-OPEN
  ID_CLOSEOPEN <- which(BD$DECISION_SIF=="CLOSE-OPEN")
  BD$PENTRADA[ID_CLOSEOPEN] <- shift(BD$PENTRADA, n=1, fill=NA)[ID_CLOSEOPEN] # Para CLOSE-OPEN se deja el PA de la posición cerrada.
  BD$PENTRADA[which(BD$DECISION_SIF == "NO POSITION")] <- NA # No aplica si no hay posición
  
  # Asignación de precio de cierre (PCIERRE)
  BD$PCIERRE <- ifelse((BD$DECISION_SIF == "CLOSE"),
                       BD$CLOSE,
                       ifelse((BD$DECISION_SIF == "CLOSE-OPEN"),
                              BD$CLOSE,
                              NA
                              )
                       )
  
  # Asignación de signo según señal para identificar posiciones largas y cortas
  BD$SENALSIGNO <- ifelse((BD$DECISION_SIF == "OPEN") | (BD$DECISION_SIF == "HOLD POSITION"),
                          ifelse((BD$SIF == "BUY"),
                                 1,
                                 -1
                                 ),
                          ifelse((BD$DECISION_SIF == "CLOSE") | (BD$DECISION_SIF == "CLOSE-OPEN"),
                                 ifelse((shift(BD$SIF, n=1, fill=NA) == "BUY"),
                                        1,
                                        -1
                                        ),
                                 0 # Cuando DECISION es "NO POSITION" o NA
                                 )
                          )
  
  BD$SENALSIGNO <- ifelse(is.na(BD$DECISION_SIF), 0, BD$SENALSIGNO)

  #Resultados
  return(BD)
  
}

BDList <- lapply(BDList, Fun_PE_PC_SENALSIGNO) # Construye lista de BDs

# Determinación de decisión de inversión final incorporando TP y SL

FunDecision_Final <- function(BD) {
  
  # Cálculo de razones para SL y TP
  BD$PA_PE <- ifelse(BD$DECISION_SIF == "HOLD POSITION",
                     (BD$OPEN / BD$PENTRADA - 1) * BD$SENALSIGNO,
                     NA
                     )
  BD$PmaxPmin_PE <- ifelse(BD$DECISION_SIF == "HOLD POSITION",
                           ifelse(BD$SIF == "BUY",
                                  (BD$LOW / BD$PENTRADA - 1) * BD$SENALSIGNO,
                                  (BD$HIGH / BD$PENTRADA - 1) * BD$SENALSIGNO
                                  ),
                           NA
                           )
  
  # Cálculo de umbrales de la razón PA/PE para las zonas de ganancia/pérdida
  SL_FIJO_PA <- Zona_PG_Cierre$MaximoRazon[Zona_PG_Cierre$Zona_PG == "Stop Loss"]
  BD$SL_MOVIL_PA <- shift(ifelse(BD$DECISION_SIF == "HOLD POSITION",
                                 ((1 + BD$PA_PE) * (1 + SL_FIJO_PA) - 1),
                                 ifelse(BD$DECISION_SIF == "OPEN" | BD$DECISION_SIF == "CLOSE-OPEN",
                                        SL_FIJO_PA,
                                        NA
                                        )
                                 ),
                          n=1, fill=NA
                          )
  BD$SL_MOVIL_PA <- ifelse(BD$DECISION_SIF == "CLOSE" | BD$DECISION_SIF == "CLOSE-OPEN",
                           NA, 
                           BD$SL_MOVIL_PA
                           )
  BD$SL_MOVIL_PA <- ave(BD$SL_MOVIL_PA, 
                        cumsum(is.na(BD$SL_MOVIL_PA) != c(T,is.na(BD$SL_MOVIL_PA)[1:N-1])), # Vector de grupos (se construye haciendo el cumsum de un veCtor con VERDADERO en puntos de inicio)
                        FUN = cummax
                        )
  TP_FIJO_PA <- Zona_PG_Cierre$MinimoRazon[Zona_PG_Cierre$Zona_PG == "Take Profit"]
  BD$TP_MOVIL_PA <- shift(ifelse(BD$DECISION_SIF == "HOLD POSITION",
                                 ((1 + BD$PA_PE) * (1 + TP_FIJO_PA) - 1),
                                 ifelse(BD$DECISION_SIF == "OPEN" | BD$DECISION_SIF == "CLOSE-OPEN",
                                        TP_FIJO_PA,
                                        NA
                                        )
                                 ),
                          n=1, fill=NA
                          )
  BD$TP_MOVIL_PA <- ifelse(BD$DECISION_SIF == "CLOSE" | BD$DECISION_SIF == "CLOSE-OPEN", 
                           NA, 
                           BD$TP_MOVIL_PA
                           )
  BD$TP_MOVIL_PA <- ave(BD$TP_MOVIL_PA, 
                        cumsum(is.na(BD$TP_MOVIL_PA) != c(T,is.na(BD$TP_MOVIL_PA)[1:N-1])), # Vector de grupos (se construye haciendo el cumsum de un veCtor con VERDADERO en puntos de inicio)
                        FUN = cummax
                        )

  # Cálculo de umbrales de la razón Pmax/PE o Pmin/PE para las zonas de ganancia/pérdida
  SL_FIJO_PmaxPmin <- Zona_PG_Cierre$MaximoRazon[Zona_PG_Cierre$Zona_PG == "Stop Loss"]
  BD$SL_MOVIL_PmaxPmin <- shift(ifelse(BD$DECISION_SIF == "HOLD POSITION",
                                       ((1 + BD$PmaxPmin_PE) * (1 + SL_FIJO_PmaxPmin) - 1),
                                       ifelse(BD$DECISION_SIF == "OPEN" | BD$DECISION_SIF == "CLOSE-OPEN",
                                              SL_FIJO_PmaxPmin,
                                              NA
                                              )
                                       ),
                                n=1, fill=NA
                                )
  BD$SL_MOVIL_PmaxPmin <- ifelse(BD$DECISION_SIF == "CLOSE" | BD$DECISION_SIF == "CLOSE-OPEN", 
                                 NA, 
                                 BD$SL_MOVIL_PmaxPmin
                                 )
  BD$SL_MOVIL_PmaxPmin <- ave(BD$SL_MOVIL_PmaxPmin,
                              cumsum(is.na(BD$SL_MOVIL_PmaxPmin) != c(T,is.na(BD$SL_MOVIL_PmaxPmin)[1:N-1])), # Vector de grupos (se construye haciendo el cumsum de un veCtor con VERDADERO en puntos de inicio)
                              FUN = cummax
                              )
  TP_FIJO_PmaxPmin <- Zona_PG_Cierre$MinimoRazon[Zona_PG_Cierre$Zona_PG == "Take Profit"]
  BD$TP_MOVIL_PmaxPmin <- shift(ifelse(BD$DECISION_SIF == "HOLD POSITION",
                                       ((1 + BD$PmaxPmin_PE) * (1 + TP_FIJO_PmaxPmin) - 1),
                                       ifelse(BD$DECISION_SIF == "OPEN" | BD$DECISION_SIF == "CLOSE-OPEN",
                                              TP_FIJO_PmaxPmin,
                                              NA
                                              )
                                       ),
                                n=1, fill=NA
                                )
  BD$TP_MOVIL_PmaxPmin <- ifelse(BD$DECISION_SIF == "CLOSE" | BD$DECISION_SIF == "CLOSE-OPEN",
                                 NA, 
                                 BD$TP_MOVIL_PmaxPmin
                                 )
  BD$TP_MOVIL_PmaxPmin <- ave(BD$TP_MOVIL_PmaxPmin,
                              cumsum(is.na(BD$TP_MOVIL_PmaxPmin) != c(T,is.na(BD$TP_MOVIL_PmaxPmin)[1:N-1])), # Vector de grupos (se construye haciendo el cumsum de un veCtor con VERDADERO en puntos de inicio)
                              FUN = cummax
                              )

  # Asignación de zonas de ganancia/pérdida para cierres de posiciones  
  BD$ZONA_PG_PA <- ifelse(BD$PA_PE <= BD$SL_MOVIL_PA,
                          "Stop Loss",
                          ifelse(BD$PA_PE > BD$SL_MOVIL_PA & BD$PA_PE <= 0,
                                 "Low Loss",
                                 ifelse(BD$PA_PE > 0 & BD$PA_PE <= BD$TP_MOVIL_PA,
                                        "Low Profit",
                                        "Take Profit"
                                        )
                                 )
                          )
  BD$ZONA_PG_PmaxPmin <- ifelse(BD$PmaxPmin_PE <= BD$SL_MOVIL_PmaxPmin,
                                "Stop Loss",
                                ifelse(BD$PmaxPmin_PE > BD$SL_MOVIL_PmaxPmin & BD$PmaxPmin_PE <= 0,
                                       "Low Loss",
                                       ifelse(BD$PmaxPmin_PE > 0 & BD$PmaxPmin_PE <= BD$TP_MOVIL_PmaxPmin,
                                              "Low Profit",
                                              "Take Profit"
                                              )
                                       )
                                )
  BD$CIERRE_PA <- -Zona_PG_Cierre$Cierre[match(BD$ZONA_PG_PA, Zona_PG_Cierre$Zona_PG)]
  BD$CIERRE_PmaxPmin <- -Zona_PG_Cierre$Cierre[match(BD$ZONA_PG_PmaxPmin, Zona_PG_Cierre$Zona_PG)]
  BD$CIERREPARCIAL <- ifelse((BD$CIERRE_PA + BD$CIERRE_PmaxPmin) < -1,
                             -1,
                             (BD$CIERRE_PA + BD$CIERRE_PmaxPmin)
                             )
  
  # Acumulado irrestricto de cierres parciales (SIN ajuste por valores menores a -1)
  BD$CIERRE_ACUM <- ave(BD$CIERREPARCIAL, 
                        cumsum(is.na(BD$CIERREPARCIAL) != c(T,is.na(BD$CIERREPARCIAL)[1:N-1])), # Vector de grupos (se construye haciendo el cumsum de un vector con VERDADERO en puntos de inicio)
                        FUN = cumsum
                        )
  
  # Acumulado acotado de cierres parciales (CON ajuste por valores menores a -1)
  BD$CIERRE_ACUM <- ifelse(BD$CIERRE_ACUM < -1,
                           -1,
                           BD$CIERRE_ACUM
                           )
  
  BD$POS_ABIERTA <- ifelse(BD$DECISION_SIF == "OPEN",
                           1, # Si decisión es "OPEN"
                           ifelse(BD$DECISION_SIF == "CLOSE-OPEN", # Si decisiÓn NO es "OPEN"
                                  1, # Si decisiÓn es "CLOSE-OPEN"
                                  ifelse(BD$DECISION_SIF == "CLOSE", # Si decisiÓn NO es "OPEN" ni "CLOSE-OPEN"
                                         0, # Si decisiÓn es "CLOSE"
                                         ifelse(BD$DECISION_SIF == "HOLD POSITION", # Si decisiÓn es NO "OPEN" ni "CLOSE-OPEN" ni "CLOSE"
                                                1 + BD$CIERRE_ACUM, # Si decisiÓn es "HOLD POSITION"
                                                0 # Si decisiÓn es "NO POSITION"
                                         )
                                  )
                           ) 
  )
  
  BD$DECISION_FINAL <- ifelse(BD$DECISION_SIF == "HOLD POSITION",
                              ifelse(BD$POS_ABIERTA == 0,
                                     ifelse(shift(BD$POS_ABIERTA, n=1, fill=NA) > 0,
                                            "CLOSE",
                                            "NO POSITION"
                                     ),
                                     BD$DECISION_SIF # Es decir, "HOLD POSITION"
                              ),
                              ifelse(BD$DECISION_SIF == "CLOSE",
                                     ifelse(shift(BD$POS_ABIERTA == 0, n=1, fill=NA),
                                            "NO POSITION",
                                            BD$DECISION_SIF # Es decir, "CLOSE".
                                     ),
                                     ifelse(BD$DECISION_SIF == "CLOSE-OPEN",
                                            ifelse(shift(BD$POS_ABIERTA == 0, n=1, fill=NA),
                                                   "OPEN",
                                                   BD$DECISION_SIF # Es decir, "CLOSE-OPEN".
                                            ),
                                            BD$DECISION_SIF # Es decir, "OPEN" o "NO POSITION"
                                     )
                              )
  )
  
  #Resultados
  return(BD)
  
}

BDList <- lapply(BDList, FunDecision_Final) # Complementa lista de BDs

# Asignación de PENTRADA, PCIERRE y signos de cortos/largos con base en decisión
# de inversión final

Fun_PE_PC_SENALSIGNO_SL_TP <- function(BD) {
  
  # Asignación de precio de entrada (PENTRADA)
  BD$PENTRADA <- ifelse((BD$DECISION_FINAL == "OPEN"),
                        BD$CLOSE,
                        ifelse((BD$DECISION_FINAL == "CLOSE-OPEN"),
                               BD$CLOSE,
                               NA
                        )
  )
  
  BD$PENTRADA <- na.locf(BD$PENTRADA, na.rm = FALSE) # Arrastre donde no es OPEN o CLOSE-OPEN
  ID_CLOSEOPEN <- which(BD$DECISION_FINAL=="CLOSE-OPEN")
  BD$PENTRADA[ID_CLOSEOPEN] <- shift(BD$PENTRADA, n=1, fill=NA)[ID_CLOSEOPEN] # Para CLOSE-OPEN se deja el PA de la posición cerrada.
  BD$PENTRADA[which(BD$DECISION_FINAL == "NO POSITION")] <- NA # No aplica si no hay posición
  
  # Asignación de precio de cierre (PCIERRE)
  BD$PCIERRE <- ifelse((BD$DECISION_FINAL == "CLOSE"),
                       BD$CLOSE,
                       ifelse((BD$DECISION_FINAL == "CLOSE-OPEN"),
                              BD$CLOSE,
                              NA
                       )
  )
  
  # Asignación de signo según señal para identificar posiciones largas y cortas
  BD$SENALSIGNO <- ifelse((BD$DECISION_FINAL == "OPEN") | (BD$DECISION_FINAL == "HOLD POSITION"),
                          ifelse((BD$SIF == "BUY"),
                                 1,
                                 -1
                          ),
                          ifelse((BD$DECISION_FINAL == "CLOSE") | (BD$DECISION_FINAL == "CLOSE-OPEN"),
                                 ifelse((shift(BD$SIF, n=1, fill=NA) == "BUY"),
                                        1,
                                        -1
                                 ),
                                 0 # Cuando DECISION es "NO POSITION" o NA
                          )
  )
  
  BD$SENALSIGNO <- ifelse(is.na(BD$DECISION_FINAL), 0, BD$SENALSIGNO)

  #Resultados
  return(BD)
  
}

BDList <- lapply(BDList, Fun_PE_PC_SENALSIGNO_SL_TP) # Complementa lista de BDs


# 13. CÁLCULO POSICIÓN Y VALORACIÓN [PROPUESTA] ###############################

# Cargue de parámetros de negociación (BAS y comisión)

BAS <- as.numeric(read_excel(ArchivoInsumos, 
                             sheet = "Insumos",
                             range = "B12",
                             col_names = FALSE
                             )
                  )

Comision <- as.numeric(read_excel(ArchivoInsumos, 
                                  sheet = "Insumos",
                                  range = "B13",
                                  col_names = FALSE
                                  )
                       )

# Cálculo de posición y valoración del portafolio suponiendo solo posiciones largas

Fun_POS_VAL_PORT <- function(BD) {
  
  # Valor de fondeo inicial
  FONDEOINICIAL <- 100
  
  # Columnas para precio de entrada (PENTRADA), volumen y valor de posición inicial
  # y final (VOL_POSINICIAL, VOL_POSFINAL, VAL_POSINICIAL y VAL_POSFINAL), volumen
  # y valor de compras y ventas (VOL_COMPRAS, VOL_VENTAS, VAL_COMPRAS y VAL_VENTAS).
  # También se definen las variables de entrada conocidas, correspondientes a la 
  # primera FH de la serie.
  BD$VOL_POSAPERTURA <- c(0, rep(NA, (N-1)))
  BD$VOL_COMPRAS <- c(0, rep(NA, (N-1)))
  BD$VAL_COMPRAS <- c(0, rep(NA, (N-1)))
  BD$VOL_VENTAS <- c(0, rep(NA, (N-1)))
  BD$VAL_VENTAS <- c(0, rep(NA, (N-1)))
  BD$VOL_POSFINAL <- c(0, rep(NA, (N-1)))
  BD$VAL_POSFINAL <- c(0, rep(NA, (N-1)))
  BD$EFECTIVO <- c(FONDEOINICIAL, rep(NA, (N-1)))
  BD$VAL_PORT <- c((BD$VOL_POSFINAL[1]*BD$CLOSE[1]+BD$EFECTIVO[1]), rep(NA, (N-1)))
  BD$COMISON <- c(0, rep(NA, (N-1)))
  
  for (t in 2:N) {
    
    #BD$VOL_POSINICIAL[t] <- BD$VOL_POSFINAL[t-1]
    #BD$VAL_POSINICIAL[t] <- BD$VAL_POSFINAL[t-1]
    
    if (BD$DECISION_FINAL[t] == "OPEN") { # Si decisiÓn es "OPEN"
      
      BD$VAL_VENTAS[t] <- 0
      BD$VOL_VENTAS[t] <- BD$VAL_VENTAS[t] / BD$PENTRADA[t]
      BD$VAL_COMPRAS[t] <- BD$EFECTIVO[t-1] * (1 - Comision)
      BD$VOL_COMPRAS[t] <- BD$VAL_COMPRAS[t] / (BD$PENTRADA[t] * (1 + BAS))
          
    } else { # Si decisiÓn NO es "OPEN"
      
      if (BD$DECISION_FINAL[t] == "CLOSE-OPEN") { # Si decisiÓn es "CLOSE-OPEN"
        
        BD$VOL_VENTAS[t] <- BD$VOL_POSFINAL[t-1]
        BD$VAL_VENTAS[t] <- (BD$VOL_VENTAS[t] * (BD$PCIERRE[t] * (1 - BAS))) * (1 - Comision)
        BD$VAL_COMPRAS[t] <- BD$VAL_VENTAS[t] * (1 - Comision)
        BD$VOL_COMPRAS[t] <- BD$VAL_COMPRAS[t] / (BD$CLOSE[t] * (1 + BAS))
        
      } else { # Si decisiÓn NO es "OPEN" ni "CLOSE-OPEN"
        
        if (BD$DECISION_FINAL[t] == "CLOSE") { # Si decisiÓn es "CLOSE"
          
          BD$VOL_VENTAS[t] <- BD$VOL_POSFINAL[t-1]
          BD$VAL_VENTAS[t] <- (BD$VOL_VENTAS[t] * (BD$PCIERRE[t] * (1 - BAS))) * (1 + Comision)
          BD$VAL_COMPRAS[t] <- 0
          BD$VOL_COMPRAS[t] <- 0
          
        } else { # Si decisiÓn es NO "OPEN" ni "CLOSE-OPEN" ni "CLOSE"
          
          if (BD$DECISION_FINAL[t] == "HOLD POSITION") { # Si decisiÓn es "HOLD POSITION"
            
            BD$VOL_VENTAS[t] <- 0
            BD$VAL_VENTAS[t] <- 0
            BD$VAL_COMPRAS[t] <- 0
            BD$VOL_COMPRAS[t] <- 0
            
          } else { # Si decisiÓn es "NO POSITION"
            
            BD$VOL_VENTAS[t] <- 0
            BD$VAL_VENTAS[t] <- 0
            BD$VAL_COMPRAS[t] <- 0
            BD$VOL_COMPRAS[t] <- 0
            
          }
          
        }
        
      }
      
    }
    
    # Siempre se calcula la posición final y la valoración del efectivo y del portafolio:
    BD$VOL_POSFINAL[t] <- BD$VOL_POSFINAL[t-1] +
      BD$VOL_COMPRAS[t] -
      BD$VOL_VENTAS[t]
    BD$VAL_POSFINAL[t] <- BD$VOL_POSFINAL[t] * BD$CLOSE[t]
    BD$EFECTIVO[t] <- BD$EFECTIVO[t-1] + BD$VAL_VENTAS[t] - BD$VAL_COMPRAS[t]
    BD$VAL_PORT[t] <- BD$VAL_POSFINAL[t] + BD$EFECTIVO[t]
    BD$COMISON <- (BD$VAL_COMPRAS + BD$VAL_VENTAS) * Comision / (1 - Comision)
    
  }
  
  #Resultados
  return(BD)
  
}

StartT <- Sys.time()
BDList <- lapply(BDList, Fun_POS_VAL_PORT)
EndT <- Sys.time()
TElapsed <- EndT - StartT


# 14. CÁLCULO ESTADÍSTICAS RETORNO Y RIESGO [PROPUESTA] #######################

# Variables y parámetros para los cálculos - Número de franjas horarias por día
NFHD <- as.numeric(read_excel(ArchivoInsumos, 
                              sheet = "Insumos",
                              range = "B2",
                              col_names = FALSE
                             )
                  )

# Variables y parámetros para los cálculos - Nivel de significancia deseado
Significancia <- as.numeric(read_excel(ArchivoInsumos, 
                                       sheet = "Insumos",
                                       range = "B20",
                                       col_names = FALSE
                                 )
                      )

# Variables y parámetros para los cálculos - Ventana móvil para volatilidades
VentanaMovilVol_Dias <- as.numeric(read_excel(ArchivoInsumos, 
                                              sheet = "Insumos",
                                              range = "B19",
                                              col_names = FALSE
                                             )
                                  )
VentanaMovilVol_FH <- VentanaMovilVol_Dias*(NFHD - NFHC)

# Variables y parámetros para los cálculos - BD activo libre de riesgo
BDLibreRiesgo <- read_excel(ArchivoCargue, sheet = "OHLC libre de riesgo")
colnames(BDLibreRiesgo) <- c("DATE","FRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")
BDLibreRiesgo$DATEFRAME <- make_datetime(year = year(BDLibreRiesgo$DATE), 
                                         month = month(BDLibreRiesgo$DATE), 
                                         day = day(BDLibreRiesgo$DATE), 
                                         hour = hour(BDLibreRiesgo$FRAME), 
                                         min = minute(BDLibreRiesgo$FRAME), 
                                         sec = second(BDLibreRiesgo$FRAME)
                                        )
BDLibreRiesgo[,c("DATE","FRAME")] <- NULL
BDLibreRiesgo <- BDLibreRiesgo[,c("DATEFRAME","VOLUME","OPEN","HIGH","LOW","CLOSE")]
BDLibreRiesgo <- BDLibreRiesgo[order(BDLibreRiesgo$DATEFRAME),]
NLR <- length(BDLibreRiesgo$DATEFRAME)

# Función para el cálculo de estadísticas de riesgo y retorno
Fun_Est_Riesgo_Retorno <- function(BD) {
  
  # Columnas
  BD$RET <- c(0, rep(NA, (N-1)))
  BD$RET_ACUM <- c(0, rep(NA, (N-1)))
  BD$VAL_PORT_ACUM_B100 <- c(100, rep(NA, (N-1)))
  BD$PERD_ACUM <- c(0, rep(NA, (N-1)))
  BD$MAX_PERD_ACUM <- c(0, rep(NA, (N-1)))
  
  # Retorno de cada FH
  BD$RET <- ((BD$VAL_PORT / c(0, BD$VAL_PORT[1:N-1]) - 1)) * c(NA, BD$SENALSIGNO[1:N-1]) 
  # Retorno acumulado y acumulado base 100
  BD$RET_ACUM <- c(NA, (cumprod(1 + BD$RET[2:N]) - 1))
  BD$VAL_PORT_ACUM_B100 <- (BD$RET_ACUM + 1) * 100
  # Pérdida acumulada y máxima pérdida acumulada
  BD$PERD_ACUM <- c(NA, (BD$VAL_PORT_ACUM_B100[2:N]) / (cummax(BD$VAL_PORT_ACUM_B100[2:N])) - 1)
  BD$MAX_PERD_ACUM <- c(NA, cummin(BD$PERD_ACUM[2:N]))
  # BD diaria
  BDRETDAILY <- BD %>% select(DATEFRAME, DATE, RET, MAX_PERD_ACUM) %>% 
                       group_by(DATE) %>%
                       mutate(RETDAY = (cumprod(1 + na.replace(RET, 0)) - 1)) %>%
                       top_n(1, DATEFRAME) %>%
                       select(-DATEFRAME, -RET)
  # BD mensual
  BDRETMONTHLY <- BD %>% mutate(YEARMONTH = paste0(year(DATE), "-", format(DATE, "%m"))) %>%
                         select(DATEFRAME, YEARMONTH, RET, MAX_PERD_ACUM) %>% 
                         group_by(YEARMONTH) %>%
                         mutate(RETMONTH = cumprod(1 + na.replace(RET, 0)) - 1) %>%
                         top_n(1, DATEFRAME) %>%
                         select(-DATEFRAME, -RET)
  # BD anual
  BDRETANNUALY <- BD %>% mutate(YEAR = year(DATEFRAME)) %>%
                         select(DATEFRAME, YEAR, RET, MAX_PERD_ACUM) %>% 
                         group_by(YEAR) %>%
                         mutate(RETYEAR = cumprod(1 + na.replace(RET, 0)) - 1) %>%
                         top_n(1, DATEFRAME)  %>%
                         select(-DATEFRAME, -RET)
  
  # Estadísticas finales retorno y riesgo
  RET_ACUM <- BD$RET_ACUM[N] # Retorno acumulado desde inicio
  VAL_PORT_ACUM_B100 <- BD$VAL_PORT_ACUM_B100[N] # Retorno acumulado base 100 desde inicio
  RET_ACUM_ANUAL <- (1 + RET_ACUM)^(365*(NFHD-NFHC)/(N-1)) - 1 # Retorno acumulado anualizado desde inicio
  MAXPERDACUM <- BD$MAX_PERD_ACUM[N] # Máxima pérdida desde inicio (MDD)
  RAA_MPA <- RET_ACUM_ANUAL / -MAXPERDACUM # Retorno anual ajustado por riesgo de pérdida
  
  # Otras estadísticas de riesgo - Volatilidad FH
  BD$VolFH <- rollapplyr(data = BD$RET, width = VentanaMovilVol_FH, FUN = sd, fill = NA)
  VolFH <- sd(BD$RET, na.rm = TRUE)
  
  # Otras estadísticas de riesgo - Volatilidad diaria
  BDRETDAILY$VolD <- rollapplyr(data = BDRETDAILY$RETDAY, width = VentanaMovilVol_Dias, FUN = sd, fill = NA)
  VolD <- sd(BDRETDAILY$RETDAY, na.rm = TRUE)
  
  # Otras estadísticas de riesgo - Volatilidad mensual
  VolM <- BDRETDAILY %>% mutate(YEARMONTH = paste0(year(DATE), "-", format(DATE, "%m"))) %>%
                         mutate(VolM = sqrt(20) * VolD) %>%
                         select(DATE, YEARMONTH, VolM) %>% 
                         group_by(YEARMONTH) %>%
                         top_n(1, DATE) %>%
                         select(-DATE)
  BDRETMONTHLY$VolM <- VolM$VolM
  VolM <- sqrt(20) * VolD

  # Otras estadísticas de riesgo - Volatilidad anual
  VolA <- BDRETDAILY %>% mutate(YEAR = year(DATE)) %>%
                         mutate(VolA = sqrt(252) * VolD) %>%
                         select(DATE, YEAR, VolA) %>% 
                         group_by(YEAR) %>%
                         top_n(1, DATE)  %>%
                         select(-DATE)
  BDRETANNUALY$VolA <- VolA$VolA
  VolA <- sqrt(252) * VolD
  
  # Otras estadísticas de riesgo - Razón de Sharpe (anual)
  BDLibreRiesgo$RETLR <- c(0, rep(NA, (NLR-1)))
  BDLibreRiesgo$RETLR <- ((BDLibreRiesgo$CLOSE / c(NA, BDLibreRiesgo$CLOSE[1:NLR-1]) - 1))
  BDLibreRiesgo <- BDLibreRiesgo[, c("DATEFRAME", "RETLR")]
  BD <- BD %>% left_join(BDLibreRiesgo, by = c("DATEFRAME"))
  BD$RETLR[is.na(BD$RETLR)] <- 0
  BD$RET_ACUM_LR <- c(0, rep(NA, (N-1)))
  BD$RET_ACUM_LR <- c(NA, (cumprod(1 + BD$RETLR[2:N]) - 1))
  BDRETLRDAILY <- BD %>% select(DATEFRAME, DATE, RETLR) %>% 
    group_by(DATE) %>%
    mutate(RETLRDAY = cumprod(1 + na.replace(RETLR, 0)) - 1) %>%
    top_n(1, DATEFRAME) %>%
    select(-DATEFRAME, -RETLR)
  BDRETDAILY$RETLRDAY <- BDRETLRDAILY$RETLRDAY
  RET_PROM_LR_ANUAL <- mean(BDRETDAILY$RETLRDAY) * 30 * 12
  RET_PROM_ANUAL <- mean(BDRETDAILY$RETDAY) * 30 * 12
  SharpeRAnual <- (RET_PROM_ANUAL - RET_PROM_LR_ANUAL) / VolA
  
  # Otras estadísticas de riesgo - Razón de Sortino (anual)
  BD$RETINFOBJ <- ifelse(BD$RET > BD$RETLR, 0, (BD$RET - BD$RETLR))
  BD$VolInfObjFH <- rollapplyr(data = BD$RETINFOBJ, width = VentanaMovilVol_FH, FUN = sd, fill = NA)
  VolInfObjFH <- sd(BD$RETINFOBJ, na.rm = TRUE)
  
  BDRETDAILY$RETINFOBJ <- ifelse(BDRETDAILY$RETDAY > BDRETDAILY$RETLRDAY, 
                                 0, 
                                 (BDRETDAILY$RETDAY - BDRETDAILY$RETLRDAY)
                                )
  BDRETDAILY$VolInfObjD <- rollapplyr(data = BDRETDAILY$RETINFOBJ, width = VentanaMovilVol_Dias, FUN = sd, fill = NA)
  VolInfObjD <- sd(BDRETDAILY$RETINFOBJ, na.rm = TRUE)
  
  VolInfObjM <- BDRETDAILY %>% mutate(YEARMONTH = paste0(year(DATE), "-", format(DATE, "%m"))) %>%
                               mutate(VolInfObjM = sqrt(20) * VolInfObjD) %>%
                               select(DATE, YEARMONTH, VolInfObjM) %>% 
                               group_by(YEARMONTH) %>%
                               top_n(1, DATE) %>%
                               select(-DATE)
  BDRETMONTHLY$VolInfObjM <- VolInfObjM$VolInfObjM
  VolInfObjM <- sqrt(20) * VolInfObjD
  
  VolInfObjA <- BDRETDAILY %>% mutate(YEAR = year(DATE)) %>%
                               mutate(VolInfObjA = sqrt(252) * VolInfObjD) %>%
                               select(DATE, YEAR, VolInfObjA) %>% 
                               group_by(YEAR) %>%
                               top_n(1, DATE)  %>%
                               select(-DATE)
  BDRETANNUALY$VolInfObjA <- VolInfObjA$VolInfObjA
  VolInfObjA <- sqrt(252) * VolInfObjD

  RET_PROM_OBJ_ANUAL <- RET_PROM_LR_ANUAL
  SortinoRAnual <- (RET_PROM_ANUAL - RET_PROM_OBJ_ANUAL) / VolInfObjA
  
  # Otras estadísticas de riesgo - VaR histórico y CVaR histórico (diarios)
  HVaRD <- quantile(x = BDRETDAILY$RETDAY, probs = Significancia)
  HCVaRD <- mean(BDRETDAILY$RETDAY[which(BDRETDAILY$RETDAY < HVaRD)])
  
  # Otras estadísticas de riesgo - VaR histórico y CVaR histórico (mensuales)
  HVaRM <- quantile(x = BDRETMONTHLY$RETMONTH, probs = Significancia)
  HCVaRM <- mean(BDRETMONTHLY$RETMONTH[which(BDRETMONTHLY$RETMONTH < HVaRM)])

  # Otras estadísticas de riesgo - VaR histórico y CVaR histórico (anuales)  
  BDRETMONTHLY$RET12MONTHS <- t(last(t(rollapplyr(data = (1+BDRETMONTHLY$RETMONTH),
                                                  width = 12, 
                                                  FUN = cumprod, 
                                                  fill = NA
                                                 )
                                      )
                                    )
                               ) -
                               1  
  HVaRA <- quantile(x = BDRETMONTHLY$RET12MONTHS, probs = Significancia, na.rm = TRUE)
  HCVaRA <- mean(BDRETMONTHLY$RET12MONTHS[which(BDRETMONTHLY$RET12MONTHS < HVaRA)], na.rm = TRUE)
  
  
  #PVaRM <- 
  #PCVaRM <- 
  #PVaRA <- 
  #PCVaRA <- 
  
  #Porcentaje de retornos negativos
  #Frecuencia de retornos menores a -1%, -5%, -10%, objetivo (libre riesgo)
  #Peor retorno
  
  
  # Gráfico utilidad acumulada
  G_VAL_PORT_ACUM_B100 <- ggplot(BD, aes(x = DATE, y = VAL_PORT_ACUM_B100)) +
    geom_line(size = 1) +
    ggtitle("Valor portafolio (Base 100)") + 
    xlab("Fecha") + ylab("Valor (Base 100)") +
    expand_limits(x =BD$DATE[1]) +
    expand_limits(y = 0) +
    PlantillaG

  #Resultados
  BD_Est_Retorno_Riesgo <- list(BD = BD, 
                                BDRDiario = BDRETDAILY,
                                BDRMensual = BDRETMONTHLY,
                                BDRAnual = BDRETANNUALY,
                                VAlPortAcumB100 = VAL_PORT_ACUM_B100, 
                                RetAcumAnual = RET_ACUM_ANUAL,
                                MaxPerdAcum = MAXPERDACUM, 
                                RAA_MPA = RAA_MPA, 
                                VolatilidadFH = VolFH,
                                VolatilidadDiaria = VolD,
                                VolatilidadMensual = VolM,
                                VolatilidadAnual = VolA,
                                RetornoPromedioLRAnual = RET_PROM_LR_ANUAL,
                                RetornoPromedioAnual = RET_PROM_ANUAL,
                                RazonSharpeAnual = SharpeRAnual,
                                VolatilidadInferiorObjetivoFH = VolInfObjFH,
                                VolatilidadInferiorObjetivoDiaria = VolInfObjD,
                                VolatilidadInferiorObjetivoMensual = VolInfObjM,
                                VolatilidadInferiorObjetivoAnual = VolInfObjA,
                                RetornoPromedioObjetivoAnual = RET_PROM_OBJ_ANUAL,
                                RazonSortinoAnual = SortinoRAnual,
                                VaRHistoricoDiario = HVaRD,
                                CVaRHistoricoDiario = HCVaRD,
                                VaRHistoricoMensual = HVaRM,
                                CVaRHistoricoMensual = HCVaRM,
                                VaRHistoricoAnual = HVaRA,
                                CVaRHistoricoAnual = HCVaRA,
                                Graf_ValPortAcumB100 = G_VAL_PORT_ACUM_B100
                               )
  
  return(BD_Est_Retorno_Riesgo)
  
}

BDList <- lapply(BDList, Fun_Est_Riesgo_Retorno) # Construye lista de listas
names(BDList) <- rownames(Senales)


# 15. RESUMEN ESTADÍSTICAS ESTRATEGIAS [PROPUESTA] ############################

# Función para obtención de estadísticas totales de retorno y riesgo de cada estrategia
Fun_R_R_Tot <- function(List) {
  R_R <- data.frame(VAlPortAcumB100 = List$VAlPortAcumB100,
                    RetAcumAnual = List$RetAcumAnual,
                    MaxPerdAcum = List$MaxPerdAcum,
                    RAA_MPA = List$RAA_MPA,
                    VolatilidadFH = List$VolatilidadFH,
                    VolatilidadDiaria = List$VolatilidadDiaria,
                    VolatilidadMensual = List$VolatilidadMensual,
                    VolatilidadAnual = List$VolatilidadAnual,
                    RetornoPromedioLRAnual = List$RetornoPromedioLRAnual,
                    RetornoPromedioAnual = List$RetornoPromedioAnual,
                    RazonSharpeAnual = List$RazonSharpeAnual,
                    VolatilidadInferiorObjetivoFH = List$VolatilidadInferiorObjetivoFH,
                    VolatilidadInferiorObjetivoDiaria = List$VolatilidadInferiorObjetivoDiaria,
                    VolatilidadInferiorObjetivoMensual = List$VolatilidadInferiorObjetivoMensual,
                    VolatilidadInferiorObjetivoAnual = List$VolatilidadInferiorObjetivoAnual,
                    RetornoPromedioObjetivoAnual = List$RetornoPromedioObjetivoAnual,
                    RazonSortinoAnual = List$RazonSortinoAnual,
                    VaRHistoricoDiario = List$VaRHistoricoDiario,
                    CVaRHistoricoDiario = List$CVaRHistoricoDiario,
                    VaRHistoricoMensual = List$VaRHistoricoMensual,
                    CVaRHistoricoMensual = List$CVaRHistoricoMensual,
                    VaRHistoricoAnual = List$VaRHistoricoAnual,
                    CVaRHistoricoAnual = List$CVaRHistoricoAnual
                   )
  return(R_R)
}

# Obtención y exportación de estadísticas retorno y riesgo de cada estrategia
SenalesSwell <- Senales
Senales <- cbind(Senales, t(sapply(BDList, Fun_R_R_Tot)))
Senales$RetAcum <- unlist(Senales$RetAcum)
Senales$ValAcumB100 <- unlist(Senales$ValAcumB100)
Senales$MaxPerdAcum <- unlist(Senales$MaxPerdAcum)
Senales$RAA_MPA <- unlist(Senales$RAA_MPA)
Senales$RazonSharpeAnual <- unlist(Senales$RazonSharpeAnual)
Senales$RazonSortinoAnual <- unlist(Senales$RazonSortinoAnual)

# Gráfico RetAcum/MDD por estrategia
G_RetAcum_MDD <- ggplot(Senales, aes(x = rownames(Senales), y = RAA_MPA)) +
  geom_col() +
  ggtitle("Ret.Acum/MDD por estrategia") + 
  xlab("Estrategia") + ylab("Ret.Acum/MDD") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_RetAcum_MDD)
ggsave(path = BaseDirPath, 
       plot = G_RetAcum_MDD,
       filename = "Ret.Acum_MDD por estrategia.png",
       scale = 2
      )

# Gráfico RetAcum/MDD para estrategias por encima del promedio
Estrategias_RAA_MPA_Mayores_Promedio <-  Senales %>% filter(RAA_MPA > mean(RAA_MPA)) 
G_RetAcum_MDD_MayMed <- ggplot(Estrategias_RAA_MPA_Mayores_Promedio, 
                               aes(x = reorder(rownames(Estrategias_RAA_MPA_Mayores_Promedio), RAA_MPA), y = RAA_MPA)) +
  geom_col() +
  ggtitle("Ret.Acum/MDD para estrategias por encima del promedio") + 
  xlab("Estrategia") + ylab("Ret.Acum/MDD") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_RetAcum_MDD_MayMed)
ggsave(path = BaseDirPath, 
       plot = G_RetAcum_MDD_MayMed,
       filename = "Ret.Acum_MDD para estrategias por encima del promedio.png",
       scale = 2
      )

# Gráfico RetAcum/MDD para estrategias por encima del objetivo
RA_MDD_Objetivo <- as.numeric(read_excel(ArchivoInsumos, 
                                         sheet = "Insumos",
                                         range = "B24",
                                         col_names = FALSE
                                        )
                             )
Estrategias_RAA_MPA_Mayores_Objetivo <-  Senales %>% filter(RAA_MPA > RA_MDD_Objetivo)

G_RetAcum_MDD_MayObj <- ggplot(Estrategias_RAA_MPA_Mayores_Objetivo, 
                               aes(x = reorder(rownames(Estrategias_RAA_MPA_Mayores_Objetivo), RAA_MPA), y = RAA_MPA)) +
  geom_col() +
  ggtitle("Ret.Acum/MDD para estrategias por encima del objetivo") + 
  xlab("Estrategia") + ylab("Ret.Acum/MDD") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_RetAcum_MDD_MayObj)
ggsave(path = BaseDirPath, 
       plot = G_RetAcum_MDD_MayObj,
       filename = "Ret.Acum_MDD para estrategias por encima del objetivo.png",
       scale = 2
      )

# Gráfico razón de Sharpe por estrategia
G_SharpeR <- ggplot(Senales, aes(x = rownames(Senales), y = RazonSharpeAnual)) +
  geom_col() +
  ggtitle("Razón de Sharpe por estrategia") + 
  xlab("Estrategia") + ylab("Razón de Sharpe") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_SharpeR)
ggsave(path = BaseDirPath, 
       plot = G_SharpeR,
       filename = "Razón de Sharpe por estrategia.png",
       scale = 2
      )

# Gráfico Razón de Sharpe para estrategias por encima del promedio
Estrategias_Sharpe_Mayores_Promedio <-  Senales %>% filter(RazonSharpeAnual > mean(RazonSharpeAnual)) 
G_Sharpe_MayMed <- ggplot(Estrategias_Sharpe_Mayores_Promedio, 
                               aes(x = reorder(rownames(Estrategias_Sharpe_Mayores_Promedio), RazonSharpeAnual), y = RazonSharpeAnual)) +
  geom_col() +
  ggtitle("Razón de Sharpe para estrategias por encima del promedio") + 
  xlab("Estrategia") + ylab("Razón de Sharpe") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_Sharpe_MayMed)
ggsave(path = BaseDirPath, 
       plot = G_Sharpe_MayMed,
       filename = "Razón de Sharpe para estrategias por encima del promedio.png",
       scale = 2
      )

# Gráfico Razón de Sharpe para estrategias por encima del objetivo
Sharpe_Objetivo <- as.numeric(read_excel(ArchivoInsumos, 
                                         sheet = "Insumos",
                                         range = "B25",
                                         col_names = FALSE
                                        )
                             )
Estrategias_Sharpe_Mayores_Objetivo <-  Senales %>% filter(RazonSharpeAnual > Sharpe_Objetivo)

G_Sharpe_MayObj <- ggplot(Estrategias_Sharpe_Mayores_Objetivo, 
                               aes(x = reorder(rownames(Estrategias_Sharpe_Mayores_Objetivo), RazonSharpeAnual), y = RazonSharpeAnual)) +
  geom_col() +
  ggtitle("Razón de Sharpe para estrategias por encima del objetivo") + 
  xlab("Estrategia") + ylab("Razón de Sharpe") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_Sharpe_MayObj)
ggsave(path = BaseDirPath, 
       plot = G_Sharpe_MayObj,
       filename = "Razón de Sharpe para estrategias por encima del objetivo.png",
       scale = 2
      )

# Gráfico Razón de Sortino por estrategia
G_SortinoR <- ggplot(Senales, aes(x = rownames(Senales), y = RazonSortinoAnual)) +
  geom_col() +
  ggtitle("Razón de Sortino por estrategia") + 
  xlab("Estrategia") + ylab("Razón de Sortino") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_SortinoR)
ggsave(path = BaseDirPath, 
       plot = G_SortinoR,
       filename = "Razón de Sortino por estrategia.png",
       scale = 2
      )

# Gráfico Razón de Sortino para estrategias por encima del promedio
Estrategias_Sortino_Mayores_Media <-  Senales %>% filter(RazonSortinoAnual > mean(RazonSortinoAnual)) 
G_Sortino_MayMed <- ggplot(Estrategias_Sortino_Mayores_Media, 
                               aes(x = reorder(rownames(Estrategias_Sortino_Mayores_Media), RazonSortinoAnual), y = RazonSortinoAnual)) +
  geom_col() +
  ggtitle("Razón de Sortino para estrategias por encima del promedio") + 
  xlab("Estrategia") + ylab("Razón de Sortino") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_Sortino_MayMed)
ggsave(path = BaseDirPath, 
       plot = G_Sortino_MayMed,
       filename = "Razón de Sortino para estrategias por encima del promedio.png",
       scale = 2
      )

# Gráfico Razón de Sortino para estrategias por encima del objetivo
Sortino_Objetivo <- as.numeric(read_excel(ArchivoInsumos, 
                                         sheet = "Insumos",
                                         range = "B26",
                                         col_names = FALSE
                                         )
                              )
Estrategias_Sortino_Mayores_Objetivo <-  Senales %>% filter(RazonSortinoAnual > Sortino_Objetivo)

G_Sortino_MayObj <- ggplot(Estrategias_Sortino_Mayores_Objetivo, 
                               aes(x = reorder(rownames(Estrategias_Sortino_Mayores_Objetivo), RazonSortinoAnual), y = RazonSortinoAnual)) +
  geom_col() +
  ggtitle("Razón de Sortino para estrategias por encima del objetivo") + 
  xlab("Estrategia") + ylab("Razón de Sortino") +
  expand_limits(x = 0) +
  expand_limits(y = 0) +
  PlantillaG
print(G_Sortino_MayObj)
ggsave(path = BaseDirPath, 
       plot = G_Sortino_MayObj,
       filename = "Razón de Sortino para estrategias por encima del objetivo.png",
       scale = 2
      )


# 16. ESTRATEGIA ÓPTIMA [PROPUESTA] ###########################################

# Identificación estrategia óptima
NEstrategiaOpt <- which.max(Senales$RAA_MPA)
EstrategiaOpt <- rownames(Senales)[NEstrategiaOpt]
paste0("La estrategia óptima es ", EstrategiaOpt)

# Visualización y exportación BD estrategia óptima
View(BDList[[EstrategiaOpt]]$BD)
ArchivoResultadosOpt <- createWorkbook()
addWorksheet(wb = ArchivoResultadosOpt, sheetName = "Senales")
writeData(wb = ArchivoResultadosOpt, sheet = "Senales", x = Senales)
addWorksheet(wb = ArchivoResultadosOpt, sheetName = "BDI")
writeData(wb = ArchivoResultadosOpt, sheet = "BDI", x = BDList[[EstrategiaOpt]]$BD)

# Visualización y exportación gráfico valor portafolio estrategia óptima
Graf_ValPortAcumB100 <- BDList[[EstrategiaOpt]]$Graf_ValPortAcumB100 +
  ggtitle(paste0("Valor portafolio estrategia ", EstrategiaOpt, " (Base 100)"))
print(Graf_ValPortAcumB100)
ggsave(path = BaseDirPath, 
       plot = Graf_ValPortAcumB100,
       filename = "Valor portafolio estrategia.png",
       scale = 2
      ) 

# Visualización y exportación de estadísticas de retorno y riesgo de estrategia óptima
View(BDList[[EstrategiaOpt]]$BDRDiario)
addWorksheet(wb = ArchivoResultadosOpt, sheetName = "RetornosDiarios")
writeData(wb = ArchivoResultadosOpt, sheet = "RetornosDiarios", x = BDList[[EstrategiaOpt]]$BDRDiario)
View(BDList[[EstrategiaOpt]]$BDRMensual)
addWorksheet(wb = ArchivoResultadosOpt, sheetName = "RetornosMensuales")
writeData(wb = ArchivoResultadosOpt, sheet = "RetornosMensuales", x = BDList[[EstrategiaOpt]]$BDRMensual)
View(BDList[[EstrategiaOpt]]$BDRAnual)
addWorksheet(wb = ArchivoResultadosOpt, sheetName = "RetornosAnuales")
writeData(wb = ArchivoResultadosOpt, sheet = "RetornosAnuales", x = BDList[[EstrategiaOpt]]$BDRAnual)
Nombre_BD_EstrategiaOpt <- Senales$NombreBD[NEstrategiaOpt]
NombreArchivoResultadosOpt <- paste0(Nombre_BD_EstrategiaOpt, "_Propuesta.xlsx")
saveWorkbook(wb = ArchivoResultadosOpt, 
             file = NombreArchivoResultadosOpt, 
             overwrite = TRUE
            )


# 17. ESTRATEGIAS ADICIONALES [PROPUESTA] #####################################

# Para visualizar la información y estadísticas de una estrategia en particular,
# a continuación asigne a la variable "Estrategia" la cambinación deseada de 
# fractales intradiario y de referencia con el formato "I#R#". Por ejemplo, para
# el fractal intradiario 2 y el fractal intradiario de referencia 8 use "I2R8":

# Identificación estrategia seleccionada
EstrategiaSel <- "I5R9"
NEstrategiaSel <- which(rownames(Senales) == EstrategiaSel)
paste0("La estrategia seleccionada es ", EstrategiaSel)

# Visualización y exportación BD estrategia seleccionada
View(BDList[[EstrategiaSel]]$BD)
ArchivoResultadosSel <- createWorkbook()
addWorksheet(wb = ArchivoResultadosSel, sheetName = "Senales")
writeData(wb = ArchivoResultadosSel, sheet = "Senales", x = Senales)
addWorksheet(wb = ArchivoResultadosSel, sheetName = "BDI")
writeData(wb = ArchivoResultadosSel, sheet = "BDI", x = BDList[[EstrategiaSel]]$BD)

# Visualización y exportación gráfico valor portafolio estrategia seleccionada
Graf_ValPortAcumB100 <- BDList[[EstrategiaSel]]$Graf_ValPortAcumB100 +
  ggtitle(paste0("Valor portafolio estrategia ", EstrategiaSel, " (Base 100)"))
print(Graf_ValPortAcumB100)
ggsave(path = BaseDirPath, 
       plot = Graf_ValPortAcumB100,
       filename = "Valor portafolio estrategia.png",
       scale = 2
      ) 

# Visualización y exportación de estadísticas de retorno y riesgo de estrategia seleccionada
View(BDList[[EstrategiaSel]]$BDRDiario)
addWorksheet(wb = ArchivoResultadosSel, sheetName = "RetornosDiarios")
writeData(wb = ArchivoResultadosSel, sheet = "RetornosDiarios", x = BDList[[EstrategiaSel]]$BDRDiario)
View(BDList[[EstrategiaSel]]$BDRMensual)
addWorksheet(wb = ArchivoResultadosSel, sheetName = "RetornosMensuales")
writeData(wb = ArchivoResultadosSel, sheet = "RetornosMensuales", x = BDList[[EstrategiaSel]]$BDRMensual)
View(BDList[[EstrategiaSel]]$BDRAnual)
addWorksheet(wb = ArchivoResultadosSel, sheetName = "RetornosAnuales")
writeData(wb = ArchivoResultadosSel, sheet = "RetornosAnuales", x = BDList[[EstrategiaSel]]$BDRAnual)
Nombre_BD_EstrategiaSel <- Senales$NombreBD[NEstrategiaSel]
NombreArchivoResultadosSel <- paste0(Nombre_BD_EstrategiaSel, "_Propuesta.xlsx")
saveWorkbook(wb = ArchivoResultadosSel, 
             file = NombreArchivoResultadosSel, 
             overwrite = TRUE
            )


# XX. OBSERVACIONES ###########################################################


#1 Comision swell
#2 Crear funcion que exporte lo de cada estrategia de un top-10 de estrategias
#3 Correr R/MDD para todas las estrategias y exportar top-10 (anualizar MDD?)
#4 Rankings
##4.1 Calcular estadisticas restantes y dejarlas en "Senales"
##4.3 Generar rankings para cada una de ellas y dejarlos en "Puntajes"
##4.4 Calcular puntajes y dejarlos en "Puntajes"
##4.5 Exportar lo de cada estrategia top-10 de "Puntajes"
#1 Columnas Raul en codigo Swell

# Falta apalancamiento
# Falta SL y TP
# Ajustar instrucciones