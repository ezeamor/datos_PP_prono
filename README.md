# datos_PP_prono

####################################################

# LECTURA DE DATOS DE PRECIPITACION Y PRONOSTICO

####################################################

rm(list=ls()) # Por si quiero limpiar el environment.
graphics.off() # Elimina configuracion de graficos previos.
setwd("/home/ezequiel.amor") # Cambiar segun donde esten los datos.

####################################################

# LECTURA DE DATOS DE PRECIPITACION

####################################################

# Cargo el archivo con los datos en una variable.

datos <- read.csv("RESISTENCIAAERO.txt",sep=";",dec=",",stringsAsFactors = FALSE)

# Aquellos espacios en blanco, caracteres sin espacio o "S/D" los reescribo como NA. 
# A los datos "S/P" y "<0.1" los reescribo como "0" (cero).
# "S/D" implica "Sin Dato", lo que quiere decir que no se sabe si hubo precipitacion 
# o no porque no se midio, mientras que "S/P" implica "Sin Precipitacion", por lo 
# que en este caso si hubo medicion pero no se registro precipitacion.

datos[datos ==" " | datos == "S/D"]     <- NA
datos[datos == "S/P" | datos == "<0.1"] <- 0

# Genero una lista para guardar los datos de cada anio y un vector con los anios del archivo.

datos_por_anio <- list()
anios <- c(1964:2022)

# Guardo en cada elemento de la lista un vector con los datos de precipitacion de cada anio.

for(i in 1:length(anios)) {
  datos_por_anio[[i]] <- c(datos[seq(19+434*(i-1),409+434*(i-1),13),1],
                           datos[seq(20+434*(i-1),410+434*(i-1),13),1],
                           datos[seq(21+434*(i-1),411+434*(i-1),13),1],
                           datos[seq(22+434*(i-1),412+434*(i-1),13),1],
                           datos[seq(23+434*(i-1),413+434*(i-1),13),1],
                           datos[seq(24+434*(i-1),414+434*(i-1),13),1],
                           datos[seq(25+434*(i-1),415+434*(i-1),13),1],
                           datos[seq(26+434*(i-1),416+434*(i-1),13),1],
                           datos[seq(27+434*(i-1),417+434*(i-1),13),1],
                           datos[seq(28+434*(i-1),418+434*(i-1),13),1],
                           datos[seq(29+434*(i-1),419+434*(i-1),13),1],
                           datos[seq(30+434*(i-1),420+434*(i-1),13),1])
}

# Elimino aquellos dias que ocupan lugar en los datos pero no tiene informacion. 
# Estos son los 29/2 cuando el anio no es bisiesto, los 30/2 que no existen y los 
# 31 de aquellos meses que no tienen.
# Ademas se reemplazan las comas de los decimales por puntos y se transforman los 
# datos a numeros para luego poder hacer calculos.

for(i in 1:length(anios)) {
  datos_por_anio[[i]] <- datos_por_anio[[i]][-which(datos_por_anio[[i]] == "")]
  datos_por_anio[[i]] <- sub(",", ".",datos_por_anio[[i]], fixed = TRUE)
  datos_por_anio[[i]] <- as.numeric(datos_por_anio[[i]])
}

# A la lista anterior se le pueden sumar mas datos que provengan de otros archivos, entonces genero
# una nueva variable para leer los datos y poder aniadirlos.

datos1 <- read.csv("RESISTENCIAAERO_2023.txt",sep=";",dec=",",stringsAsFactors = FALSE)

# Reescribo los datos como en el caso anterior.

datos1[datos1 == " " | datos1 == "S/D"]    <- NA
datos1[datos1 == "S/P" | datos1 == "<0.1"] <- 0

# Reescribo el vector de anios con aquel al que pertenecen los datos nuevos.

anios <- c(1964:2023)

# Como el archivo nuevo puede no contar con los datos de todos los meses, genero un condicional
# que me permita saber cuantos meses de datos tengo y asi extraerlos correctamente.

if(datos1[7,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(8,68,2),1])
} else if(datos1[8,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(9,99,3),1],
                                       datos1[seq(10,100,3),1])
} else if(datos1[9,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(10,130,4),1],
                                       datos1[seq(11,131,4),1],
                                       datos1[seq(12,132,4),1])
} else if(datos1[10,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(11,161,5),1],
                                       datos1[seq(12,162,5),1],
                                       datos1[seq(13,163,5),1],
                                       datos1[seq(14,164,5),1])
} else if(datos1[11,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(12,192,6),1],
                                       datos1[seq(13,193,6),1],
                                       datos1[seq(14,194,6),1],
                                       datos1[seq(15,195,6),1],
                                       datos1[seq(16,196,6),1])
} else if(datos1[12,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(13,222,7),1],
                                       datos1[seq(14,223,7),1],
                                       datos1[seq(15,224,7),1],
                                       datos1[seq(16,225,7),1],
                                       datos1[seq(17,226,7),1],
                                       datos1[seq(18,227,7),1])
} else if(datos1[13,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(14,254,8),1],
                                       datos1[seq(15,255,8),1],
                                       datos1[seq(16,256,8),1],
                                       datos1[seq(17,257,8),1],
                                       datos1[seq(18,258,8),1],
                                       datos1[seq(19,259,8),1],
                                       datos1[seq(20,260,8),1])
} else if(datos1[14,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(15,285,9),1],
                                       datos1[seq(16,410,9),1],
                                       datos1[seq(17,411,9),1],
                                       datos1[seq(18,412,9),1],
                                       datos1[seq(19,413,9),1],
                                       datos1[seq(20,414,9),1],
                                       datos1[seq(21,415,9),1],
                                       datos1[seq(22,416,9),1])
} else if(datos1[15,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(16,316,10),1],
                                       datos1[seq(17,317,10),1],
                                       datos1[seq(18,318,10),1],
                                       datos1[seq(19,319,10),1],
                                       datos1[seq(20,320,10),1],
                                       datos1[seq(21,321,10),1],
                                       datos1[seq(22,322,10),1],
                                       datos1[seq(23,323,10),1],
                                       datos1[seq(24,324,10),1])
} else if(datos1[16,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(17,347,11),1],
                                       datos1[seq(18,348,11),1],
                                       datos1[seq(19,349,11),1],
                                       datos1[seq(20,350,11),1],
                                       datos1[seq(21,351,11),1],
                                       datos1[seq(22,352,11),1],
                                       datos1[seq(23,353,11),1],
                                       datos1[seq(24,354,11),1],
                                       datos1[seq(25,355,11),1],
                                       datos1[seq(26,356,11),1])
} else if(datos1[17,1] == "1") {
  datos_por_anio[[length(anios)]] <- c(datos1[seq(18,378,12),1],
                                       datos1[seq(19,379,12),1],
                                       datos1[seq(20,380,12),1],
                                       datos1[seq(21,381,12),1],
                                       datos1[seq(22,382,12),1],
                                       datos1[seq(23,383,12),1],
                                       datos1[seq(24,384,12),1],
                                       datos1[seq(25,385,12),1],
                                       datos1[seq(26,386,12),1],
                                       datos1[seq(27,387,12),1],
                                       datos1[seq(28,388,12),1])
} else { 
  datos_por_anio[[length(anios)]] <- c(datos1[seq(19,409,13),1],
                                       datos1[seq(20,410,13),1],
                                       datos1[seq(21,411,13),1],
                                       datos1[seq(22,412,13),1],
                                       datos1[seq(23,413,13),1],
                                       datos1[seq(24,414,13),1],
                                       datos1[seq(25,415,13),1],
                                       datos1[seq(26,416,13),1],
                                       datos1[seq(27,417,13),1],
                                       datos1[seq(28,418,13),1],
                                       datos1[seq(29,419,13),1],
                                       datos1[seq(30,420,13),1])
}

# Nuevamente elimino los datos que no me dan informacion, cambio el tipo de decimal 
# y transformo los datos a numeros.

datos_por_anio[[length(anios)]] <- datos_por_anio[[length(anios)]][-which(datos_por_anio[[length(anios)]] == "")]
datos_por_anio[[length(anios)]] <- sub(",", ".",datos_por_anio[[length(anios)]], fixed = TRUE)
datos_por_anio[[length(anios)]] <- as.numeric(datos_por_anio[[length(anios)]])

######## LECTURA PARA ARCHIVOS DE TEXTO EN COLUMNA ########

# En caso de tener archivo de texto con ancho fijo, puedo usar otra funcion para
# leer los datos.

datos_smn <- read.table("197654-2.txt",skip=1,head=F,sep=",")
colnames(datos_smn) <- c("Estacion","Fechas","Tmax","Tmin","Precipitacion")

datos_Formosa    <- data.frame("fecha"=datos_smn$Fechas[which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2020-11-20"):
                                                                 which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2023-04-27")],
                               "PP"=as.numeric(datos_smn$Precipitacion[which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2020-11-20"):
                                                              which(datos_smn$Estacion=="87162" & datos_smn$Fechas=="2023-04-27")]))

datos_Corrientes <- data.frame("fecha"=datos_smn$Fechas[which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2020-11-20"):
                                                          which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2023-04-27")],
                               "PP"=as.numeric(datos_smn$Precipitacion[which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2020-11-20"):
                                                              which(datos_smn$Estacion=="87166" & datos_smn$Fechas=="2023-04-27")]))

####################################################

# LECTURA DE DATOS DE PRONOSTICO Y CALCULO DE 
# OCURRENCIA DE CATEGORIA DE ACUMULADO

####################################################

#Libreria a utilizar.

library(readxl) # Lee archivos excel.

# Cargo el archivo con los datos en una variable.

prono <- as.data.frame(read_xlsx("App_Pron_S2.xlsx"))

# Tengo que calcular la probabilidad de la ocurrencia de cada categoria de acumulado,
# para eso primero guardo los datos de precipitacion acumulada pronosticada por fecha 
# para analizar que pasa en los 16 miembros de cada dia en una lista.
# Tambien guardo las fechas de referencia para los pronosticos.

PP <- list()
prono$FechaRef <- as.character(prono$FechaRef)

fechas_prono <- prono$FechaRef[seq(1,which.max(prono$FechaRef=="2023-04-13")+15,16)]

# Guardo en cada elemento de la lista los 16 miembros de pronostico para cada dia.

for(i in 1:length(fechas_prono)) {
  PP[[i]] <- prono$PreS2_C[(1+16*(i-1)):(16+16*(i-1))]
}

# Ahora que cuento con los datos para cada fecha, calculo la probalidad de ocurrencia de cada categoria.
# Para ello, cuento la cantidad de miembros donde el pronostico cumple que se acumula igual o mas 
# precipitacion de la que indica cada categoria y lo divido por los miembros totales.

Probabilidad_1   <- c()
Probabilidad_20  <- c()
Probabilidad_50  <- c()
Probabilidad_100 <- c()

for(i in 1:length(fechas_prono)) {
  Probabilidad_1[i]   <- (length(PP[[i]][which(PP[[i]] >= 1)])/16)
  Probabilidad_20[i]  <- (length(PP[[i]][which(PP[[i]] >= 20)])/16)
  Probabilidad_50[i]  <- (length(PP[[i]][which(PP[[i]] >= 50)])/16)
  Probabilidad_100[i] <- (length(PP[[i]][which(PP[[i]] >= 100)])/16)
} 

# Guardo las fechas y los vectores de probabilidad de ocurrencia de las categorias en un dataframe.

Probabilidad <- data.frame("FechaRef"=fechas_prono,"Prob_1mm"=Probabilidad_1,"Prob_20mm"=Probabilidad_20,
                           "Prob_50mm"=Probabilidad_50,"Prob_100mm"=Probabilidad_100)

####################################################

# CALCULO DE ACUMULADOS DE LA SEMANA 2

####################################################

# Los pronosticos son desde el 12/11/2020, asi que la primera "semana 2" es la 
# del 20/11/2020 al 26/11/2020. Los acumulados se calculan entonces desde esa semana. 
# La ultima semana a trabajar es la del 21/4/2023 al 27/4/2023 asociado al 
# pronostico del 13/4/2023, ya que solo se cuenta con datos de PP hasta el 30/4/2023.

# Armo un dataframe que combine las fechas de todo el periodo de datos y la 
# precipitacion registrada. A los datos de precipitacion le saco el formato de 
# lista para que queden todos en un unico vector.
# Modificar las fechas en caso de que sean diferentes ante el uso de nuevos datos.

fechas <- as.character(seq(as.Date("1964-01-01"),as.Date("2023-04-30"),1))

Precipitacion <- unlist(datos_por_anio)

datos_PP <- data.frame("Fechas"=fechas, "PP"=Precipitacion, stringsAsFactors = FALSE)

# Ahora extraigo de todos los datos de precipitacion aquellos que permiten calcular 
# los acumulados de cada semana 2 (o sea, desde el 20/11/2020 al 27/04/2023). 
# Tambien genero un vector con todas las fechas que existen entre la primera y 
# la ultima de referencia.

datos_PP_prono <- datos_PP$PP[which(datos_PP$Fechas == fechas_prono[9]):
                                (which(datos_PP$Fechas == fechas_prono[length(fechas_prono)])+14)]

fechas_totales <- as.character(seq(as.Date("2020-11-12"),as.Date("2023-04-13"),1))

# Realizo los calculos de los acumulados de la semana 2. 
# Como toma 7 dias de datos, los ultimos 6 acumulados son NA dado que no tengo 
# informacion mas alla del 27/04/2023 (ejemplo: si me paro en el 22/04/2023, 
# necesito el dato del 28/04/2023, pero no lo tengo). Esos datos luego los elimino.

acumulados <- c()

for(i in 1:length(datos_Corrientes$PP)) {
  acumulados[i] <- sum(datos_Corrientes$PP[(1+(i-1)):(7+(i-1))]) # Agregar na.rm=T si hay NAs en los datos.
}

acumulados <- acumulados[-which(is.na(acumulados))] # Correr si no tengo NAs en los datos (mas alla de los ultimos 6).

acumulados <- acumulados[-c(884:889)] # Correr si tengo NAs en los datos (mas alla de los ultimos 6).

# Guardo las fechas y los acumulados en un dataframe.

datos_totales <- data.frame("A"=fechas_totales,"B"=acumulados)

# Necesito solo los datos de precipitacion acumulada para las fechas de referencia que hay
# en el excel. Hasta ahora tengo datos para todo el periodo 12/11/2020-13/04/2023 con sus
# respectivas semana 2, pero en los datos reales no todas las fechas presentan pronosticos.
# Selecciono entonces aquellos datos de acumulados donde las fechas totales del 
# periodo coincidan con aquellas donde hubo pronostico.

acumulados1 <- datos_totales$B[which(fechas_totales %in% fechas_prono)]

# IMPORTANTE: Si estoy trabajando con los datos de Resistencia Aero, continuo
# para generar el dataframe con toda la informacion. Pero si estoy trabajando con
# otra estacion, saltearse hasta la linea 329 (no correr ninguna linea hasta pasar
# la de "PP_acum") porque el dataframe original ya fue construido y sino se 
# perderia toda la informacion.

# Genero vectores con las fechas de inicio y final de las semana 2.

S2_Desde <- c()
S2_Hasta <- c()

for(i in 1:length(fechas_prono)) {
  S2_Desde[i] <- datos_PP$Fechas[which(datos_PP$Fechas == fechas_prono[i])+8]
  S2_Hasta[i] <- datos_PP$Fechas[which(datos_PP$Fechas == fechas_prono[i])+14]
}

# Genero un dataframe con las fechas de referencia del pronostico, de inicio y fin de la semana 2
# y los acumulados.

PP_acum <- data.frame("FechaRef"=fechas_prono,"S2_Desde"=S2_Desde,"S2_Hasta"=S2_Hasta,
                      "Acum_S2"=acumulados1)

# Hasta aca se obtuvo toda la informacion de Resistencia Aero, pero si tengo alguna
# estacion mas (como en este caso Corrientes o Formosa), entonces agrego otra 
# columna de acumulado en el dataframe anterior. Con la siguiente linea se
# agregaran todos los datos de precipitacion acumulada para cualquier estacion
# nueva con la que se trabaje, por lo que debe cambiarse el nombre segun la 
# estacion a agregar.

# IMPORTANTE: los acumulados de las estaciones de la RCB se guardan en el dataframe
# "PP_acum" desde el archivo "datos_estaciones". Aca solo se guardan datos de 
# estaciones del SMN.

PP_acum$Acum_S2_Corrientes <- acumulados1

# Agrego tambien en "Probabilidad" las fechas de inicio y final de cada semana 2.

Probabilidad$S2_Desde <- S2_Desde
Probabilidad$S2_Hasta <- S2_Hasta

#####################################################

# CALCULO DE OCURRENCIA DE PRECIPITACION ACUMULADA

#####################################################

# Ya calculados los acumulados de la semana 2, debe verificarse la ocurrencia de 
# precipitacion acumulada dentro cada categoría del pronostico (es decir, si para 
# cada acumulado se registro >=1mm, >=20mm, >=50mm y >=100mm).
# Se guarda la informacion en vectores, donde con "1" se determina que se registro 
# un acumulado dentro de la categoria estudiada y con "0" que el acumulado fue menor.

Ocurrencia_PP_1mm   <- c()
Ocurrencia_PP_20mm  <- c()
Ocurrencia_PP_50mm  <- c()
Ocurrencia_PP_100mm <- c()

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=1) {
    Ocurrencia_PP_1mm[i] <- 1
  } else {
    Ocurrencia_PP_1mm[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=20) {
    Ocurrencia_PP_20mm[i] <- 1
  } else {
    Ocurrencia_PP_20mm[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=50) {
    Ocurrencia_PP_50mm[i] <- 1
  } else {
    Ocurrencia_PP_50mm[i] <- 0}
}

for(i in 1:length(PP_acum$Acum_S2)){
  if(PP_acum$Acum_S2[i]>=100) {
    Ocurrencia_PP_100mm[i] <- 1
  } else {
    Ocurrencia_PP_100mm[i] <- 0}
}

#####################################################

# CALCULO DE OCURRENCIA DE CATEGORIA DE ACUMULADO
# CLIMATOLOGICA

#####################################################

# Al momento de trabajar con la ocurrencia de la precipitacion acumulada,
# es importante tener la base climatologica de la ocurrencia de cada categoria
# de acumulado. Para ello, se deberan calcular los acumulados de cada semana 2
# durante un periodo de 30 anios.
# El periodo de 30 anios a utlizar es el de 1989-2018, de esta forma puedo tomar
# los primeros dias de 2019 para poder realizar los calculos de la semana 2 que
# incluye dias de diciembre y enero del anio siguiente y no estar parado en 
# algun anio que se use dentro del pronostico.

# Primero genero un vector de fechas para luego poder obtener los datos de PP
# asociado a todas esas fechas. Tomo todo el mes de enero y no solo los primeros
# dias para no tener problemas con las variaciones de las posiciones por los 
# anios bisciestos.
# Extraigo también los datos de PP asociados a dichas fechas y guardo todo en un
# dataframe para organizar la informacion.

fechas_clima <- fechas[which(fechas=="1989-01-01"):which(fechas=="2019-01-31")]

datos_PP_clima <- datos_PP$PP[which(datos_PP$Fechas == fechas_clima[1]):
                                which(datos_PP$Fechas == fechas_clima[length(fechas_clima)])]

datos_clima <- data.frame("Fechas"=fechas_clima,"PP"=datos_PP_clima)

# Ahora genero una lista donde voy a guardar en cada elemento los 30 datos de
# precipitacion acumulada en cada anio del periodo de referencia. Los acumulados
# se calculan segun las fechas de cada semana 2 guardadas en el dataframe 
# "PP_acum".

acum_clima    <- list()
acum_S2_clima <- c()

for(i in 1:length(PP_acum$FechaRef)) {
    for(j in 1:30) {
      acum_S2_clima[j] <- sum(datos_clima$PP[which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*(j-1))],1,4),substr(PP_acum$S2_Desde[i],6,10),sep="-")):
                                               which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*(j-1))],1,4),substr(PP_acum$S2_Hasta[i],6,10),sep="-"))],na.rm = T)
      acum_clima[[i]] <- acum_S2_clima
  }  
}

# Al momento de hacer lo acumulados, las semanas 2 que arrancan entre los dias
# 26 y 31 de diciembre tienen asociados dias de enero del anio siguiente. El 
# ciclo anterior no reconoce ese salto de anio (sino que sigue parado en el 
# mismo) entonces calcula un acumulado incorrecto. Ante esto, miro cuales son las 
# posiciones correspondientes a las fechas entre el 26 y 31 de diciembre para 
# que luego reemplace esos acumulados por los correctos, donde el anio de los 
# ultimos dias de la semana sea el siguiente y no el mismo.

dias_diciembre <- c(which(substr(PP_acum$S2_Desde,6,10) == "12-26"),which(substr(PP_acum$S2_Desde,6,10) == "12-27"),
                    which(substr(PP_acum$S2_Desde,6,10) == "12-28"),which(substr(PP_acum$S2_Desde,6,10) == "12-29"),
                    which(substr(PP_acum$S2_Desde,6,10) == "12-30"),which(substr(PP_acum$S2_Desde,6,10) == "12-31"))
dias_diciembre <- dias_diciembre[order(dias_diciembre)]

for(i in dias_diciembre) {
  for(j in 1:30) {
    acum_S2_clima[j] <- sum(datos_clima$PP[which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*(j-1))],1,4),substr(PP_acum$S2_Desde[i],6,10),sep="-")):
                                             which(datos_clima$Fechas == paste(substr(datos_clima$Fechas[1+(366*j)],1,4),substr(PP_acum$S2_Hasta[i],6,10),sep="-"))],na.rm = T)
    acum_clima[[i]]  <- acum_S2_clima
  }
}

# Ahora calculo la probabilidad de que ocurra cada categoria de acumulado a 
# nivel climatologico.

Probabilidad_1_clima   <- c()
Probabilidad_20_clima  <- c()
Probabilidad_50_clima  <- c()
Probabilidad_100_clima <- c()

for(i in 1:length(PP_acum$FechaRef)) {
  Probabilidad_1_clima[i]   <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 1)])/30)
  Probabilidad_20_clima[i]  <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 20)])/30)
  Probabilidad_50_clima[i]  <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 50)])/30)
  Probabilidad_100_clima[i] <- (length(acum_clima[[i]][which(acum_clima[[i]] >= 100)])/30)
}

# Agrego las probabilidades al dataframe "Probabilidad" para tener tanto las 
# probabilidades de cada semana 2 segun los miembros de cada fecha de referencia
# como tambien las probabilidades climatologicas.

Probabilidad$Prob_1mm_clima   <- Probabilidad_1_clima
Probabilidad$Prob_20mm_clima  <- Probabilidad_20_clima
Probabilidad$Prob_50mm_clima  <- Probabilidad_50_clima
Probabilidad$Prob_100mm_clima <- Probabilidad_100_clima

#####################################################

# Lectura de datos para periodos calido y frio

#####################################################

# Voy a extraer los datos separados en periodos calidos y frios del anio. El
# periodo calido va de octubre a marzo y el frio de abril a septiembre.

################## PERIODO CALIDO ###################

# Solo tomo los datos que corresponden a los meses de octubre y marzo, pero 
# siempre y cuando el ultimo dia de la semana 2 no sea de abril (sin importar si
# el dia de inicio de la semana 2 es en marzo).

PP_acum_calido <- c()
PP_acum_calido <- PP_acum$Acum_S2[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                          substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido <- data.frame("Acum"=PP_acum_calido)

# Agrego las probabilidades de cada categoría y las climatologicas asociadas al
# periodo calido.

datos_calido$Prob_1mm <- Probabilidad$Prob_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_20mm <- Probabilidad$Prob_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_50mm <- Probabilidad$Prob_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_100mm <- Probabilidad$Prob_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_1mm_clima <- Probabilidad$Prob_1mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_20mm_clima <- Probabilidad$Prob_20mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Prob_50mm_clima <- Probabilidad$Prob_50mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "12")] 

datos_calido$Prob_100mm_clima <- Probabilidad$Prob_100mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                                       substr(PP_acum$S2_Hasta,6,7) == "12")]

# Agrego el numero de la semana 2 a la que corresponden las probabilidades y 
# los acumulados.

datos_calido$Semana_2 <- which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                 substr(PP_acum$S2_Hasta,6,7) == "12")

# Agrego las ocurrencias de cada categoria para las fechas del periodo calido.

datos_calido$Ocurrencia_1mm <- Ocurrencia_PP_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Ocurrencia_20mm <- Ocurrencia_PP_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Ocurrencia_50mm <- Ocurrencia_PP_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "12")]

datos_calido$Ocurrencia_100mm <- Ocurrencia_PP_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "01" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "02" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "03" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "10" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "11" | 
                                                             substr(PP_acum$S2_Hasta,6,7) == "12")]

################### PERIODO FRIO ####################

# Solo tomo los datos que corresponden a los meses de abril y septiembre, pero 
# siempre y cuando el ultimo dia de la semana 2 no sea de octubre (sin importar 
# si el dia de inicio de la semana 2 es en septiembre).

PP_acum_frio <- c()
PP_acum_frio <- PP_acum$Acum_S2[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                        substr(PP_acum$S2_Hasta,6,7) == "09")]


datos_frio <- data.frame("Acum"=PP_acum_frio)

# Agrego las probabilidades de cada categoría y las climatologicas asociadas al
# periodo frio.

datos_frio$Prob_1mm <- Probabilidad$Prob_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_20mm <- Probabilidad$Prob_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_50mm <- Probabilidad$Prob_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_100mm <- Probabilidad$Prob_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_1mm_clima <- Probabilidad$Prob_1mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                 substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_20mm_clima <- Probabilidad$Prob_20mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Prob_50mm_clima <- Probabilidad$Prob_50mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                   substr(PP_acum$S2_Hasta,6,7) == "09")] 

datos_frio$Prob_100mm_clima <- Probabilidad$Prob_100mm_clima[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                                     substr(PP_acum$S2_Hasta,6,7) == "09")] 

# Agrego el numero de la semana 2 a la que corresponden las probabilidades y 
# los acumulados.

datos_frio$Semana_2 <- which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                               substr(PP_acum$S2_Hasta,6,7) == "05" | 
                               substr(PP_acum$S2_Hasta,6,7) == "06" | 
                               substr(PP_acum$S2_Hasta,6,7) == "07" | 
                               substr(PP_acum$S2_Hasta,6,7) == "08" | 
                               substr(PP_acum$S2_Hasta,6,7) == "09")

# Agrego las ocurrencias de cada categoria para las fechas del periodo frio.

datos_frio$Ocurrencia_1mm <- Ocurrencia_PP_1mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                       substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Ocurrencia_20mm <- Ocurrencia_PP_20mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Ocurrencia_50mm <- Ocurrencia_PP_50mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                         substr(PP_acum$S2_Hasta,6,7) == "09")]

datos_frio$Ocurrencia_100mm <- Ocurrencia_PP_100mm[which(substr(PP_acum$S2_Hasta,6,7) == "04" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "05" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "06" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "07" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "08" | 
                                                           substr(PP_acum$S2_Hasta,6,7) == "09")]

####################################################

# CASOS EXTREMOS.

####################################################

# Primero busco aquellos casos donde habia pronostico nulo de precipitacion pero
# se registró acumulado.

# Como no existen probabilidades de acumulado nulo, tomo los casos con la proba-
# bilidad de lluvia más bajas.

print(which(Probabilidad$Prob_1mm < "0.1"))
print(Probabilidad$Prob_1mm[which(Probabilidad$Prob_1mm < "0.1")])
print(PP_acum$Acum_S2[which(Probabilidad$Prob_1mm < "0.1")])
print(PP_acum$S2_Desde[which(Probabilidad$Prob_1mm < "0.1")])
print(PP_acum$S2_Hasta[which(Probabilidad$Prob_1mm < "0.1")])

# El resto de las categorias si presentan probabilidad nula, por lo que miro
# en que semana donde se tenía dicha probabilidad si llovio.

# Categorias 20 mm.

prob_nulo_20 <- which(Probabilidad$Prob_20mm == "0")
print(PP_acum$Acum_S2[prob_nulo_20[which(PP_acum$Acum_S2[which(Probabilidad$Prob_20mm == "0")]>=20)]])
print(PP_acum$S2_Desde[prob_nulo_20[which(PP_acum$Acum_S2[which(Probabilidad$Prob_20mm == "0")]>=20)]])
print(PP_acum$S2_Hasta[prob_nulo_20[which(PP_acum$Acum_S2[which(Probabilidad$Prob_20mm == "0")]>=20)]])

# Categorias 50 mm.

prob_nulo_50 <- which(Probabilidad$Prob_50mm == "0")
print(PP_acum$Acum_S2[prob_nulo_50[which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm == "0")]>=50)]])
print(PP_acum$S2_Desde[prob_nulo_50[which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm == "0")]>=50)]])
print(PP_acum$S2_Hasta[prob_nulo_50[which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm == "0")]>=50)]])

# Categorias 100 mm.

prob_nulo_100 <- which(Probabilidad$Prob_100mm == "0")
print(PP_acum$Acum_S2[prob_nulo_100[which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm == "0")]>=100)]])
print(PP_acum$S2_Desde[prob_nulo_100[which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm == "0")]>=100)]])
print(PP_acum$S2_Hasta[prob_nulo_100[which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm == "0")]>=100)]])


# Ahora miro los casos donde la probabilidad de acumular 50mm o 100mm eran igua-
# les o superiores al 50% pero no se registro precipitacion.

# Categoria 50mm.

extremo_50 <- which(Probabilidad$Prob_50mm >= "0.5")

extremos_50mm <- data.frame("Prob_50mm"=Probabilidad$Prob_50mm[extremo_50[
  which(PP_acum$Acum_S2[which(Probabilidad$Prob_50mm >= "0.5")]<50)]])

extremos_50mm$Inicio_S2 <- PP_acum$S2_Desde[extremo_50[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_50mm >= "0.5")]<50)]]
  
extremos_50mm$Fin_S2 <- PP_acum$S2_Hasta[extremo_50[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_50mm >= "0.5")]<50)]]

extremos_50mm$Acum <- PP_acum$Acum_S2[extremo_50[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_50mm >= "0.5")]<50)]]

# Categoria 100mm.

extremo_100 <- which(Probabilidad$Prob_100mm >= "0.5")

extremos_100mm <- data.frame("Prob_100mm"=Probabilidad$Prob_100mm[extremo_100[
  which(PP_acum$Acum_S2[which(Probabilidad$Prob_100mm >= "0.5")]<100)]])

extremos_100mm$Inicio_S2 <- PP_acum$S2_Desde[extremo_100[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_100mm >= "0.5")]<100)]]

extremos_100mm$Fin_S2 <- PP_acum$S2_Hasta[extremo_100[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_100mm >= "0.5")]<100)]]

extremos_100mm$Acum <- PP_acum$Acum_S2[extremo_100[which(PP_acum$Acum_S2[
  which(Probabilidad$Prob_100mm >= "0.5")]<100)]]
