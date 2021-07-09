
# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(skimr)
library(propagate)
library(nortest)
options (scipen = 999) #Permite mostrar decimales
# BD ----------------------------------------------------------------------

###Carga información de precios
grupos <- read_excel("Grupos.xlsx",sheet = "Hoja2")
datos <- read_excel("BD Proyecto Semanal 26.06.21.xlsx", sheet = "Datos",
                    col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 3)
names(datos)[1] = "FECHA" ##Renombrar nombre de primera columna

###Carga información de Retornos
retornos <- read_excel("BD Proyecto Semanal 26.06.21.xlsx", 
                       sheet = "Retornos", col_types = c("date", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric"))
names(retornos)[1] = "FECHA"
#Nemos seleccionados
nemo_seleccionado <- c("FECHA","IPSA","BCHILE", "VAPORES", "PARAUCO", "Casos", "Cobre",
                       "COPEC", "CAP","Dosis", "Dosis_Completa","FALABELLA",
                       "AGUAS_ANDINAS", "Muertes", "Oro", "Recuperados",
                       "CONCHATORO", "ENTEL", "USDCLP", "SONDA", "VIX")

nemo_correlacion <- c("FECHA","IPSA","BCHILE", "VAPORES", "PARAUCO", "Cobre",
                             "COPEC", "CAP", "FALABELLA","AGUAS_ANDINAS",  "Oro",
                             "CONCHATORO", "ENTEL", "USDCLP", "SONDA", "VIX")

datos <- datos %>% 
  filter(!is.na(FECHA))##Elimina primera fila de N/A
retornos <- retornos %>% 
  filter(!is.na(FECHA))
retornos <- retornos %>% 
  filter(!is.na(IPSA))
#retornos <- retornos %>% 
  #mutate(IPSA = IPSA*100)
#Eliminar retorno de fecha 27-05-2016 debido a que no existe información y afecta la media

#Apertura la tabla de datos(Precios) entre antes de la pandemia y post pandemia



datos_pre_pandemia <- datos %>% 
  filter(FECHA < "2020-03-13") %>% 
  dplyr::select(nemo_seleccionado) 
  

datos_post_pandemia <- datos %>% 
  filter(FECHA >="2020-03-13") %>% 
  dplyr::select(nemo_seleccionado)
  
#Apertura la tabla de retornos entre antes de la pandemia y post pandemia
retornos_pre_pandemia <- retornos %>% 
  filter(FECHA < "2020-03-12") %>% 
  dplyr::select(nemo_seleccionado)

retornos_post_pandemia <- retornos %>% 
  filter(FECHA >= "2020-03-12") %>% 
  dplyr::select(nemo_seleccionado)
  

#Precios y retornos agrupados en una sola variable
datos_agrupado_pre <- gather(data = datos_pre_pandemia, key = "NEMO", value = "PRECIO", 2:20)
datos_agrupado_post <- gather(data = datos_post_pandemia, key = "NEMO", value = "PRECIO", 2:20)

retornos_agrupado_pre <- gather(data = retornos_pre_pandemia, key = "NEMO", value = "RETORNO", 2:20)
retornos_agrupado_post <- gather(data = retornos_post_pandemia, key = "NEMO", value = "RETORNO", 2:20)
retornos_agrupado_pre <- mutate(retornos_agrupado_pre, RETORNO = RETORNO*100)

retornos_agrupado_post <- mutate(retornos_agrupado_post, RETORNO = RETORNO*100)

#Incorpora columna de clasificación 
retornos_agrupado_pre <- retornos_agrupado_pre %>% 
  dplyr::left_join( grupos, by = c("NEMO"="nemo"))
#Reemplaza N/A de nueva columna de grupo por el texto SIN CLASIFICACIón
skim(retornos_agrupado_pre)
retornos_agrupado_pre <- retornos_agrupado_pre %>% 
  mutate(grupo = replace_na(grupo, "SIN CLASIFICACION")) 

# Análisis descriptivo ----------------------------------------------------
datos %>% 
  dplyr::select(FECHA,nemo_seleccionado) %>% 
  skim()
skim(datos_pre_pandemia)
skim(datos_post_pandemia)


skim(retornos$BCHILE*100)
###Para el comportamiento del retorno de Banco chile desde el año 2016 a junio 2021, podemos observar que la desviación estandar es 3,41% con una media de 0,04% concentrando el 50% de los retornos en la parte negativa.
fitDistr(retornos$BCHILE*100)
###Al analizar la distribución podemos observar que destribuye Cauchy 


skim(retornos_pre_pandemia$IPSA*100)
#Antes de la pandemia la media era de un 0,03% con una desviación estandar de 2,47 es decir datos con una menor variabilidad respecto a todo el conjunto de información Ademas a partir del percentil 50 los retornos comienzan a ser positivos así como la cantidad de información se concentra en retornos positivos
fitDistr(retornos_pre_pandemia$BCHILE*100) #Distribuye laplace

ggplot(retornos_pre_pandemia, aes(x = BCHILE)) +
  geom_boxplot()

ggplot(retornos_pre_pandemia, aes(x = FECHA, y = BCHILE*100)) +
  geom_point()


skim(retornos_post_pandemia$IPSA*100)
#Después la pandemia la media era de un 0,09% con una desviación estandar de 5.41 es decir datos con mayor variabilidad respecto a todo el conjunto de información y al periodo pre pandemia. Ademas los datos se concentran en retornos negativos



ggplot(retornos_post_pandemia, aes(x = FECHA,y = BCHILE*100)) +
  geom_point()

ggplot(retornos_agrupado_post, aes(x = FECHA, y = RETORNO, color= NEMO) )+
  geom_point()

# Análisis descriptivo IPSA -----------------------------------------------

skim(retornos$IPSA*100)
skim(retornos$FECHA)
#Al realizar el análisis descriptivo de la variable IPSA, desde el 03-06-2016 al 25-06-2021, en donde tenemos 265 observaciones sin datos perdidos, podemos observar que la información tiene una media de 0,0366%, con una desviación estandar de 3,06. Además en el histograma se puede observar que a partir del P50 se concentran los datos, obteniendo como resultado retornos positivos.
skim(retornos$IPSA*100)
ggplot(retornos, aes(x=FECHA, y = retornos$IPSA*100)) +
  geom_line()
ggplot(retornos, aes( y = retornos$IPSA*100)) +
  geom_boxplot()
ggplot(retornos, aes( x = retornos$IPSA*100)) +
  geom_density() +
  geom_histogram() 
  
  
df <- tibble(boxplot.stats(retornos$IPSA*100))
view(df)
retornos
#Al realizar una exploración visual, podemos observar que los retornos de nuestra variable IPSA distribuyen normal, se realizará un test para aceptar o no la hipótesis nula realizando un test
lillie.test(retornos$IPSA)
#Al realizar un test de normalidad lillie.test de la variable ipsa, podemos observar que el p-value = 0.00000001918 es menor a 0,05 lo que implica que no existe evidencia significativa para aceptar la hipótesis nula de supuesto de normalidad.
#Para corroborar se utiliza la librería de R -  Propagete en la cual podemos observar que el IPSA distribuye T
######################### IPSA PRE -----------------
skim(retornos_pre_pandemia$IPSA*100)
#Al realizar el análisis descriptivo de la variable IPSA, desde en el escenario 2, en donde tenemos 197 observaciones sin datos perdidos, podemos observar que la información tiene una media de 0,035%, con una desviación estandar de 2.05. Además en el histograma se puede observar que a partir del P50 se concentran los datos, obteniendo como resultado retornos positivos.
ggplot(retornos_pre_pandemia, aes(x=FECHA, y = retornos_pre_pandemia$IPSA*100)) +
  geom_line()
ggplot(retornos_pre_pandemia, aes( y = retornos_pre_pandemia$IPSA*100)) +
  geom_boxplot()
ggplot(retornos_pre_pandemia, aes( x = retornos_pre_pandemia$IPSA*100)) +
  geom_histogram()
#Al realizar una exploración visual, podemos observar que los retornos de nuestra variable IPSA distribuyen normal, se realizará un test para aceptar o no la hipótesis nula realizando un test
lillie.test(retornos_pre_pandemia$IPSA*100)
#Al realizar un test de normalidad lillie.test de la variable ipsa, podemos observar que el p-value = 0.00000001918 es menor a 0,05 lo que implica que no existe evidencia significativa para aceptar la hipótesis nula de supuesto de normalidad.
#Para corroborar se utiliza la librería de R -  Propagete en la cual podemos observar que el IPSA distribuye T

######################### IPSA POST -----------------

skim(retornos_post_pandemia$IPSA*100)
ggplot(retornos_post_pandemia, aes(x=FECHA, y = retornos_post_pandemia$IPSA*100)) +
  geom_line()
ggplot(retornos_post_pandemia, aes( y = retornos_post_pandemia$IPSA*100)) +
  geom_boxplot()
boxplot.stats(retornos_post_pandemia$IPSA*100)

ggplot(retornos_post_pandemia, aes( x = retornos_post_pandemia$IPSA*100)) +
  geom_histogram()
lillie.test(retornos_post_pandemia$IPSA*100)


######################### COBRE -----------------

# retornos cobre ----------------------------------------------------------



skim(retornos$Cobre*100)

mean(retornos$Cobre*100, na.rm = TRUE)
retornos %>% 
  
  mutate(Cobre = replace_na(Cobre,mean(retornos$Cobre*100, na.rm = TRUE) )) %>% 
skim(Cobre)

which(is.na(retornos$Cobre))
retornos[247,]$Cobre



ggplot(retornos, aes(x=FECHA, y = retornos$Cobre*100)) +
  geom_line()
ggplot(retornos, aes( y = retornos$Cobre*100)) +
  geom_boxplot()
boxplot.stats(retornos$Cobre*100)

ggplot(retornos, aes( x = retornos$Cobre*100)) +
  geom_histogram()
lillie.test(retornos$Cobre*100)

# retornos cobre PRE----------------------------------------------------------

skim(retornos_pre_pandemia$Cobre*100)

ggplot(retornos_pre_pandemia, aes(x=FECHA, y = retornos_pre_pandemia$Cobre*100)) +
  geom_line()
ggplot(retornos_pre_pandemia, aes( y = retornos_pre_pandemia$Cobre*100)) +
  geom_boxplot()
boxplot.stats(retornos_pre_pandemia$Cobre*100)

ggplot(retornos, aes( x = retornos$Cobre*100)) +
  geom_histogram()
lillie.test(retornos_pre_pandemia$Cobre*100)


# retornos cobre POST----------------------------------------------------------


skim(retornos_post_pandemia$Cobre*100)

ggplot(retornos, aes(x=FECHA, y = retornos$Cobre*100)) +
  geom_line()
ggplot(retornos, aes( y = retornos$Cobre*100)) +
  geom_boxplot()
boxplot.stats(retornos$Cobre*100)

ggplot(retornos, aes( x = retornos$Cobre*100)) +
  geom_histogram()
lillie.test(retornos$Cobre*100)

######################### VIX -----------------

skim(retornos$VIX*100)
summary(retornos$VIX*100)

ggplot(retornos, aes(x=FECHA, y = retornos$VIX)) +
  geom_line()
ggplot(retornos, aes( y = retornos$Cobre*100)) +
  geom_boxplot()
boxplot.stats(retornos$Cobre*100)

ggplot(retornos, aes( x = retornos$Cobre*100)) +
  geom_histogram()
lillie.test(retornos$Cobre*100)


























skim(retornos_pre_pandemia$IPSA*100)
ggplot(retornos_pre_pandemia, aes(x=FECHA, y = retornos_pre_pandemia$IPSA*100)) +
  geom_line()
ggplot(retornos_pre_pandemia, aes(x=FECHA, y = retornos_pre_pandemia$IPSA*100)) +
  geom_boxplot()
boxplot(retornos_pre_pandemia$IPSA)
boxplot.stats(retornos_pre_pandemia$IPSA*100)

ggplot(retornos_pre_pandemia, aes( x = retornos_pre_pandemia$IPSA*100)) +
  geom_histogram()


fitDistr(retornos$IPSA*100)
fitDistr(retornos_post_pandemia$IPSA*100)
fitDistr(retornos_pre_pandemia$IPSA*100)

df_retornos <- data.frame(grupo = "retornos",skim(retornos)) 
df_retornos <- df_retornos %>% 
    na.omit(df_retornos) %>% 
  
aux <- data.frame(grupo = "post",skim(retornos_post_pandemia))  

df_retornos <-  rbind(df_retornos, aux)

aux <- data.frame(grupo = "pre",skim(retornos_pre_pandemia))  
df_retornos <-  rbind(df_retornos, aux)

write.csv2(df_retornos, "df_retornos.csv")

df_retornos <- df_retornos %>% 
  filter(skim_variable == "IPSA" | skim_variable ==  "USDCLP" | skim_variable ==  "Oro" | skim_variable ==  "VIX" | skim_variable ==  "Cobre")

df_retornos %>% 
  arrange(skim_variable) %>% 
  View()
# Matriz de correlación ---------------------------------------------------
str(retornos_pre_pandemia)


correlacion_retornos <- retornos %>% 
  dplyr::select(nemo_correlacion) %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  cor()
ggcorrplot::ggcorrplot(correlacion_retornos)


correlacion_retornos_pre <- retornos_pre_pandemia %>% 
  dplyr::select(nemo_correlacion) %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  cor()


ggcorrplot::ggcorrplot(correlacion_retornos_pre)

correlacion_retornos_post <- retornos_post_pandemia %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  cor()

ggcorrplot::ggcorrplot(correlacion_retornos_post)
