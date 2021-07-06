
# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(skimr)
library(propagate)
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
#Apertura la tabla de datos(Precios) entre antes de la pandemia y post pandemia


datos_pre_pandemia <- datos %>% 
  filter(is.na(Casos)) %>% 
  dplyr::select(nemo_seleccionado) 
  

datos_post_pandemia <- datos %>% 
  filter(!is.na(Casos)) %>% 
  dplyr::select(nemo_seleccionado)
  
#Apertura la tabla de retornos entre antes de la pandemia y post pandemia
retornos_pre_pandemia <- retornos %>% 
  filter(is.na(Casos)) %>% 
  dplyr::select(nemo_seleccionado)





retornos_post_pandemia <- retornos %>% 
  filter(!is.na(Casos)) %>% 
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
  geom_line()

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
