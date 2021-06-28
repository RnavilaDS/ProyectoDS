###Proyecto Diplomado DS

# Carga Base Excel --------------------------------------------------------

getwd()


library(tidyverse)
library(readxl)
datos <- read_excel("BD Proyecto Semanal 26.06.21.xlsx", sheet = "Datos",
col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 3)

view(datos)

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
                                                                             

View(datos)
View(retornos)
head(datos)
str(datos)
glimpse(datos)
names(datos)
names(datos)[1] = "FECHA" ##Renombrar nombre de primera columna
names(datos)
library(skimr)
skim(datos)
min(datos$IPSA)

correlacion <- retornos %>% 
  select_if(is.numeric) %>% 
  cor()

correlacion
ggcorrplot::ggcorrplot(correlacion)

##naniar
##nombre_base %>% remove_empty(which=c('rows','cols'))
##Funciones de conteos de NA
##n_miss(): Total de NA en un data frame o columna.
##n_complete(): Número de valores completos.
##prop_miss()/pct_miss(): Propoción o porcentaje de valores NA
##miss_var_summary(): Tabla de resumen de los NA por variable.
##miss_case_table(): Tabla de resumen de los NA por 
library(ggcorrplot)

str(mtcars)

mtcars %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  round(2)

# asignamos la matriz de correlación al objeto correlación
correlacion <- mtcars %>% 
  select_if(is.numeric) %>% 
  cor()

ggcorrplot::ggcorrplot(correlacion)

## Matriz de dispersión y correlación
# usamos library(GGally)
ggpairs(mtcars)  


## otra función para calcular la correlación de a pares
library(corrr)

mtcars %>% 
  select_if(is.numeric) %>% 
  correlate(method = "pearson") %>%
  stretch(remove.dups = TRUE)%>% 
  filter(r > 0.7) 

install.packages("propagate")
library(propagate)

fitDistr(retornos$RIPLEY)
boxplot(retornos$RIPLEY)
boxplot.stats(retornos$RIPLEY)
View(retornos$RIPLEY)

retornos <- mutate(retornos, ripley_2 = retornos$RIPLEY*100)
fitDistr(retornos$ripley_2)
View(retornos$ripley_2)

boxplot(retornos$ripley_2)
boxplot.stats(retornos$ripley_2)


