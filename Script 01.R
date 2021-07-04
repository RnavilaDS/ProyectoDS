###Proyecto Diplomado DS

# Carga Base Excel --------------------------------------------------------

getwd()
setwd()

library(tidyverse)
library(readxl)
options (scipen = 999) #Permite mostrar decimales
datos <- read_excel("BD Proyecto Semanal 26.06.21.xlsx", sheet = "Datos",
col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 3)

names(datos)[1] = "FECHA" ##Renombrar nombre de primera columna

datos <- datos %>% 
  filter(!is.na(FECHA))##Elimina primera fila de N/A
view(datos)

summary(datos, na.rm) ##Resumen estadistico

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

head(retornos)
view(retornos)


# Resumen estadístico inicio pandemia -------------------------------------

###Retornos

# Matriz de correlación de retornos ---------------------------------------


retornos_inicio_pandemia <- retornos %>% 
  filter(!is.na(Casos)) %>% 
  arrange(FECHA)
  
summary(retornos_inicio_pandemia)
##Genera matriz de correlación
correlacion_retornos <- retornos_inicio_pandemia %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor()

correlacion_retornos
ggcorrplot::ggcorrplot(correlacion_retornos)


# Gráfico de retornos -----------------------------------------------------



library(tidyverse)
retornos_ini_nemo <- gather(data = retornos_inicio_pandemia, key = "NEMO", value = "RETORNO", 2:37) 

mutate(retornos_ini_nemo, RETORNO = RETORNO*100)

view(retornos_ini_nemo)
ggplot(retornos_ini_nemo, aes(x = FECHA, y= RETORNO , color = NEMO)) +
  geom_line()

plot_retorno_grid <- ggplot(retornos_ini_nemo, aes(x = FECHA, y= RETORNO , color = NEMO)) +
  geom_boxplot()+
  facet_grid(~ NEMO)


# Análisis de Precios -----------------------------------------------------

precios_inicio_pandemia <- datos %>% 
  filter(!is.na(Casos)) %>% 
  arrange(FECHA)

summary(precios_inicio_pandemia)
##Genera matriz de correlación
correlacion_precios <- precios_inicio_pandemia %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor()

correlacion_precios
ggcorrplot::ggcorrplot(correlacion_precios)



# Graficos de precios -----------------------------------------------------

precios_ini_nemo <- gather(data = precios_inicio_pandemia, key = "NEMO", value = "RETORNO", 2:37) 
view(precios_ini_nemo)
precios_ini_nemo <- filter(precios_ini_nemo, !(NEMO == "Dosis" | NEMO == "Dosis_Completa"
       | NEMO == "Recuperados" | NEMO == "Muertes" | NEMO == "Casos"))

ggplot(precios_ini_nemo, aes(x = FECHA, y= RETORNO , color = NEMO)) +
  geom_line()

plot_precios_grid <- ggplot(precios_ini_nemo, aes(x = FECHA, y= RETORNO , color = NEMO)) +
  geom_boxplot()+
  facet_grid(~ NEMO)


# Gráfico precios y retornos ----------------------------------------------

gridExtra::grid.arrange(plot_precios_grid,plot_retorno_grid, nrow=2)













datos %>% 
  summarise(
            mean(IPSA)
            ,var(IPSA)
            )

# boxplot -----------------------------------------------------------------

boxplot( datos$VIX ~ datos$IPSA)   
boxplot(datos$RIPLEY) 
boxplot(datos$ECL)

install.packages("psych") #Instala package para hacer análisis de correlación de variables
library(psych)
pairs(datos)


# corrplot ----------------------------------------------------------------
install.packages("corrplot")
library(corrplot)
head(datos)
select (datos, -FECHA)

datos_cor <- cor(select (datos, -FECHA) , method = "pearson")

datos_cor
corrplot(datos_cor)

ggpairs(datos)  

# ggcorr ------------------------------------------------------------------

correlacion <- datos %>% 
  select_if(is.numeric) %>% 
  cor()
correlacion
ggcorrplot::ggcorrplot(correlacion)

library(tidyverse)
cor_test_01 <- retornos %>% 
  select(IPSA,RIPLEY) %>% 
mutate(IPSA = IPSA*100,
       RIPLEY = RIPLEY*100) %>% 
  na.omit() %>% 
  cor()

cor_test_01
  
retornos
correlacion_retornos <- retornos %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor()

correlacion_retornos
ggcorrplot::ggcorrplot(correlacion_retornos)

View(datos)
View(retornos)
head(datos)
str(datos)
glimpse(datos)
names(datos)

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


