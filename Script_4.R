#############################################
##### Visualización de Datos en R ###########
##### Sesión 4 - Septiembre 22 de 2020 ######
#############################################

##############################################################################
### Agenda:                                ###################################
##############################################################################
### 1. Gráficas de funciones matemáticas                     ##
### 2. Modificación de etiquetas y escalas de ejes           ##
### 3. Elaboración de gráficas de manera separada por grupos ##
###############################################################

#############################################

# Activar paquetes
# (SE DEBE HACER CADA VEZ QUE SE INICIA R)
library(tidyverse)
#install.packages("scales")
library(scales)
library(lubridate)


##############################################
### Fijar directorio de trabajo ##############
##############################################

getwd()

# (SE RECOMIENDA HACER CADA VEZ QUE SE INICIA R)

# Control + Shift + H

setwd("C:/Users/sanal/OneDrive - Universidad Externado de Colombia/Externado/Cursos virtuales R/Visualización")

getwd()

##############################################
### Importar datos ###########################
##############################################

### 1. Prueba Saber11 2019-2

saber11_2019 <- read_csv("saber11_2019.csv")

### 2. Datos Covid - 19 Colombia
covid_colombia <- read_csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD")

### 3. Datos de la trm en Colombia:
trm <- read_csv("https://www.datos.gov.co/api/views/mcec-87by/rows.csv?accessType=DOWNLOAD")

##############################################
### Gráficos de funciones matemáticas ########
##############################################

# Con funciones de R:
# Ej. La función dnorm permite graficar la función de densidad de la distribución normal

# Para el caso de la distribución normal estándar:
ggplot() + xlim(-4, 4) + 
  geom_function(fun = dnorm)

# Para el caso de la otras distribuciones normales. 
# Ej. Una con media 100 y desviación 10

ggplot() + xlim(60, 140) +
  geom_function(fun = dnorm, 
                args = list(mean = 100, sd = 10))


# Ej: Distribución t con 5, 20 y 100 grados de libertad

ggplot() + xlim(-4, 4) +
  geom_function(fun = dt, 
                args = list(df = 5)) + 
  geom_function(fun = dt, 
                args = list(df = 20), 
                color = "Red") + 
  geom_function(fun = dt, 
                args = list(df = 100), 
                color = "Blue")

ggplot() + xlim(-4, 4) +
  geom_function(fun = dt, 
                args = list(df = 5)) + 
  geom_function(fun = dt, 
                args = list(df = 20), 
                color = "Red") + 
  geom_function(fun = dt, 
                args = list(df = 100), 
                color = "Blue") +
  geom_function(fun = dnorm, 
                color = "Green")

# Ej. Gráfica de una función creada en R

# Creo la función
mi_funcion <- function(x){
  2*x + 10
}

mi_funcion(-2)
mi_funcion(c(10, 20, -2))

# Grafico la función

ggplot() + xlim(-1000,1000) +
  geom_function(fun = mi_funcion)

# También podría crearla dentro de ggplot:

ggplot() + xlim(-10, 10) +
  geom_function(fun = function(x){-x + 8})

# Otro ejemplo:

mi_segunda_funcion <- function(x){
  -x^2 + 2*x -10
}

ggplot() + xlim(-100, 100) +
  geom_function(fun = mi_segunda_funcion)  


####################################################
## 2. Modificación de etiquetas y escalas de ejes ##
####################################################

## Las funciones scale_?_? permiten modificar los puntos
## de corte y las etiquetas de los ejes, así como el rango de
## de los ejes

## ¿Cuál función usar?

## Para variables categóricas:

# Ejemplo 1:

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_GENERO)) +
  geom_bar() 

# Para cambiar las categorías en el eje x:

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_GENERO)) +
  geom_bar() +
  scale_x_discrete(labels = c("Femenino", "Masculino"))

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_GENERO)) +
  geom_bar() +
  scale_x_discrete(labels = c("Femenino", "Masculino"), 
                   na.translate = FALSE)

# Ejemplo 2

ggplot(data = saber11_2019,
       mapping = aes(x = FAMI_ESTRATOVIVIENDA)) +
  geom_bar() 

# Para quitar los valores faltantes

ggplot(data = saber11_2019,
       mapping = aes(x = FAMI_ESTRATOVIVIENDA)) +
  geom_bar() + 
  scale_x_discrete(na.translate = FALSE)

# Ejemplo 3

ggplot(data = saber11_2019, 
       mapping = aes(x = PUNT_GLOBAL, 
                     color = ESTU_GENERO)) +
  geom_density()

# Para modificar las categorías de la leyenda y quitar los valores faltantes

ggplot(data = saber11_2019, 
       mapping = aes(x = PUNT_GLOBAL, 
                     color = ESTU_GENERO)) +
  geom_density()+
  scale_color_discrete(labels = c("Femenino", "Masculino"), 
                       na.translate = FALSE) +
  labs(x = "Puntaje Global", 
       y = "Densidad", 
       title = "Densidad del puntaje por género", 
       color = "Género del Estudiante")

ggplot(data = saber11_2019, 
       mapping = aes(x = PUNT_GLOBAL, 
                     fill = ESTU_GENERO)) +
  geom_density()+
  scale_fill_discrete(labels = c("Femenino", "Masculino"), 
                      na.translate = FALSE) +
  labs(x = "Puntaje Global", 
       y = "Densidad", 
       title = "Densidad del puntaje por género", 
       fill = "Género del Estudiante")

# ¿Cómo se podría cambiar el título de la leyenda?

## Para variables cuantitativas

# Ejemplo 4

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                     y = PUNT_INGLES)) +
  geom_point() 

# Para cambiar los puntos de corte:

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                     y = PUNT_INGLES)) +
  geom_point() +
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 5)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80, 90, 100))

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                     y = PUNT_INGLES)) +
  geom_point() +
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 5)) +
  scale_y_continuous(breaks = c(32, 45, 51, 69, 78, 84, 90, 100))

# Para enfocarse en una parte de la gráfica:

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                     y = PUNT_INGLES)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 5), 
                     limits = c(60, 80)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80, 90, 100), 
                     limits = c(60, 80))

# Ejemplo 5

ggplot(data = saber11_2019, mapping = aes(x = FAMI_ESTRATOVIVIENDA, 
                                          y = stat(prop), 
                                          group = 1)) + 
  geom_bar()

# Para cambiar etiquetas (i.e. formato) del eje y (porcentaje):
# y quitar los valores faltantes
# y abreviar las etiquetas del eje x

ggplot(data = saber11_2019, mapping = aes(x = FAMI_ESTRATOVIVIENDA, 
                                          y = stat(prop), 
                                          group = 1)) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  scale_x_discrete(na.translate = FALSE, 
                   labels = abbreviate)

# Ejemplo 6:

ggplot(data = covid_colombia, mapping = aes(x = Estado)) + 
  geom_bar()

# Para quitar la notación científica:

# Alternativa 1

ggplot(data = covid_colombia, mapping = aes(x = Estado)) + 
  geom_bar() + 
  scale_y_continuous(labels = number)

# Alternativa 2

ggplot(data = covid_colombia, mapping = aes(x = Estado)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma)

# Alternativa 3

ggplot(data = covid_colombia, mapping = aes(x = Estado, 
                                            y = ..count../1000)) + 
  geom_bar() + 
  scale_y_continuous(labels = number) + 
  labs(y = "# Casos en miles")

# Alternativa 4

options(scipen = 999)

ggplot(data = covid_colombia, mapping = aes(x = Estado)) + 
  geom_bar()

# Alternativa 5
ggplot(data = covid_colombia, mapping = aes(x = Estado)) + 
  geom_bar() + 
  scale_y_continuous(labels = label_number_si())


## Para fechas

# Transformar la variable VIGENCIAHASTA a fecha:
trm <- trm %>% 
  mutate(VIGENCIAHASTA = dmy(VIGENCIAHASTA))
class(trm$VIGENCIAHASTA)

# Ejemplo 7:

ggplot(data = trm, mapping = aes(x = VIGENCIAHASTA, y = VALOR)) + 
  geom_line()

# Para cambiar las etiquetas de la fecha y del valor:

ggplot(data = trm, mapping = aes(x = VIGENCIAHASTA, y = VALOR)) + 
  geom_line() +
  scale_y_continuous(labels = dollar) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y %B")
?strptime

# Enfocarse en este año:

ggplot(data = trm, mapping = aes(x = VIGENCIAHASTA, y = VALOR)) + 
  geom_line() +
  scale_y_continuous(labels = dollar, limits= c(3150, NA)) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%b %d", 
               limits = c(as_date("2020-01-01"),as_date("2020-09-22")))





