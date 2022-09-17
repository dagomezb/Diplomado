#############################################
##### Modelos estadísticos para la toma de decisiones ###########
##### Sesión 1 - Septiembre 17 de 2022 #######
#############################################

#############################################
### Agenda:##################################
#############################################
### 1. Introducción a R y RStudio          ##
### 2. Introducción al trabajo con scripts ##
### 3. Importar datos a R                  ##
### 4. Estructuras de programación         ##
### 4. Introducción a visualización        ##
#############################################

#############################################
### Recursos de interés:#####################
#############################################
### https://www.r-graph-gallery.com/ ########
### https://es.r4ds.hadley.nz/ ##############
### https://ggplot2-book.org/ ###############
#############################################

#############################################
### Instalar y activar paquetes (librerías) ##

# Instalar paquetes 
# (Solamente es necesario hacerlo una vez)

install.packages("tidyverse")

# https://www.tidyverse.org/

# Activar paquetes
# (SE DEBE HACER CADA VEZ QUE SE INICIA R)
library(tidyverse)

##############################################
### Fijar directorio de trabajo ##############
##############################################

getwd()

# (SE RECOMIENDA HACER CADA VEZ QUE SE INICIA R)

# Control + Shift + H

setwd("~/OneDrive - Universidad EIA/Diplomado Analitica")


getwd()

# Para ejecutar código en un Script
# Control + Enter

##############################################
### Importar datos ###########################
##############################################
### 1. Prueba Saber11 2019-2

#saber11_2019 <- read_csv("saber11_2019.csv")
saber11_2019 <- read_csv("saber11_2019.csv")

### 2. Datos Covid - 19 Colombia
covid_colombia <- read_csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD")

##############################################
###    Tipos de datos  #######################
##############################################

# Cambiar mi_numérico para que sea 42
numerico <- 42

# Cambia mi_carácter para que sea "universo"
caracter <- "universo"

# Cambia mi_lógico para que sea FALSE
logico <- FALSE

# Clase de numerico
class(numerico)

# clase de caracter
# ...

# clase de logico
# ...

##############################################
###       Vectores     #######################
##############################################

numeric_vector <- c(1, 10, 49)
character_vector <- c("a", "b", "c")

# Complete el codigo para un boolean_vector
# ...

# Ganancias de póker y ruleta de lunes a viernes:
poker_vector <- c(140, -50, 20, -120, 240)
roulette_vector <- c(-24, -50, 100, -350, 10)
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- days_vector
names(roulette_vector) <- days_vector

# Calcular las ganancias totales del póquer y la ruleta
total_poker <- sum(poker_vector)
total_roulette <- sum(roulette_vector)

# Comprueba si has obtenido mayores ganancias totales en el póker que en la ruleta
total_poker > total_roulette

# Definir una nueva variable basada en una selección
poker_wednesday <- poker_vector[3]
poker_midweek <- poker_vector[c(2,3,4)]
roulette_selection_vector <- roulette_vector[2:5]
poker_start <- poker_vector[c("Monday","Tuesday", "Wednesday")]

# Selección por comparación - ¿Qué días ganaste dinero en el póker?
selection_vector <- poker_vector > 0
poker_winning_days <- poker_vector[selection_vector]

##############################################
###     Matrices       #######################
##############################################

# Construye una matriz con 3 filas que contengan los números del 1 al 9
matrix(1:9, byrow = TRUE, nrow = 3)

# Taquilla Star Wars (en millones)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# Crear box_office
box_office <- c(new_hope, empire_strikes, return_jedi)

# Construir star_wars_matrix
star_wars_matrix <- matrix(box_office, byrow = TRUE, nrow = 3)

# Vectores región y títulos, utilizados para nombrar
region <- c("US", "non-US")
titles <- c("Una nueva esperanza", "El imperio contraataca", "El retorno del Jedi")

# Nombrar las columnas con la región
colnames(star_wars_matrix) <- region

# Nombra las filas con los títulos
rownames(star_wars_matrix) <- titles

# Imprime la matriz
star_wars_matrix

star_wars_matrix <- matrix(box_office, 
                           nrow = 3, byrow = TRUE,
                           dimnames = list(titles, region))

# Calcula las cifras de la taquilla mundial
worldwide_vector <- rowSums(star_wars_matrix)

# Ingresos totales de EE.UU. y de fuera de EE.UU.
# ...

# Selecciona los ingresos no estadounidenses de todas las películas
non_us_all <- star_wars_matrix[,2]

# Media de los ingresos no estadounidenses
mean(non_us_all)

# Seleccionar los ingresos no estadounidenses de las dos primeras películas
non_us_some <- star_wars_matrix[1:2,2]

# Media de los ingresos no estadounidenses de las dos primeras películas
# ...

### Ejercicio - Probar aritmética con matrices.
#...

### Factor ###

# Sex vector
sex_vector <- c("Male", "Female", "Female", "Male", "Male")
class(sex_vector)

# Convertir sex_vector a factor
factor_sex_vector <- factor(sex_vector)

# Print out factor_sex_vector
factor_sex_vector

# Animals
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
factor_animals_vector <- factor(animals_vector)
factor_animals_vector

# Temperature
temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector

survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)
levels(factor_survey_vector)
levels(factor_survey_vector) <- c("Female", "Male")
factor_survey_vector

summary(survey_vector)

summary(factor_survey_vector)

##############################################
### Data Frames #######################
##############################################

# vectores
name <- c("Mercury", "Venus", "Earth", 
          "Mars", "Jupiter", "Saturn", 
          "Uranus", "Neptune")
type <- c("Terrestrial planet", 
          "Terrestrial planet", 
          "Terrestrial planet", 
          "Terrestrial planet", "Gas giant", 
          "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 
              11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 
              0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Create a data frame from the vectors
planetas_df <- data.frame(name, type, diameter, rotation, rings)

str(planets_df)

# Imprime el diámetro de Mercurio (fila 1, columna 3)
planetas_df[1,3]

# Imprime los datos de Marte (toda la cuarta fila)
planets_df[4,]

# Seleccionar los 5 primeros valores de la columna de diámetro
planetas_df[1:5,"diameter"]

# Selecciona la variable anillos de planets_df
rings_vector <- planetas_df$rings
planetas_df[rings_vector, ]

# Selecciona los planetas con diámetro < 1
subset(planets_df, subset = diameter < 1)

# Usar order() para crear posiciones
positions <- order(planetas_df$diameter)

# Utiliza las posiciones para ordenar planets_df
planetas_df[positions,]

##############################################
### Listas #######################
##############################################

# Vector con números de 1 a 10
mi_vector <- 1:10 

# Matriz con números de 1 a 9
mi_matriz <- matrix(1:9, ncol = 3)

# Primeros 10 elementos del marco de datos incorporado mtcars
mi_df <- mtcars[1:10,]

# Construir la lista con estos diferentes elementos:
mi_lista <- list(mi_vector, mi_matriz, mi_df)

mi_lista2 <- list(vec = mi_vector, mat = mi_matriz, df = mi_df)

mi_lista2[["vec"]]

# Print the second element of the vector representing the actors
mi_lista2[["vec"]][2]


##############################################
###      Condicionales #######################
##############################################


# Variables relacionadas con su último día de grabaciones
medium <- "LinkedIn"
num_views <- 14

# Estructura de control para el medio
if (medium == "LinkedIn") {
  print("Mostrando información de LinkedIn")
} else if (medium == "Facebook") {
  # Añadir código para imprimir la cadena correcta cuando la condición es TRUE
  print("Mostrar información de Facebook")
  
} else {
  print("Medio desconocido")
}

# Estructura de control para num_views
if (num_views > 15) {
  print("¡Eres popular!")
} else if (num_views <= 15 & num_views > 10) {
  # Añadir código para imprimir la cadena correcta cuando la condición es TRUE
  print("Tu número de vistas es medio")
} else {
  print("¡Intenta ser más visible!")
}

##############################################
####### ciclo          #######################
##############################################

speed <- 64

# ciclo while
while ( speed > 30 ) {
  print("Slow down!")
  speed <- speed - 7
}

#ciclo for
linkedin <- c(16, 9, 13, 5, 2, 17, 14)

# Versión de bucle 1
for (valor in linkedin){
  print(valor)
}

# Versión de bucle 2
for (i in 1:length(linkedin)){
  print(linkedin[i])
}

# The nyc list is already specified
nyc <- list(pop = 8405837, 
            boroughs = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"), 
            capital = FALSE)

# version 1
for (dato in nyc){
  print(dato)
}


# version 2
for(i in 1:length(nyc)){
  print(nyc[[i]])
}


##############################################
### Funciones #######################
##############################################

# Crear una función pow_two()
pow_two <- function (a){
  a*a
}


# Utiliza la función
pow_two(12)

# Crea una función sum_abs()
sum_abs <- function (a,b) {
  abs(a) + abs(b)
}


# Utiliza la función
sum_abs(-2,3)

pow_two <- function(x, print_info = TRUE) {
  y <- x ^ 2
  if (print_info){
    print(paste(x, "to the power two equals", y))
  }
  return(y)
}

pow_two(12)

##############################################
### Explorar los datos #######################
##############################################

head(saber11_2019)
tail(saber11_2019)
dim(saber11_2019)
ncol(saber11_2019)
nrow(saber11_2019)
names(saber11_2019)

head(covid_colombia)
tail(covid_colombia)
dim(covid_colombia)
ncol(covid_colombia)
nrow(covid_colombia)
names(covid_colombia)

# Algunas variables que vamos a usar
summary(saber11_2019$PUNT_GLOBAL)
summary(saber11_2019$ESTU_INSE_INDIVIDUAL)
table(saber11_2019$COLE_NATURALEZA)
table(covid_colombia$Estado)

##############################################
### Introducción a ggplot2 ###################
##############################################

# ggplot2 permite hacer gráficas por capas
# La primera capa siempre es ggplot y contiene:
#   El conjunto de datos que voy a usar. 
#   La o las variables que quiero graficar

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL))

# En la capa siguiente, indico el tipo de gráfico:
#   Las capas se agregan con el signo +
#   Por ejemplo, para hacer un diagrama de dispersión entre las variables 
#   ESTU_INSE_INDIVIDUAL y PUNT_GLOBAL:

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL)) + 
  geom_point()


# Por ejemplo, para hacer un diagrama de barras de la variable ESTU_GENERO:

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar()

# Para modificar las características de algún elemento de la gráfica, se modifica 
# la capa que crea ese elemento. P. Ej:

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL)) + 
  geom_point(color = "Red")

# Otro ejemplo

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar(fill = "Darkgreen")

# Cualquier cosa adicional se logra agregando más capas:

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL)) + 
  geom_point(color = "Red") + 
  labs(title = "Relación positiva entre NSE y puntaje", 
       x = "Índice de nivel socioeconómico", 
       y = "Puntaje total")

# Y para cambiar el aspecto general de la gráfica:
ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL)) + 
  geom_point(color = "Red") + 
  labs(title = "Relación positiva entre NSE y puntaje", 
       x = "Índice de nivel socioeconómico", 
       y = "Puntaje total") + 
  theme_minimal()

## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
## vignette("ggplot2-specs")
## https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf

