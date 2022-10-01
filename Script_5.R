#############################################
##### Visualización de Datos en R ###########
##### Sesión 5 - Septiembre 29 de 2020 ######
#############################################

###############################################################
### Agenda:                                ####################
###############################################################
### 1. Modificación de colores usando las funciones scale_*  ##
### 2. Modificación escala numérica                          ##
### 3. Agregar texto a gráficas                              ##
### 4. Elaboración de gráficas de manera separada por grupos ##
### 5. Personalización avanzada de gráficas                  ##
###############################################################

#############################################

# Activar paquetes
# (SE DEBE HACER CADA VEZ QUE SE INICIA R)
library(tidyverse)
# install.packages("scales")
library(scales)
library(lubridate)
# install.packages("ggrepel")
library(ggrepel)


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



#############################################################
## 1. Modificación de colores usando las funciones scale_* 
#############################################################

# La función scale sirve para cambiar los colores también. 
# El cambio de colores se puede hacer manualmente, o de acuerdo con una paleta de colores.
# Pueden encontrar más información acá: 
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually

# Manualmente

ggplot(data = saber11_2019, aes(x = FAMI_ESTRATOVIVIENDA, fill = DESEMP_INGLES)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("Red", "Blue", "Green", "Yellow", "Pink"))

# De acuerdo con una paleta de colores

ggplot(data = saber11_2019,  mapping = aes(x = FAMI_ESTRATOVIVIENDA, fill = DESEMP_INGLES)) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Oranges")

ggplot(data = saber11_2019,  mapping = aes(x = FAMI_ESTRATOVIVIENDA, fill = DESEMP_INGLES)) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Oranges", direction = -1)

# Para variables cuantitativas:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_INGLES, y = PUNT_GLOBAL, 
                                          color = ESTU_INSE_INDIVIDUAL)) + 
  geom_jitter() + 
  scale_color_gradient(low = "Red", high = "Blue")

?scale_color_gradientn


punto_medio <- mean(saber11_2019$ESTU_INSE_INDIVIDUAL, na.rm = TRUE)

ggplot(data = saber11_2019, mapping = aes(x = PUNT_INGLES, y = PUNT_GLOBAL, 
                                          color = ESTU_INSE_INDIVIDUAL)) + 
  geom_jitter() + 
  scale_color_gradient2(midpoint = punto_medio,
                        low = "Red", mid = "White", high = "Blue")


ggplot(data = saber11_2019, mapping = aes(x = PUNT_INGLES, y = PUNT_GLOBAL, 
                                          color = ESTU_INSE_INDIVIDUAL)) + 
  geom_jitter() + 
  scale_color_gradientn(colours = rainbow(5))

# 2. Modificaciones a escalas numéricas

# Transformar datos

head(covid_colombia)

# Shift + Control + M

casos_diarios <- covid_colombia %>% 
  group_by(`fecha reporte web`) %>% 
  summarize(n = n()) %>% 
  mutate(acumulados = cumsum(n))

# Gráfica básica

ggplot(data = casos_diarios, mapping = aes(x = `fecha reporte web`, y = acumulados)) + 
  geom_line() + 
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","))

# ¿Cómo podríamos mejorar esta gráfica?

?scale_y_log10

ggplot(data = casos_diarios, mapping = aes(x = `fecha reporte web`, y = acumulados)) + 
  geom_line() + 
  scale_y_log10(label = label_number(big.mark = ".", decimal.mark = ","))

ggplot(data = casos_diarios, mapping = aes(x = `fecha reporte web`, y = acumulados)) + 
  geom_line() + 
  scale_y_log10(label = label_number_si(big.mark = ".", decimal.mark = ","))

ggplot(data = casos_diarios, mapping = aes(x = `fecha reporte web`, y = acumulados)) + 
  geom_line() + 
  scale_y_continuous(trans = "log10", label = label_number_si(big.mark = ".", decimal.mark = ","))

ggplot(data = casos_diarios, mapping = aes(x = `fecha reporte web`, y = acumulados)) + 
  geom_line() + 
  coord_trans(y = "log10") + 
  scale_y_continuous(label = label_number_si())

# Revisen la ayuda de scale_y_continuous para ver otras funciones para hacer 
# transformaciones comunes, y el argumento trans para ver otras transformaciones posibles

# 3. Agregar texto a gráficas:

# Si no depende de una variable:

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                     y = PUNT_INGLES)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_text(label = "Línea de Tendencia", x = 70, y = 85)

ggplot(data = saber11_2019,
       mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                     y = PUNT_INGLES)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_text(label = "Línea de Tendencia", x = 70, y = 85, 
            size = 3, colour = "Darkgreen", hjust = 0.1)

vignette("ggplot2-specs")

# Si depende de una variable:

# Preparación de datos

deptos <- saber11_2019 %>% 
  group_by(ESTU_DEPTO_RESIDE) %>% 
  summarize(across(.cols = c(PUNT_GLOBAL, ESTU_INSE_INDIVIDUAL), 
                   .fns = mean, 
                   na.rm = TRUE))

ggplot(data = deptos, mapping = aes(x = ESTU_INSE_INDIVIDUAL, y = PUNT_GLOBAL)) + 
  geom_point()

ggplot(data = deptos, mapping = aes(x = ESTU_INSE_INDIVIDUAL, y = PUNT_GLOBAL, 
                                    label = ESTU_DEPTO_RESIDE)) + 
  geom_text()

ggplot(data = deptos, mapping = aes(x = ESTU_INSE_INDIVIDUAL, y = PUNT_GLOBAL, 
                                    label = ESTU_DEPTO_RESIDE)) + 
  geom_text() + 
  geom_point()

ggplot(data = deptos, mapping = aes(x = ESTU_INSE_INDIVIDUAL, y = PUNT_GLOBAL, 
                                    label = ESTU_DEPTO_RESIDE)) + 
  geom_label() + 
  geom_point()

# El paquete ggrepel contiene un par de funciones que evitan el traslape:

ggplot(data = deptos, mapping = aes(x = ESTU_INSE_INDIVIDUAL, y = PUNT_GLOBAL, 
                                    label = ESTU_DEPTO_RESIDE)) + 
  geom_text_repel() + 
  geom_point()

ggplot(data = deptos, mapping = aes(x = ESTU_INSE_INDIVIDUAL, y = PUNT_GLOBAL, 
                                    label = ESTU_DEPTO_RESIDE)) + 
  geom_label_repel() + 
  geom_point()

### Agregar etiquetas a una diagrama de barras:


ggplot(data = saber11_2019, mapping = aes(x = ESTU_GENERO)) +
  geom_bar() + 
  scale_x_discrete(na.translate = FALSE) + 
  geom_text(aes(label = percent(..count../sum(..count..))), 
            stat = "count", position = position_stack(0.5), 
            color = "White")



## 4. Elaboración de gráficas de manera separada para grupos conformados 
# por una o más variables

# Hay dos opciones para esto: facet_wrap o facet_grid

# Con una variable:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  facet_wrap(vars(FAMI_ESTRATOVIVIENDA))

# Con dos variables:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  facet_wrap(vars(COLE_NATURALEZA, COLE_AREA_UBICACION))

# Usando facet_grid
ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  facet_grid(rows = vars(COLE_NATURALEZA, COLE_AREA_UBICACION))


ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  facet_grid(rows = vars(FAMI_ESTRATOVIVIENDA), 
             cols = vars(COLE_AREA_UBICACION))

ggplot(data = saber11_2019, mapping = aes(x = PUNT_INGLES, y = PUNT_GLOBAL)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(vars(COLE_AREA_UBICACION))

# 5. Personalización avanzada de gráficas:

# Para cambiar cualquier aspecto particular de una gráfica que no tenga que ver 
# con los datos se debe agregar la capa theme(). Esta capa tiene un amplio 
# número de argumentos que contiene aspectos que se pueden modificar:

?theme

### Cambiar la posición de la leyenda con el tipo de colegio

# Partamos de esta gráfica:


ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "En los colegios privados tiende a haber mejor desempeño y mejor nivel
       socioeconómico",
       x = "Nivel Socioeconómico",
       y = "Puntaje de Inglés", color = "Tipo de Colegio")

# Por defecto la leyenda del atributo color es colocada en el lado izquierdo. 
# Para modificar esta posición, se usaría la capa theme() con el argumento 
# legend.position, así:

ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "En los colegios privados
       tiende a haber mejor desempeño y mejor nivel
       socioeconómico",
       x = "Nivel Socioeconómico",
       y = "Puntaje de Inglés", color = "Tipo de Colegio") +
  theme(legend.position = "bottom")

ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "En los colegios privados
       tiende a haber mejor desempeño y mejor nivel
       socioeconómico",
       x = "Nivel Socioeconómico",
       y = "Puntaje de Inglés", color = "Tipo de Colegio") +
  theme(legend.position = "top")

# Para conocer que otros aspectos se pueden cambiar en la leyenda se puede 
# volver a explorar la ayuda de theme(). Ahí, por ejemplo, se observa que los
# valores posibles de este argumento son: "none", "left", "right", "bottom", "top". 

### Modificar texto

# Para modificar el tipo de letra de todo el gráfico se usa el argumento text 
# dentro de la capa theme():

ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "Al aumentar el nivel socioeconómico el puntaje de inglés tiende a aumentar ",
       subtitle = "Los colegios privados tienden a tener mayores puntajes y mejor nivel socioeconómico",
       x = "Nivel Socioeconómico", y = "Punta de Inglés",
       color = "Tipo de Colegio") +
  theme(text = element_text(family = "serif", face = "bold"))

# Para ver información sobre fuentes vea: http://www.cookbook-r.com/Graphs/Fonts/ o busquen la viñeta

vignette("ggplot2-specs")

# Para cambiar solamente el texto de un elemento en particular, tengo que 
# señalar explícitamente ese argumento:

ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "Al aumentar el nivel socioeconómico el puntaje de inglés aumenta",
       subtitle = "Los colegios privados tienden a tener mayores puntajes y mejor nivel socioeconómico",
       x = "Nivel Socioeconómico", y = "Punta de Inglés",
       color = "Tipo de Colegio") +
  theme(text = element_text(family = "serif"), 
        plot.title = element_text(face = "bold"))

# Cambiar el angulo de las etiquetas de los ejes:

ggplot(data = casos_diarios, mapping = aes(x = `fecha reporte web`, y = acumulados)) + 
  geom_line() + 
  coord_trans(y = "log10") + 
  scale_y_continuous(label = label_number_si()) + 
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%d %b") + 
  theme(axis.text.x = element_text(angle = 90) )

?strftime

# Para quitar el fondo de la gráfica se puede usar el siguiente código:


ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "Al aumentar el nivel socioeconómico el puntaje de inglés tiende a aumentar ",
       subtitle = "Los colegios privados tienden a tener mayores puntajes y mejor nivel socioeconómico",
       x = "Nivel Socioeconómico", 
       y = "Punta de Inglés",
       color = "Tipo de Colegio") +
  theme(text = element_text(family = "serif"), 
        plot.title = element_text(face = "bold", hjust = 0.5), 
        panel.grid = element_blank(),
        panel.background = element_blank())

# O para cambiarlo:

ggplot(data = saber11_2019, mapping = aes(x = ESTU_INSE_INDIVIDUAL,
                                          y = PUNT_INGLES,
                                          color = COLE_NATURALEZA)) +
  geom_jitter() +
  labs(title = "Al aumentar el nivel socioeconómico el puntaje de inglés tiende a aumentar ",
       subtitle = "Los colegios privados tienden a tener mayores puntajes y mejor nivel socioeconómico",
       x = "Nivel Socioeconómico", 
       y = "Punta de Inglés",
       color = "Tipo de Colegio") +
  theme(text = element_text(family = "serif"), 
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightcyan"))

# Algunas funciones relacionadas a `theme()` son:

# theme_get() para conocer las características actuales. 
# theme_set() para crear un nuevo theme. 
# theme_update() para cambiar alguna característica del theme actual.




