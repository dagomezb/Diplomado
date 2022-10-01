library("tidyverse")

cars_original <- read.csv("../../datasets/cars.csv")

# Convertir variables a tipo categorico
categoricas <- c(1,4,6,9,10)
cars <- data.frame(cars_original)
cars[categoricas] <- lapply(cars_original[categoricas], factor)

# Eliminar filas que tengan por lo menos un dato nulo
cars <- na.omit(cars)

# Tabla de conteo cruzado
tabla_conteo <- table(cars$fuelType,cars$transmission)
tabla_conteo

# Grafica de barras por grupo
df <- data.frame(tabla_conteo)
names(df) <- c("Fuel Type", "Transmission","Frec") 
ggplot(df,aes(x = `Fuel Type`,y = Frec, fill = Transmission)) +
  geom_bar(stat = "identity",position = "dodge")

library(plotly)

# Mapa de calor de tabla de conteo cruzado
mapaDeCalor <- plot_ly(
  x=rownames(tabla_conteo), 
  y=colnames(tabla_conteo), 
  z = matrix(tabla_conteo, nrow=5, ncol=4), 
  type = "heatmap")
mapaDeCalor
# colorscales: https://plotly.com/r/builtin-colorscales/
# plotly: https://plotly.com/r/

# Grafico de dispersion o scatter plot
ggplot(cars,aes(x=price,y=mpg))+
  geom_point()

numericas = names(select_if(cars, is.numeric))

# Mapa de calor de matriz de correlaciones
correlaciones <- cor(cars[numericas])
plot_ly(x=colnames(correlaciones), 
        y=rownames(correlaciones), 
        z = correlaciones, 
        type = "heatmap",
        colorscale= "Accent")

library(lsa)
# Mapa de calor de similitud coseno
similitud_coseno <- cosine(as.matrix(cars[numericas]))
plot_ly(x=colnames(similitud_coseno), 
        y=rownames(similitud_coseno), 
        z = similitud_coseno, 
        type = "heatmap",
        colorscale= "Accent")

# Histograma por grupo
var_num <- "mileage"
var_cat <- "Make"
ggplot(cars, aes_string(x = var_num, fill = var_cat, colour = var_cat)) + 
  geom_histogram(alpha=0.5, position = "identity")

# Boxplot por grupo
ggplot(cars, aes_string(y = var_num, x = var_cat)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + 
  geom_boxplot()


#### ANALISIS MULTIVARIANTE ####


# Scatter plot con mas de 2 variables
ggplot(cars,aes(x=price,y=mpg,))+
  geom_point(alpha=0.5,aes(
    color=Make,
    shape=transmission,
    # size=mileage,
  ))

# Grafico 3D con 3 variables
plot_ly(x=cars$mileage, 
        y=cars$price, 
        z=cars$mpg, 
        type="scatter3d", 
        mode="markers"
)

# Grafico 3D con mas de 4 variables
plot_ly(x=cars$mileage, 
        y=cars$price, 
        z=cars$mpg, 
        color = cars$fuelType,
        type="scatter3d", 
        mode="markers"
)
# Reto: Scatter plot en 3D con 5 variables
