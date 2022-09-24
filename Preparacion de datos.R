cars <- read.csv("../../datasets/cars.csv")

# Cantidad de filas y columnas
dim(cars)

# Mostrar X filas del principio
head(cars,5)

# Estructura de los datos
str(cars)

# Convertir variables a tipo categorico
categoricas <- c(1,4,6,9,10)
cars[categoricas] <- lapply(cars[categoricas], factor)
str(cars)
summary(cars)
# Apply vs lapply https://www.datasciencemadesimple.com/apply-function-r/

# Contar cantidad de datos nulos por columna
colSums(is.na(cars))

# Eliminar filas que tengan por lo menos un dato nulo
cars <- na.omit(cars)

# Eliminar filas que tengan mas X datos nulos
cars <- cars[rowSums(!is.na(cars)) > ncol(cars)-2, ]

# Eliminar filas que tengan datos nulos en columnas especificas
cars <- cars[rowSums(is.na(cars[c(3,5)])) == 0, ]

# Rellenar valores nulos con un valor especifico
cars$price[is.na(cars$price)] <- mean(cars$price,na.rm=TRUE)

