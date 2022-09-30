library("dplyr")
library("tidyverse")

# Seleccion de nombres de variables numericas
numericas = names(select_if(cars, is.numeric))

# Resumen estadistico de variables
summary(cars[numericas])

library("ggplot2")

# Histograma y boxplot de las variables
for (columna in numericas) {
  hist <- ggplot(cars,aes_string(x=columna))+
    geom_histogram(bins=20)
  print(hist)
  
 box <- ggplot(cars,aes_string(y=columna))+
    stat_boxplot(geom = "errorbar",
                 width = 0.15) + 
    geom_boxplot()+
    xlab(columna)+
    ylab("")
  print(box)
}

library(moments)

# Asimetria y apuntamiento de las variables
apply(cars[numericas], 2, skewness)

apply(cars[numericas], 2, kurtosis)

#### Análisis Univariante Categórico ####

library("dplyr")

# Seleccion de nombres de variables categoricas
categoricas <- names(select_if(cars, is.factor))

# Resumen estadistico de variables
summary(cars[categoricas])

# Tablas de frecuencia de las variables
for (columna in c("model","engineSize")) {
  print(columna)
  print(sort(table(cars[columna])))
  print("--------------")
}

library("ggplot2")

# Graficas de barras de las variables
filtro <- 10
for (columna in categoricas) {
  df <- data.frame(table(cars[columna]))
  df <- df[order(-df[,2]),]
  if (nrow(df)>filtro) {
    df <- df[1:filtro,]
  }
  plot <- ggplot(df,aes(x=Var1,y=Freq))+
    theme(axis.text.x=element_text(angle=45)) +
    geom_bar(stat = "identity") +
    xlab(columna)
  print(plot)
}
# Reto: ordenar las barras https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/

