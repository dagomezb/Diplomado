# Se hace PCA
pca <- prcomp(breast_cancer[numericas], center= TRUE,scale = TRUE)

# Es lo mismo que
# pca <- prcomp(standard_data)

# Resumen
summary(pca)

# Datos proyectados sobre las componentes principales
pca$x

# Raiz cuadrada de los autovalores
pca$sdev

library(factoextra)
# Scree plot o diagrama de sedimentacion
fviz_eig(pca, addlabels = TRUE)

# Varianza explicativa acumulada
cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(h = 0.9, col="blue")

library(plotly)

# Visualicemos los datos usando las dos componentes principales de
# mayor variabilidad
plot_ly(
  x=pca$x[,1],
  y=pca$x[,2],
  type="scatter",
  mode="markers",
  color=breast_cancer$diagnosis,
  colors=c("red","blue")
)

# Visualicemos los datos usando las tres componentes principales de
# mayor variabilidad
plot_ly(
  x=pca$x[,1],
  y=pca$x[,2],
  z=pca$x[,3],
  type="scatter3d",
  mode="markers",
  color=breast_cancer$diagnosis,
  colors=c("red","blue")
)
