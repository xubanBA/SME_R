


#######################################################
#######     Correlación e Informacion Mutua     #######
#######################################################


visualizar_matriz_correlacion <- function(datos) {
  #' Visualiza la matriz de correlación para un dataframe.
  #' Se hace uso de función calcular_correlacion() para obtener los datos y asi poder realizar el plot.
  #'
  #' @param datos Un DataFrame que contiene las variables para las cuales se calculará la correlacion.
  
  # Calcula la matriz de correlaciones
  matriz <- calcular_correlacion(datos) 
  matriz <-  as.matrix(matriz[nrow(matriz):1, ]) # Hace falta que sea matriz para usar image()
  # Ademas, invertir las filas porque sino salia al reves
  matriz <- t(apply(matriz, 2, c)) # Transpuesta porque salia al reves
  
  # Configura el tamaño de la figura
  par(mar=c(5, 4, 2, 2)) # Configura los márgenes
  
  # Crea un mapa de calor para visualizar la matriz de correlaciones
  image(1:nrow(matriz), 1:ncol(matriz), matriz, col=heat.colors(256), axes=FALSE)
  
  # Etiquetas de los ejes
  num_variables <- ncol(matriz)
  axis(1, at=1:num_variables, labels=colnames(matriz), las=2)
  axis(2, at=1:num_variables, labels=rev(colnames(matriz)), las=2)
  
  # Muestra el valor de correlación en cada celda
  for (i in 1:num_variables) {
    for (j in 1:num_variables) {
      text(i, j, round(matriz[i, j], 2), col="black", cex=1.5)
    }
  }
  
  # Título y etiquetas de los ejes
  title("Matriz de Correlación")
  mtext("Variables", side=1, line=3)  # Etiqueta del eje X
  mtext("Variables", side=2, line=3)  # Etiqueta del eje Y
  
  # Muestra la matriz de correlaciones
  box()
}



visualizar_infmutua <- function(datos) {
  #' Visualiza la matriz de información mutua para un dataframe.
  #' Se hace uso de función calcular_info_mutua() para obtener los datos y asi poder realizar el plot.
  #'
  #' @param datos Un DataFrame que contiene las variables para las cuales se calculará la información mutua.
  
  # Calcula la matriz de correlaciones
  matriz <- calcular_info_mutua(datos)
  matriz <-  as.matrix(matriz[nrow(matriz):1, ])
  matriz <- t(apply(matriz, 2, c)) # Transpuesta porque salia al reves
  
  
  # Configura el tamaño de la figura
  par(mar=c(5, 4, 2, 2)) # Configura los márgenes
  
  # Crea un mapa de calor para visualizar la matriz de correlaciones
  image(1:nrow(matriz), 1:ncol(matriz), matriz, col=heat.colors(256), axes=FALSE)
  
  # Etiquetas de los ejes
  num_variables <- ncol(matriz)
  axis(1, at=1:num_variables, labels=colnames(matriz), las=2)
  axis(2, at=1:num_variables, labels=rev(colnames(matriz)), las=2)
  
  # Muestra el valor de correlación en cada celda
  for (i in 1:num_variables) {
    for (j in 1:num_variables) {
      text(i, j, round(matriz[i, j], 2), col="black", cex=1.5)
    }
  }
  
  
  # Título y etiquetas de los ejes
  title("Matriz de Correlación")
  xlabel <- expression("Variables")
  ylabel <- expression("Variables")
  mtext(side=1, line=3, xlabel)
  mtext(side=2, line=3, ylabel)
  
  # Muestra la matriz de correlaciones
  box()
}



###################################
#######     Curva AUC-ROC   #######
###################################

visualizar_roc <- function(tpr_lisr, fpr_list) {
  #' Visualiza una curva AUC-ROC utilizando las tasas de falsos positivos (FPR) y verdaderos positivos (TPR) proporcionadas.
  #'
  #' @param fpr_list Lista de tasas de falsos positivos.
  #' @param tpr_list Lista de tasas de verdaderos positivos.
  #' Los dos argumentos deben de tener la misma longitud
  
  # Pasar a vector
  res <- unlist(res) 
  tpr_lisr <- unlist(tpr_lisr) 
  fpr_list <- unlist(fpr_list)
  
  # Crear un gráfico de la curva ROC
  plot(fpr_list, tpr_lisr, type = "l", col = "blue", lwd = 2, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Tasa de Falsos Positivos (FPR)", ylab = "Tasa de Verdaderos Positivos (TPR)",
       main = paste("Curva ROC (AUC)"))
  
  # Línea de referencia (diagonal)
  abline(0, 1, col = "gray", lty = 2)
  
  # Leyenda
  
}