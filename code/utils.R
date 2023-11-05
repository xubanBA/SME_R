


######################################
#######     Discretizacion     #######
######################################

algoritmo_discretizeEW <- function(x, num_bins) {
  #' Aplica el algoritmo Equal Width (igual anchura) para obtener los puntos de corte y así
  #' poder discretizar una lista o columna numérica.
  #'
  #' @param x vector que se va a discretizar.
  #' @param num_bins Número de intervalos en los que se discretizará.
  #'
  #' @return Una lista que contiene la discretizacion categórica de la entrada en la primera
  #'         posicion y los puntos de corte en la segunda.

  
  # Empezar aplicando el algoritmo equal width para determinar los puntos de corte
  minv <- min(x)
  maxv <- max(x)
  interv <- (maxv - minv) / num_bins
  cut_points <- seq(minv + interv, maxv, by = interv)
  
  # Discretizar tomando cada valor de x para mirar en qué intervalo cae
  x_discretized <- c()
  for (val in x) {
    for (i in 1:(length(cut_points) - 1)) {
      punto <- cut_points[i]
      if (val <= punto) {
        x_discretized <- c(x_discretized, paste0("I", i))
        break
      }
      if (i == length(cut_points) - 1) {
        x_discretized <- c(x_discretized, paste0("I", num_bins))
      }
    }
  }
  return(list(x_discretized = as.factor(x_discretized), cut_points = cut_points))
}



algoritmo_discretizeEF <- function(x, num_bins) {
  #' Aplica el algoritmo Equal Frequency (igual frecuencia) para obtener los puntos de corte y así
  #' poder discretizar una lista o columna numérica.
  #'
  #' @param x vector que se va a discretizar.
  #' @param num_bins Número de intervalos en los que se discretizará.
  #'
  #' @return Una lista que contiene la discretizacion categórica de la entrada en la primera
  #'         posicion y los puntos de corte en la segunda.
  
  # Empezar aplicando el algoritmo equal frequency para determinar los puntos de corte
  interval_size <- length(x) %/% num_bins  # Calcular el tamaño de cada intervalo
  sorted_x <- sort(x)  # Ordenar el vector de entrada
  cut_points <- sorted_x[seq(interval_size, length(x), by = interval_size)]  # Sacar los puntos de corte
  
  # Discretizar tomando cada valor de x para mirar en qué intervalo cae
  x_discretized <- c()
  for (val in x) {
    for (i in 1:(length(cut_points) - 1)) {
      punto <- cut_points[i]
      if (val <= punto) {
        x_discretized <- c(x_discretized, paste0("I", i))
        break
      }
      if (i == length(cut_points) - 1) {
        x_discretized <- c(x_discretized, paste0("I", num_bins))
      }
    }
  }
  
  return(list(x_discretized = as.factor(x_discretized), cut_points = cut_points))
}


################################
#######     Métricas     #######
################################

varianza <- function(col) {
  #' Calcula la varianza de un vector cuyos elementos sean continuos.
  #'
  #' Originalmente implementado para utilizar con columnas de un dataframe, pero se puede utilizar con cualquier vector
  #'
  #' @param col Vector de números para los cuales se calculará la varianza.
  #'
  #' @return Un valor de tipo float.

  if (length(col) != 0) {
    media <- sum(col) / length(col) # Calcular la media
    varianza <- sum((col - media) ^ 2) / length(col) # Aplicar la fórmula de la varianza
    return(varianza)
  } 
}


entropia <- function(x) {
  #' Calcula la entropia de un vector cuyos elementos sean discretos
  #'
  #' Originalmente implementado para utilizar con columnas de un dataframe, pero se puede utilizar con cualquier vector
  #'
  #' @param col Vector de números para los cuales se calculará la entropia
  #'
  #' @return Un valor de tipo float.
  
  # Empezar contando las apariciones de cada elemento
  cont_val <- table(x)
  
  # Calcula las Probabilidades de Cada Valor
  probs <- cont_val / length(x)
  
  # Calcular la entropía dada las probabilidades
  return(-sum(probs * log2(probs), na.rm = TRUE) )
}


get_roc <- function(df, etiquetas) {
  #' Calcula la curva ROC para una columna continua y las etiquetas de clase proporcionadas.
  #'
  #' Originalmente implementado para utilizar con columnas de un dataframe, pero se puede utilizar con cualquier vector
  #'
  #' @param col Vector de números para los cuales se calculará la curva ROC.
  #' @param etiquetas Vector que representa de clase correspondientes a la columna continua.
  #'
  #' @return Una lista donde la primera posicion es el area debajo de la curva, la segunda los TPR y la tercera los FPR.

  # Ordenar el DataFrame
  df <- sort(df)
  
  # Inicializar listas para almacenar TPR y FPR
  tpr_list <- vector()
  fpr_list <- vector()
  
  # Determinar los puntos de la curva ROC (es decir, todos los pares TPR, FPR para cada posible valor de corte)
  for (valor_corte in df) {
    # Calcular TP, FP, TN, FN para el valor de corte dado
    TP <- sum(df >= valor_corte & etiquetas == TRUE)
    FP <- sum(df >= valor_corte & etiquetas == FALSE)
    TN <- sum(df < valor_corte & etiquetas == FALSE)
    FN <- sum(df < valor_corte & etiquetas == TRUE)
    
    # Calcular TPR y FPR para el valor de corte actual
    TPR <- TP / (TP + FN)
    FPR <- FP / (FP + TN)
    
    # Guardar TPR y FPR del valor de corte en la lista
    tpr_list <- c(tpr_list, TPR)
    fpr_list <- c(fpr_list, FPR)
  }
  
  # Calcular el área bajo la curva ROC (AUC) usando la regla del trapecio
  auc <- 0
  for (i in 2:length(tpr_list)) {
    auc <- auc + 0.5 * (fpr_list[i] - fpr_list[i - 1]) * (tpr_list[i] + tpr_list[i - 1])
  }
  
  return(list(auc = auc, tpr_list = tpr_list, fpr_list = fpr_list))
}



#######################################################
#######     Normalización y Estandarización     #######
#######################################################

aux_normalizar <- function(x) {
  #' Normaliza una lista de valores numéricos en el rango [0, 1].
  #' Para normalizar los valores se utiliza la fórmula: (val - min_valor) / (max_valor - min_valor)
  #'
  #' @param x Vector que se va a normalizar
  #'
  #' @return Un vector normalizado.
  #' @return Vector original si no se cumple (max_valor - min_valor) != 0
  
  min_valor <- min(x)
  max_valor <- max(x)
  
  if ((max_valor - min_valor) != 0) {
    return( (x - min_valor) / (max_valor - min_valor) ) # Devolver la lista normalizada
  } else {
    return(x) 
  }
}


aux_estandarizar <- function(x) {
  #' Estandariza un vector de valores numéricos para que tengan media 0 y desviación estándar 1.
  #' Para estandarizar se ha utilizado la formula:  (val - media) / des_estandar
  #'
  #' @param x Vector que se va a estandarizar
  #'
  #' @return Un vector estandarizado
  #' @return Vector original si no se cumple des_estandar != 0

  media <- mean(x)
  des_estandar <- sd(x)
  
  if (des_estandar != 0) {
    return( (x - media) / des_estandar ) # Devolver la lista normalizada
  } else {
    return(x)
  }
}


#######################################################
#######     Correlación e Informacion Mutua     #######
#######################################################

entropia_conjunta <- function(x, y) {
  #' Calcula la entropía conjunta de dos vectores.
  #' Función auxiliar para calcular la informacion mutua entre 2 variables.
  #'
  #' Al igual que la funcion entropia() utiliza la fórmula: H(X) = -sum(p(x) * log2(p(x))),  donde ahora p(x)
  #' son las probabilidades conjuntas de cada valor de una lista con la otra.
  #' 
  #' @param x Vector que representa la primera variable.
  #' @param y Vector que representa la segunda variable.
  #'
  #' @return float: Valor de entropía conjunta.
  
  # Calcular las probabilidades conjuntas
  cont_val <- table(x, y)
  
  # Calcula las Probabilidades Conjuntas de Cada Par (x, y)
  probs_conjuntas <- prop.table(cont_val)
  
  # Calcular la entropía conjunta dada las probabilidades
  probs_conjuntas[probs_conjuntas == 0] <- 1e-10 # Evitar el logaritmo de 0 agregando un pequeño valor epsilon
  return(-sum(probs_conjuntas * log2(probs_conjuntas)) )
}


info_mutua <- function(df) {
  #' Calcula la información mutua entre todas las columnas de un DataFrame.
  #' Es una función auxiliar que se usa en la función calcular_info_mutua() donde se pasa como argumento
  #' un dataframe con la columnas categoricas.
  #' 
  #' Para calcular la información mutua entre 2 variables se ha hecho uso de la función:
  #' información mutua = entropia1 + entropia2 - entropia_conjunta
  #' 
  #' @param df  DataFrame de R que contiene variables para las cuales se calculará la información mutua.
  #'
  #' @return DataFrame que contiene los valores de información mutua para cada par de columnas del DataFrame.
  #'          Los nombres de las columnas y las filas del DataFrame corresponden a las columnas del DataFrame original.
  
  # Obtener nombres de las columnas
  columnas <- colnames(df)
  
  # Inicializar una matriz vacía para almacenar los resultados
  mat_mut <- matrix(nrow = length(columnas), ncol = length(columnas))
  
  # Procesar todos los pares de columnas
  for (i in 1:length(columnas)) {
    fila <- numeric(length(columnas))
    
    # Calcular entropia 1
    entr1 <- entropia(df[[columnas[i]]])
    
    for (j in 1:length(columnas)) {
      # Calcular entropia 2
      entr2 <- entropia(df[[columnas[j]]])
      
      # Calcular entropia conjunta
      entr_conj <- entropia_conjunta(df[[columnas[i]]], df[[columnas[j]]])
      
      # Calcular información mutua
      inf_mut <- entr1 + entr2 - entr_conj
      
      # Guardar en la matriz
      fila[j] <- inf_mut
    }
    
    mat_mut[i,] <- fila
  }
  
  # Crear un dataframe a partir de la matriz y establecer nombres de columnas
  mat_mut_df <- as.data.frame(mat_mut)
  colnames(mat_mut_df) <- columnas
  rownames(mat_mut_df) <- columnas
  
  return(mat_mut_df)
}