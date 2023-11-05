


######################################
#######     Discretizacion     #######
######################################

discretizeEW <- function(x, num_bins) {
  #'  Discretiza una lista o un DataFrame utilizando el algoritmo Equal Width (igual anchura).
  
  #'  Utiliza la función algoritmo_discretizeEW() del archivo utils.R para aplicar el algoritmo.
  #'  En esta función se comprueba de que tipo es el argumento recibido para actuar de una forma u otra.
  #'  En cualuier caso, filtrá las variables para escoger solo las numéricas.
  #'
  #' @param x La lista o DataFrame que se va a discretizar.
  #' @param num_bins (entero) El número intervalos en los que se discretizará.
  #' 
  #' @return Una lista donde se guarda la discretizacion y los puntos de corte.
  #' @return En caso que la entrada sea una dataframe, se devuelve una lista de listas
  #'         donde en cada elemento guarda la discretizacion y los puntos de corte de cada variable.

  if (is.vector(x)) { # si es atributo en forma de lista
    es_numerico <- all(sapply(x, function(valor) is.numeric(valor)))
    if (es_numerico) {
      resultado <- algoritmo_discretizeEW(x, num_bins)
      x_discretizado <- resultado$x_discretized
      cut_points <- resultado$cut_points
      return(list(x_discretizado = x_discretizado, cut_points = cut_points))
    }
  } else if (is.data.frame(x)) { # si es dataframe
    x <- x[sapply(x, function(col) is.numeric(col))]
    x_discretizado <- lapply(x, function(col) algoritmo_discretizeEW(col, num_bins))
    return(x_discretizado)
  }
}


discretizeEF <- function(x, num_bins) {
  #'  Discretiza una lista o un DataFrame utilizando el algoritmo Equal Frequency (igual frecuencia).
  
  #'  Utiliza la función algoritmo_discretizeEF() del archivo utils.R para aplicar el algoritmo.
  #'  En esta función se comprueba de que tipo es el argumento recibido para actuar de una forma u otra.
  #'  En cualuier caso, filtrá las variables para escoger solo las numéricas.
  #'
  #' @param x La lista o DataFrame que se va a discretizar.
  #' @param num_bins (entero) El número intervalos en los que se discretizará.
  #' 
  #' @return Una lista donde se guarda la discretizacion y los puntos de corte.
  #' @return En caso que la entrada sea una dataframe, se devuelve una lista de listas
  #'         donde en cada elemento guarda la discretizacion y los puntos de corte de cada variable.
  
  if (is.vector(x)) { # si es atributo en forma de lista
    es_numerico <- all(sapply(x, function(valor) is.numeric(valor)))
    if (es_numerico) {
      resultado <- algoritmo_discretizeEF(x, num_bins)
      x_discretizado <- resultado$x_discretized
      cut_points <- resultado$cut_points
      return(list(x_discretizado = x_discretizado, cut_points = cut_points))
    }
  } else if (is.data.frame(x)) { # si es dataframe
    x <- x[sapply(x, function(col) is.numeric(col))]
    x_discretizado <- lapply(x, function(col) algoritmo_discretizeEF(col, num_bins))
    return(x_discretizado)
  }
}


################################
#######     Métricas     #######
################################

varianza_dataframe <- function(df) {
  #' Calcula la varianza de las columnas continuas de un DataFrame.
  #'
  #' Utiliza la función varianza() del archivo utils.R para calcular la varianza para cada columna.
  #'
  #' @param df Un DataFrame de R que contiene columnas con cualquier tipo de datos.
  #'        La función filtra el DataFrame para seleccionar solo las columnas continuas
  #'
  #' @return Un vector de tipo numerico con los nombres de las variables originales asociados a sus elementos.
  
  df <- df[sapply(df, is.double)]  # Seleccionar las columnas numéricas del DataFrame
  
  return(sapply(df, varianza))  # Aplicar la función de varianza a cada columna
}


entropia_dataframe <- function(df) {
  #' Calcula la entropia de las columnas discretas de un DataFrame.
  #'
  #' Utiliza la función entropia() del archivo utils.R para calcular la varianza para cada columna.
  #'
  #' @param df Un DataFrame de R que contiene columnas con cualquier tipo de datos.
  #'        La función filtra el DataFrame para seleccionar solo las columnas discretas
  #'
  #' @return Un vector de tipo numerico con los nombres de las variables originales asociados a sus elementos.

  df <- df[sapply(df, is.integer)]  # Seleccionar las columnas numéricas del DataFrame
  
  return(sapply(df, entropia))  # Aplicar la función de entropía a cada columna
}


roc_dataframe <- function(df) {
  #' Calcula las curvas ROC para las columnas continuas de un DataFrame utilizando las etiquetas de la última columna
  #' como valores de clase.
  #' 
  #' Utiliza la función get_roc() del archivo utils.R para calcular para cada columna el área debajo de la curva ROC y las listas
  #' FPR y TPR.
  #'
  #' @param df Un DataFrame de R que contiene columnas con cualquier tipo de datos.
  #'        La función filtra el DataFrame para seleccionar solo las columnas continuas
  #'
  #' @return Un dataframe. Para cada columna (variables originales) tiene 3 filas donde la primera muestra
  #'         el area debajo de la curva, la segunda devuelve la lista de TPR y la tercera la lista FPR.
  
  col_etiqueta <- df[, ncol(df)]  # Guardar las etiquetas (última columna)
  df <- df[sapply(df, is.double)]  # Seleccionar las columnas numéricas del DataFrame
  
  return( as.data.frame(sapply(df, get_roc, etiquetas = col_etiqueta)))  # Aplicar la función get_roc a cada columna
}



#######################################################
#######     Normalización y Estandarización     #######
#######################################################

normalizar <- function(x) {
  #' Normaliza un vector de valores numéricos o un DataFrame con columnas numéricas en el rango [0, 1].
  #'
  #' Utiliza la función aux_normalizar() del archivo utils.R.
  #'
  #' @param x Vector o dataframe que se va a normalizar
  #'
  #' @return Un vector normalizado si los elementos son numericos, sino devuelve el propoo vector sin cambios
  #' @return Dataframe de la entrada pero con las columnas numericas normalizadas
  
  if (is.vector(x)) { # es un vector
    if (all(sapply(x, function(valor) is.numeric(valor)))) { # mirar que sea todo numerico
      return(aux_normalizar(x))
    } else {
      return(x)
    }
    
  } else if (is.data.frame(x)) { # es un dataframe
    df <- x
    columnas_numericas <- sapply(df, is.numeric)
    df[columnas_numericas] <- lapply(df[columnas_numericas], aux_normalizar)
    return(df)
    
  } else {
    return("No es ni una lista ni un DataFrame")
  }
}


estandarizar <- function(x) {
  #' Estandariza un vector de valores numéricos o un DataFrame con columnas numéricas 
  #' para que tengan media 0 y desviación estándar 1.
  #'
  #' Utiliza la función aux_estandarizarr() del archivo utils.R.
  #'
  #' @param x Vector o dataframe que se va a estandarizar
  #'
  #' @return Un vector normalizado si los elementos son numericos, sino devuelve el propoo vector sin cambios
  #' @return Dataframe de la entrada pero con las columnas numericas estandarizadas
  
  if (is.vector(x)) { # es un vector
    if (all(sapply(x, function(valor) is.numeric(valor)))) { # mirar que sea todo numerico
      return(aux_estandarizar(x))
    } else {
      return(x)
    }
    
  } else if (is.data.frame(x)) { # es un dataframe
    df <- x
    columnas_numericas <- sapply(df, is.numeric)
    df[columnas_numericas] <- lapply(df[columnas_numericas], aux_estandarizar)
    return(df)
    
  } else {
    return("No es ni una lista ni un DataFrame")
  }
}



#############################################
#######     Filtrado de variables     #######
#############################################

aplicar_filtrado <- function(nombre_func, df, umbral) {
  #' Aplica un filtrado a un DataFrame utilizando una función para calcular una métrica específica y un umbral.
  #' De este modo, dado un dataframe solo se escogen las columnas que SUPEREN el umbral establecido para la función
  #' indicada. 
  #'
  #' Las funciones disponibles estan establecdos en la lista lista_funciones con nombres asocados. Por ahora solo se han 
  #' añadido las metricas varianza y entropia, pero si se quiere añadir otras solo hay que indicar en la lista
  #'
  #' @param nombre_func string con el nombre de la metrica
  #' @param df Dataframe donde se aplicara las funciones
  #' @param umbral numerico. El umbrals para filtrar los resultados de la función.
  #'
  #' @return Dataframe con las columnas que cumplan los requisitos
  #' @return Si el nombre de la función no es válido, devuelve un mensaje indicando que la función no es válida.
  
  # Mapeo de nombres de funciones 
  lista_funciones <- list(
    varianza = varianza_dataframe,
    entropia = entropia_dataframe
  )
  
  # Verificar si la función existe en el diccionario
  if (nombre_func %in% names(lista_funciones)) {
    res <- lista_funciones[[nombre_func]](df) # Llamada a la función pasando el dataframe
    
    # Filtrar el dataframe según el umbral
    columnas_filtradas <- names(res[res > umbral])
    
    df_nuevo <- df[columnas_filtradas]
    return(df_nuevo)
    
  } else {
    return("Función no válida")
  }
}


#######################################################
#######     Correlación e Informacion Mutua     #######
#######################################################

calcular_correlacion <- function(df) {
  #' Calcula la matriz de correlación para las columnas numéricas de un DataFrame.
  #' Hace uso de la funcion cor().
  #'
  #' Los valores estarán en el rango de -1 a 1, donde 1 indica una correlación positiva perfecta, -1 
  #' indica una correlación negativa perfecta y 0 indica falta de correlación. 
  #' 
  #' @param df El DataFrame del cual se calculará la correlación. Las columnas ueden ser de cualquier tipo,
  #'           pero se escogerán solo aquellas que sean numéricas.
  #'
  #' @return Un dataframe en forma de matriz de correlación para las columnas numéricas del DataFrame.
  #'         Las columnas y filas mantendran los mismos nombres que el dataframe original.

  df <- df[sapply(df, is.numeric)] # Seleccionar solo las columnas numéricas
  return( as.data.frame(cor(df)) )
}


calcular_info_mutua <- function(df) {
  #' Calcula la información mutua entre las columnas categóricas de un DataFrame.
  #' Utiliza la función info_mutua() del archivo utils.R para calcular la informaciOn mutua entre pares de columnas.
  #'
  #' Los valores estarán en un rango positivo, donde valores más altos indican una mayor dependencia entre las variables.
  #' 
  #' @param df El DataFrame del cual se calculará la información mutua. Las columnas ueden ser de cualquier tipo,
  #'           pero se escogerán solo aquellas que sean numéricas.
  #'
  #' @return Un dataframe en forma de matriz de informacion mutua para las columnas categóricas del DataFrame.
  #'         Las columnas y filas mantendran los mismos nombres que el dataframe original.
  
  df <- df[sapply(df, is.factor)] # Seleccionar solo las columnas numéricas
  return(info_mutua(df))
}