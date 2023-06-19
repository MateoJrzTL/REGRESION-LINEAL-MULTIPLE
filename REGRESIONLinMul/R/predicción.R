#'Predicciones de regresion lineal multiple.
#'
#'Realiza predicciones de una regresion linal multiple.
#'
#' @param Xn (data.frame) de los datos de las variables explicativas.
#' @param ajuste es la salida de valores de la funcion Regresion_Lineal_M
#' @return los valores de las predicciones de y
#' @export
predicción <- function (Xn,ajuste){

  #extraemos las Betas o coeficientes de prediccion
  Betas <- ajuste$resultados[,1]

  #creamos nuestra matriz de variables predictoras
  x <- as.matrix(cbind(1, Xn))

  #calculamos las predicciones
  y_predicciones <- x %*% Betas
  return(y_predicciones)
}
