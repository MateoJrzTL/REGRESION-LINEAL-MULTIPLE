#'Regresion lineal multiple
#'
#'Realiza una regresion linal multiple.
#'
#' @param X (data.frame) de todos los datos.
#' @param Y (data.frame) de la variable respuesta.
#' @return Una lista con las Betas, Minimo,Maximo, Error residual estandar, r^2 y r ajustada.
#' @export
Regresion_Lineal_M <- function (X,Y){
  #crear la matriz muestra
  xs <- as.matrix(cbind(1, X[,-1]))

  #calculamos betas o coeficientes de regresion
  beta <- solve(t(x) %*% x) %*% t(x) %*% Y

  #calcular el vector de predicciones
  y_pred <- x %*% beta

  #calcular el error resisudal
  error <- Y - y_pred

  #calcular la varianza residual
  var_residual <- sum(error^2) / (length(Y) - ncol(x))

  #calcular la matriz de covarianza de los coeficientes
  cov_beta <- var_residual * solve(t(x) %*% x)

  #calcular el valor estandar de los coeficientes
  se_beta <- sqrt(diag(cov_beta))

  #calcular el estadistico t y el valor p para cada coeficiente
  t_beta <- beta / se_beta
  p_beta <- 2 * pt(abs(t_beta), df = length(Y) - ncol(x), lower.tail = FALSE)

  #crear un data frame con los resultados
  results <- data.frame(coeficientes = beta, error_estandar = se_beta, t = t_beta, p_valor = p_beta)
  row.names(results) <- c("intercepto", colnames(x)[-1])

  #calcular el minimo y el maximo
  Minimo <- min(error)
  Maximo <- max(error)

  #calcular el error estandar residual
  RSE <- sqrt(var_residual)

  #calcular el error de la suma de cuadrados
  SCE <- sum(error ^2)

  #calcular el promedio de y para funciones proximas
  y_prom <- mean(Y)

  #calcular la suma total de cuadrados
  w <- y_prom - y_pred
  w1 <- w^2
  src <- sum(w1)
  sct <- src + SCE

  #clacular r^2 o coeficiente de determinacion
  R_square <- src/sct

  #calcular r ajustada
  obser_variable <- DF[,-1]
  K <- NCOL(obser_variable)
  r_uno <- (1-R_square)
  R_ajusta <- 1-((r_uno) * ((nrow(x)-1) / (nrow(x)-K-1)))

  #crear una lista con los valores y resultaados anteriores.
  valores <- list("Min"=Minimo,"Max"=Maximo,"Error estandar residual"=RSE,"R2"=R_square,"R ajustada"=R_ajusta, "resultados"=results)
  return(valores)
}
