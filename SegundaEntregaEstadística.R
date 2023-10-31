error_gradiente_vector = c()
error_lm_vector = c()
for (i in 1:5){
  datos = runif(100)
  coef = c(8,4)
  x = runif(100)
  error = rnorm(100,0,1)
  y = coef[1] + coef[2]*x + error
  plot(y~x)
  
  # Función objetivo
  J <- function(beta0, beta1) {
    sum((y - beta0 - beta1*x)^2)
  }
  
  # Dirección
  gradiente <- function(beta, x, y) {
    derivada_beta0 = -2*sum(y - beta[1] - beta[2]*x)
    derivada_beta1 = -2*sum(x*(y - beta[1] - beta[2]*x))
    return(-c(derivada_beta0, derivada_beta1))
  }
  
  descenso_gradiente <- function(x, y, alpha, iteraciones = 5000) {
    beta = c(0, 0)
    tol <- 10^(-6)
    dir = gradiente(beta, x, y)
    for (i in 1:iteraciones) {
      beta = beta + alpha*dir
      dir = gradiente(beta, x, y)
      if(sum(dir)<tol){break}
    }
    cat("Iteración de parada: ",i, "\n")
    return(beta)
  }
  
  
  punto = descenso_gradiente(x, y, alpha = 0.0001)
  punto
  abline(a=punto[1], b=punto[2], col = "red")
  cat("El valor de beta obtenido por descenso gradiente es: ", punto,"\n")
  
  datos <- data.frame(x = x, y = y)
  modelo <- lm(y ~ x, data = datos)
  abline(a = coef(modelo)[1], b = coef(modelo)[2], col = "blue")
  cat("El valor de beta obtenido por LM es: ", coef(modelo),"\n")
  error_gradiente = J(punto[1],punto[2])
  cat(valor,"\n")
  error_lm = sum(resid(modelo)^2)
  cat(error_lm,"\n\n")
  error_gradiente_vector = c(error_gradiente_vector, error_gradiente)
  error_lm_vector = c(error_lm_vector, error_lm)
  
}
error_medio_gradiente = mean(error_gradiente_vector)
error_medio_lm = mean(error_lm_vector)
diff_error = error_medio_gradiente-error_medio_lm
cat("El error medio utilizando el método de descenso gradiente es: ", error_medio_gradiente, "\n")
cat("El error medio utilizando el método de descenso gradiente es: ", error_medio_lm, "\n")
cat("La diferencia de error es de: ", diff_error, "\n")