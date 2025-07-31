library(ggplot2)
set.seed(1)

#Función de distribución inversa
distribucion_inversa <- function(u, theta) {
  return(theta * sqrt(u))
}

#valor del parámetro dado por enunciado
valor_theta <- 5

# Función para generar una muestra aleatoria
simular_muestra <- function(n) {
  muestra <- runif(n)
  muestra_simulada <- distribucion_inversa(muestra, valor_theta)
  return(muestra_simulada)
}

muestra1 <- simular_muestra(10)
muestra2 <- simular_muestra(50)
muestra3 <- simular_muestra(100)

# Función para crear intervalos de confianza
#   de nivel 1-alpha
generar_IC <- function(muestra, alpha, n){
  maximo <- max(muestra)
  q2 <- (alpha/2)^(1/(2*n))
  q1 <- ((alpha/2) + 0.95)^(1/(2*n))
  A <- maximo/q1
  B <- maximo/q2
  return(c(A,B))
}

IC1 <- generar_IC(muestra1, 0.05, 10)
IC2 <- generar_IC(muestra2, 0.05, 50)
IC3 <- generar_IC(muestra3, 0.05, 100)

# Repliquemos el experimento (con un mismo alpha) muchas veces y grafiquemos los intervalos
# Generamos una tabla que tenga la iteración (i), la cota inferior (A) y la superior (B)
Nrep <- 100
alpha <- 0.05
data <- data.frame("i"=integer(), "A"=double(), "B"=double())
for(i in 1:Nrep){
  muestra <- simular_muestra(50)
  ic = generar_IC(muestra, alpha, 50)
  data[i, ] <- c(i, ic[1], ic[2])
}
data

# Función para graficar los resultados de la tabla [i;A;B]
# Theta es el valor original del parámetro al cual queremos encontrarle un IC
# En el caso de la normal será theta = mu = 11
ic_plot <- function(data_plot, theta){
  options(repr.plot.width=16, repr.plot.height=8)
  ggplot(data_plot, aes(x=i, y=(A+B)/2), color=as.factor(ContainsTheta)) +
    geom_errorbar(aes(ymin=A, ymax=B), width=0.25) +
    geom_point(aes(color= A<=theta & theta<=B), size=3)+
    geom_hline(yintercept=theta, linetype="dashed")+
    
    ggtitle("Intervalos de confianza") +
    theme_light() +
    scale_colour_manual(values = c("TRUE" = "green4", "FALSE" = "red4"),
                        labels = c("Sí", "No"), breaks = c("TRUE", "FALSE")) + 
    theme(plot.title=element_text(hjust=0.5, size=24),
          axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14)) +
    guides(color=guide_legend("El IC contiene a theta"))+
    xlab("Muestra") + ylab("IC")
}

# Grafiquemos los resultados
ic_plot(data, valor_theta)

# Calcular proporción de IC que contiene al parámetro usando mean
proporcion_contiene_theta <- mean(data$A <= valor_theta & valor_theta <= data$B)
