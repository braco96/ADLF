# ==============================================================================
# PROYECTO: ADLF (Análisis Discriminante)
# AUTOR: Luis Bravo Collado (Braco96)
# ==============================================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(MASS, gmodels, scatterplot3d, install=FALSE)

print("=== PARTE 1: CASO PRÁCTICO (Iris) ===")
data(iris)
# Visualización 3D
scatterplot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, color=(2:4)[iris$Species], pch=15)

# Modelo Completo
fit4 <- lda(data=iris, Species ~ .)
print("Predicción Modelo Completo:")
CrossTable(iris$Species, predict(fit4)$class, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

print("=== PARTE 2: ANEXO TEÓRICO (Simulación Mytilicola) ===")
set.seed(123)
g1 <- mvrnorm(76, mu=c(219, 138), Sigma=matrix(c(306,-1,-1,210),2))
g2 <- mvrnorm(91, mu=c(241, 147), Sigma=matrix(c(306,-1,-1,210),2))
datos <- rbind(data.frame(g1, G="G1"), data.frame(g2, G="G2"))
colnames(datos)[1:2] <- c("L", "A")

modelo_sim <- lda(G ~ L + A, data=datos)
print(modelo_sim)
