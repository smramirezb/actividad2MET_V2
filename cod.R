# Códigos de R para revisar la normalidad de la muestra

# Muestra de eficiencia energética (kWh/L)
muestra <- c(6.12, 5.87, 5.45, 6.33, 5.71, 6.04, 5.92, 5.65, 6.18, 
             5.78, 5.95, 6.21, 5.63, 5.79, 6.11, 5.88, 6.02, 5.76, 5.85, 6.10)

# Histograma y QQ-Plot para inspección visual
par(mfrow = c(1, 2))
hist(muestra, main = "Histograma de la muestra", col = "skyblue", 
     xlab = "Eficiencia (kWh/L)", border = "black")
qqnorm(muestra, main = "QQ-Plot")
qqline(muestra, col = "red")

# Prueba de Shapiro-Wilk
shapiro_test <- shapiro.test(muestra)
cat("\nResultado de Shapiro-Wilk:\n")
print(shapiro_test)

# Prueba de Kolmogorov-Smirnov contra normalidad
ks_test <- ks.test(muestra, "pnorm", mean = mean(muestra), sd = sd(muestra))
cat("\nResultado de Kolmogorov-Smirnov:\n")
print(ks_test)

# 2. Intervalos de confianza Bootstrap
set.seed(123)
B <- 1000  # Número de muestras bootstrap
bootstrap_means <- replicate(B, mean(sample(muestra, length(muestra), replace = TRUE)))

# Método 1: Percentil (95%)
ci_percentil <- quantile(bootstrap_means, probs = c(0.025, 0.975))
cat("\nIntervalo de Confianza Percentil (95%):\n")
print(ci_percentil)

# Método 2: BCa (Sesgo-Corrección, aproximación simple)
mean_muestra <- mean(muestra)
ci_bca <- c(2 * mean_muestra - quantile(bootstrap_means, 0.975),
            2 * mean_muestra - quantile(bootstrap_means, 0.025))
cat("\nIntervalo de Confianza BCa (95%):\n")
print(ci_bca)


# 3. Intervalo de confianza paramétrico para la media (asumiendo normalidad)
n <- length(muestra)
error_estandar <- sd(muestra) / sqrt(n)
t_valor <- qt(0.975, df = n - 1)
ci_parametrico <- c(mean_muestra - t_valor * error_estandar, 
                    mean_muestra + t_valor * error_estandar)
cat("\nIntervalo de Confianza Paramétrico (95%):\n")
print(ci_parametrico)

