#Question 1

bottles <- read.csv("C:/Users/antoi/Desktop/UTC/SY02 - Méthodes statistiques pour l'ingénieur/TP6/bottles.data", sep="")

t.test(bottles, mu = 500, alternative = "less", conf.level = 0.95)
t.test(bottles, mu = 500, alternative = "less", conf.level = 0.9)

# Test Student : vérifier que l'espérance est bien celle espérée
# p-value = 0.07243
# Puisque alpha* = 0.1 > 0.07, rejet de l'hypothèse
# Puisque alpha* = 0.05 < 0.07, acceptation de l'hypothèse

#Question 2

MM <- read.csv("C:/Users/antoi/Desktop/UTC/SY02 - Méthodes statistiques pour l'ingénieur/TP6/MM.data")

for (n in c(1:6))
{
  print(prop.test(MM[1, n], sum(MM), 1/6))
}

# Puisque p-value < 0.01 dans tous les cas, les couleurs ne sont pas représentés uniformément

#Question 3

install.packages("isdals")
library(isdals)
data(bodyfat)

par(mfrow = c(1, 3))

# Création des régressions linéaires
reglin1 <- lm(bodyfat$Fat~bodyfat$Triceps) # On peut refaire le test sans l'ordonnée à l'origine : 0 + Triceps pour tester le lien direct entre les 2 variables
reglin2 <- lm(bodyfat$Fat~bodyfat$Thigh) # Pr (> |t|) correspond à la pvalue du test
reglin3 <- lm(bodyfat$Fat~bodyfat$Midarm)

# Représentation des données
plot(bodyfat$Fat~bodyfat$Triceps, main = "Fat en fonction de Triceps", xlab = "Triceps", ylab = "Fat")
abline(reglin1$coefficients[1], reglin1$coefficients[2])
plot(bodyfat$Fat~bodyfat$Thigh, main = "Fat en fonction de Thigh", xlab = "Thigh", ylab = "Fat")
abline(reglin2$coefficients[1], reglin2$coefficients[2])
plot(bodyfat$Fat~bodyfat$Midarm, main = "Fat en fonction de Midarm", xlab = "Midarm", ylab = "Fat")
abline(reglin3$coefficients[1], reglin3$coefficients[2])

# Affichage des R²
summary(reglin1)$r.squared
summary(reglin2)$r.squared
summary(reglin3)$r.squared

# Test normalité
qqnorm(reglin1$residuals, main = "Normal Q-Q Plot 1", xlab = "Theoretical Quantiles", ylab = "Dataset Quantiles")
qqline()
qqnorm(reglin2$residuals, main = "Normal Q-Q Plot 2", xlab = "Theoretical Quantiles", ylab = "Dataset Quantiles")
qqline()
qqnorm(reglin3$residuals, main = "Normal Q-Q Plot 3", xlab = "Theoretical Quantiles", ylab = "Dataset Quantiles")
qqline()

# Test homogénéité
plot(reglin1$fitted.values, rstandard(reglin1), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 1")
plot(reglin1$fitted.values, rstandard(reglin2), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 2")
plot(reglin1$fitted.values, rstandard(reglin3), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 3")

#Question 4

# W = {x_bar < |(u_alpha* / sqrt(n) + 1) * 1 / theta0|}

delai.data <- read.csv("C:/Users/antoi/Desktop/UTC/SY02 - Méthodes statistiques pour l'ingénieur/TP6/delai-data.data", sep="")

alpha <- 0.05
x_bar <- mean(delai.data$delai)
n <- length(delai.data$delai)
theta0 <- 1 / 151
seuil <- abs((qnorm(alpha) / sqrt(n) + 1) / theta0)

# Puisque seuil > x_bar, x_bar est dans la région critique, donc on rejette l'hypothèse

p_value <- pnorm((x_bar - 1 / theta0) / (1 / (theta0 * sqrt(n))))

#Question 6

puiss_emp <- function(theta0, theta, n)
{
  echantillon <- rexp(n, theta)
  return ((mean(echantillon) < abs((qnorm(alpha) / sqrt(n) + 1) / theta0)))
}

#Question 7

mean(replicate(10000, puiss_emp(1/151, 1/151, 100))) # Proche de alpha

#Question 8

theta <- seq(theta0, theta0 * (1 + 0.70), length.out = 100)

puiss_emp_rep <- function(theta0, theta, n, k)
{
  mean(replicate(k, puiss_emp(theta0, theta, n)))
}

n <- length(delai.data$delai)
puiss_emp_vec <- sapply(theta, function(t) puiss_emp_rep(theta0, t, n, 1000))

plot(theta, puiss_emp_vec, main = "Puissance empirique en fonction de theta", xlab = "Theta", ylab = "Puissance empirique")

#Question 9

x1 <- sleep$extra[sleep$group == 1]
x2 <- sleep$extra[sleep$group == 2]

t.test(x1, mu = 0, alternative = "greater")
t.test(x2, mu = 0, alternative = "greater")
# On remarque qu'au seuil 0.05, le médicament 1 ne semble pas avoir d'effets contrairement au deuxième.
