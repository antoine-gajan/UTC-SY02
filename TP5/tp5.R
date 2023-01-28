#Question 1

donnees <- data.frame(varx = c(0, 0.2, 0.3, 0.6),
                      vary = c(1.01, 1.44, 1.55, 2.1))
m <- lm(vary~varx, data = donnees)

a <- m$coefficients[1]
b <- m$coefficients[2]

#Question 2

plot(varx, vary, xlab = "varx", ylab = "vary", main = "Régression linéaire vary en fonction de varx")
abline(a, b)

#Question 3

sum(m$residuals)

a + b * mean(donnees$varx) == mean(donnees$vary)

#Question 4

var_totale <- mean((vary - mean(vary))^2)
var_exp_reg <- mean((m$fitted.values - mean(vary))^2)
var_res <- mean(m$residuals^2)
var_totale_bis <- var_exp_reg + var_res
R2 <- var_exp_reg / var_totale 

pearson <- cor(donnees$varx, donnees$vary, method = "pearson")
pearson^2 == R2

#Question 5

#Chargement des data
attach(anscombe)
anscombe$x1
#Régression linéaire
lm(y1~x1)
lm(y2~x2)
lm(y3~x3)
lm(y4~x4)

par(mfrow = c(2, 2))
plot(y1~x1, main = "Dataset 1")
plot(y2~x2, main = "Dataset 2")
plot(y3~x3, main = "Dataset 3")
plot(y4~x4, main = "Dataset 4")

#Question 6

#Test normalité (méthode 1)
par(mfrow = c(2, 2))
qqnorm(y1, main = "Normal Q-Q Plot 1", xlab = "Theoretical Quantiles", ylab = "y1")
qqline(y1)
qqnorm(y2, main = "Normal Q-Q Plot 2", xlab = "Theoretical Quantiles", ylab = "y2")
qqline(y2)
qqnorm(y3, main = "Normal Q-Q Plot 3", xlab = "Theoretical Quantiles", ylab = "y3")
qqline(y3)
qqnorm(y4, main = "Normal Q-Q Plot 4", xlab = "Theoretical Quantiles", ylab = "y4")
qqline(y4)

#Test normalité (méthode 2)
par(mfrow = c(2, 2))
m1 <- lm(y1~x1, data = anscombe)
hist(m1$residuals, freq = FALSE, main = "Histogramme résidus 1", xlab = "Résidus", ylab = "Densité")
curve(dnorm(x, mean(m1$residuals), sd(m1$residuals)), add = TRUE)

m2 <- lm(y2~x2, data = anscombe)
hist(m2$residuals, freq = FALSE, main = "Histogramme résidus 2", xlab = "Résidus", ylab = "Densité")
curve(dnorm(x, mean(m2$residuals), sd(m2$residuals)), add = TRUE)

m3 <- lm(y3~x3, data = anscombe)
hist(m3$residuals, freq = FALSE, main = "Histogramme résidus 3", xlab = "Résidus", ylab = "Densité")
curve(dnorm(x, mean(m3$residuals), sd(m3$residuals)), add = TRUE)

m4 <- lm(y4~x4, data = anscombe)
hist(m4$residuals, freq = FALSE, main = "Histogramme résidus 4", xlab = "Résidus", ylab = "Densité")
curve(dnorm(x, mean(m4$residuals), sd(m4$residuals)), add = TRUE)

#Test homoscédasticité
par(mfrow = c(2, 2))
plot(m1$fitted.values, rstandard(m1), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 1")
plot(m2$fitted.values, rstandard(m2), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 2")
plot(m3$fitted.values, rstandard(m3), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 3")
plot(m4$fitted.values, rstandard(m4), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité 4")

#Question 7

#Régression linéaire
hooker <- data.frame(Temp = hooker.data$Temp, Pression = hooker.data$Pression)
reg.lin <- lm(Pression~Temp, data = hooker)
summary(reg.lin)

#Affichage du dataset et de sa régression
par(mfrow=c(1,1))
plot(hooker$Temp, hooker$Pression, main = "Dataset Hooker", xlab = "Température (en °C)", ylab = "Pression")
abline(reg.lin$coefficients[1], reg.lin$coefficients[2])

par(mfrow=c(2,1))
#Test normalité
qqnorm(reg.lin$residuals, ylab = "Hooker dataset Quantiles")
qqline(reg.lin$residuals)

#Test homoscédasticité
plot(reg.lin$fitted.values, rstandard(reg.lin), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité")

#Question 8

alpha <- 0.01
confint(reg.lin, level = 1 - alpha)


#Question 9

newdata <- data.frame(Temp = c(97))
predict(reg.lin, newdata,interval = "confidence")

#Question 10

moore <- data.frame(Temps = moore.data$Date.of.introduction, NTransistors = moore.data$Transistor.count)

#Régression linéaire
reg.lin <- lm(log(NTransistors)~Temps, data = moore)
summary(reg.lin)

a <- reg.lin$coefficients[1]
b <- reg.lin$coefficients[2]

#Affichage du dataset et de sa régression
par(mfrow=c(1,2))
plot(moore$Temps, moore$NTransistors, main = "Dataset Moore Normal", xlab = "Année", ylab = "Nombre de transistors")
abline(reg.lin$coefficients[1], reg.lin$coefficients[2], col = "blue")
plot(moore$Temps, log(moore$NTransistors), main = "Dataset Moore Logarithmique", xlab = "Année", ylab = "Nombre de transistors")
abline(reg.lin$coefficients[1], reg.lin$coefficients[2], col = "blue")

par(mfrow=c(1,2))
#Test normalité
qqnorm(reg.lin$residuals, ylab = "Moore dataset Quantiles")
qqline(reg.lin$residuals)

#Test homoscédasticité
plot(reg.lin$fitted.values, rstandard(reg.lin), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité")


#Détermination alpha et beta
alpha <- exp(a)
beta <- b

#Prédiction du nombre de transistors en 2018
newdata <- data.frame(Temps = c(2018))
IC_confiance <- exp(predict(reg.lin, newdata,interval = "confidence"))
IC_pred <- exp(predict(reg.lin, newdata,interval = "prediction"))

#Question 11

cedar.data <- read.csv("cedar-data.data")
cedar <- data.frame(Diametre = cedar.data$diameter, Hauteur = cedar.data$height)
reg.lin <- lm(Hauteur~Diametre, data = cedar)
summary(reg.lin)
plot(reg.lin)


par(mfrow=c(1,1))
plot(cedar$Diametre, cedar$hauteur, main = "Dataset cèdres", xlab = "Diamètre", ylab = "Hauteur")
abline(reg.lin$coefficients[1], reg.lin$coefficients[2])

par(mfrow=c(1,2))
#Test normalité
qqnorm(reg.lin$residuals, ylab = "Cèdres dataset Quantiles")
qqline(reg.lin$residuals)

#Test homoscédasticité
plot(reg.lin$fitted.values, rstandard(reg.lin), xlab = "Valeurs prédites", ylab = "Résidus standardisés", main = "Homoscédasticité")

#Question 12

boxcot <- function(x, lambda)
{
  if (lambda == 0)
  {
    log(x)
  }
  else
  {
    (x^lambda - 1)/lambda
  }
}

#Question 13

par(mfrow=c(3,2))
for (lambda in c(-1, -1/2, 0, 1/3, 1/2, 1)) 
{
  plot(cedar$Diametre, cedar$Hauteur, main = paste("lambda = ", lambda), xlab = "Diamètre", ylab = "Hauteur")
  reg.lin <- lm(Hauteur~boxcot(Diametre, lambda), data = cedar)
  print(summary(reg.lin)$r.squared)
  curve(reg.lin$coefficients[1] + reg.lin$coefficients[2] * boxcot(x, lambda), add = TRUE, col = "red")
}

#Aide pour évaluation

Sreg <- function(x, y)
{
  reg <- lm(y~x)
  mean((reg$fitted.values - mean(y))^2)
}

Sres <- function(x, y)
{
  reg <- lm(y~x)
  mean(reg$residuals^2)
}

Stotale <- function(x, y)
{
  mean((y - mean(y))^2)
}

pearson <- function(x, y)
{
  cor(x, y, method = "pearson")
}

R2 <- function(x, y)
{
  reg <- lm(y~x)
  summary(reg)$r.squared
}

#Intervalle de confiance sur a et b
confint(reg, level = 1 - 0.05)

#Intervalle de confliance en un point
reglin <- lm(Y~X, data = RL05)
predict(reglin, data.frame(X = c(0.1)),interval = "confidence", level = 0.95)

#Intervalle de prédiction en un point
reglin <- lm(Y~X, data = RL05)
predict(reglin, data.frame(X = c(0.3)),interval = "prediction", level = 0.95)

#Levier
levier <- function(x, y)
{
  n <- length(x)
  1/n + ((x - mean(x))^2)/sum((x - mean(x))^2)
}

#Résidu standard / student
rstandard(reglin)
rstudent(reglin)



reg <- lm(Y~X, data = RLXX)
confint(reg, level = 0.95)
reg$residuals[NB]
predict(reg, data.frame(X = c(VALEUR)), interval = "REMPLIR")
