#Exercice 1

#Import de la librairie MASS
library(MASS)
head(painters)

#Question 1
hist(painters$Composition)
hist(painters$Drawing)
hist(painters$Colour)
hist(painters$Expression)

#Question 2
moyenne <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression)/4

#Question 3
moyenne_emp1 <- sum(moyenne) / length(moyenne)
variance1 <- (sum((moyenne - moyenne_emp1)**2))/length(moyenne)
variance_corr1 <- length(moyenne)/(length(moyenne) - 1) * variance1
ecart_type1 <- sqrt(variance1)
ecart_type_corr1 <- sqrt(variance_corr1)

#Question 4
moyenne_emp2 <- mean(moyenne)
variance2 <- var(moyenne)
ecart_type2 <- sd(moyenne)
variance_corr2 <- (length(moyenne)+1)/length(moyenne) * variance2
ecart_type_corr2 <- sqrt(variance_corr2)
  
#La variance dans R est la variance corrigée
  
#Question 5
hist(moyenne)

#Question 6
un <- 1 - pnorm(3, 0, 1) # ou pnorm(3, lower.tail = FALSE)
deux <- pnorm(42, 35, 6)
trois <- pnorm(50, 35, 6) - pnorm(40, 35, 6)
quatre_a <- dbinom(4, 5, 0.5)
quatre_b <- dbinom(9, 10, 0.5)
quatre_c <- dbinom(29, 30, 0.5)
cinq <- 1 - pbinom(14, 20, 0.5)
six <- pbinom(15, 20, 0.5) - pbinom(9, 20, 0.5)

#Question 7
alpha <- c(0.05, 0.1, 0.9)
un <- qnorm(alpha, 0, 1)
deux <- qchisq(alpha, 10)
trois <- qt(alpha, 5)
quatre <- qf(alpha, 2, 5)

#Question 8
dloi <- function(x, b)
{
  if (b < 0)
  {
    stop("Erreur : b doit être strictement positif")
  }
  a <- 2 / b**2
  f <- a * x
  f[x < 0] = 0
  f[x > b] = 0
  return(f)
}

#Question 9
dloi(c(-1:5), 3)
curve(dloi(x, 3), from = -1, to = 5)

#Question 10
ploi <- function(x, b)
{
  if (b < 0)
  {
    stop("Erreur : b doit être strictement positif")
  }
  a <- 2 / b**2
  f_rep <- a * x**2 / 2
  f_rep[x < 0] = 0
  f_rep[x > b] = 1
  return (f_rep)
}

#Question 11
curve(ploi(x, 3), from = -5, to = 5)

#Question 12
qloi <- function(alpha, b)
{
  if (b < 0)
  {
    stop("Erreur : b doit être strictement positif")
  }
  #Si un alpha de la liste est négatif
  if (length(alpha[alpha < 0 | alpha > 1] > 0))
  {
    stop("Erreur : alpha doit être compris entre 0 et 1")
  }
  quartile <- b * sqrt(alpha)
  quartile[alpha == 0] <- 0
  quartile[alpha == 1] <- b
  return (quartile)
}

curve(qloi(x, 3), from = 0, to = 1)

#Question 13
rloi <- function(n, b)
{
  val <- qloi(runif(n, 0, 1), b)
  return (val)
}

#Question 14

par(mfrow = c(2, 2))
for (n in c(10, 50, 100, 1000))
{
  hist(rloi(n, 3), breaks = round(1 + 10/3 * log10(n)), freq = FALSE, main = n, xlim = c(-1, 4))
  curve(dloi(x, 3), add = TRUE)
}
