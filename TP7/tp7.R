#Question 1

install.packages("MASS")
library("MASS")

t.test(immer$Y1, immer$Y2, paired = TRUE)

#p-value = 0.002413 => rejet de H0 et de l'hypothèse d'égalité des espérances des rendements

z <- immer$Y2 - immer$Y1
shapiro.test(z)
hist(z, main = "Histogramme de la différence")

#Question 2

difference <- (immer$Y2 - immer$Y1) < 0
n <- length(difference)
difference <- sum(difference)
mean(difference)

prop.test(difference, n, 0.5)

# p-value = 0.001911 => rejet de H0 et de l'hypothèse d'égalité

#Question 3

var.test(shoes$A, shoes$B)

# p-value = 0.9372 => accepte H0 et l'hypothèse d'égalité des variances

#Question 4

t.test(shoes$A, shoes$B, var.equal = TRUE)

# p-value = 0.7165 => accepte H0 et l'hypothèse d'égalité des espérances

z <- shoes$A - shoes$B
shapiro.test(z)

# => Accepte H0
#Question 5

shapiro.test(galaxies)

# p-value = 7.302e-07 => rejette H0 et l'hypothèse de normalité des données

#Question 6

delai.data <- read.csv("C:/Users/antoi/Desktop/UTC/SY02 - Méthodes statistiques pour l'ingénieur/TP7/data/delai-data.data", sep="")
View(delai.data)

lambda <- 1 / mean(delai.data$delai)

FrepH0 <- function(x) pexp(x, rate = lambda)

ks.test(delai.data$delai, FrepH0)
# ou ks.test(delai, "pexp", lambda)

# p-value = 0.05795 => Ne rejette pas H0

#Question 7

separation <- quantile(delai.data$delai, seq(0, 1, 0.1))

#Question 8

tab_effectifs <- table(cut(delai.data$delai, separation, include.lowest = TRUE))

#Question 9

proba_theorique <- diff(c(0, pexp(separation, lambda)[3:length(separation)-1], 1))

#Question 10

chisq.test(tab_effectifs, p=proba_theorique)

#Question 11

stat <- chisq.test(tab_effectifs, p=proba_theorique)$statistic
1 - pchisq(stat, df = length(tab_effectifs) - 1 - 1)

#Question 12

glace <- data.frame(chocolat = c(100, 350),
                    vanille = c(120, 200),
                    fraise = c(60, 90),
                    row.names = c("H", "F"))

# Question 13 et 14
ct <- chisq.test(glace)

# p-value = 6.938e-07 => rejet H0 et pas indépendance

#Question 15

ct$observed
ct$expected

#Question 16
sum((glace - ct$expected)^2/ct$expected)

#Question 17

#H0 : Indépendance entre rhume et vitamine C
#H1 : Dépendance entre rhume et vitamine C
cold <- read.csv("C:/Users/antoi/Desktop/UTC/SY02 - Méthodes statistiques pour l'ingénieur/TP7/data/cold.data", row.names = 1)
chisq.test(cold)
