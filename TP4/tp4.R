#Question 1

n <- 100
u <- 0
o <- 1

chisq1 <- function(u, o, n)
{
  X = rnorm(n, u, o)
  var_empirique = var(X)
  return ((n-1) * var_empirique / o^2)
}

x <- replicate(10000, chisq1(u, o, n))

hist(x, freq = FALSE)
curve(dchisq(x, n - 1), add = TRUE)

#Question 2

t1 <- function(u, o, n)
{
  X = rnorm(n, u, o)
  x_barre = mean(X)
  ecart_type = sd(X)
  return ((x_barre - u)/(ecart_type/sqrt(n)))
}

x <- replicate(10000, t1(u, o, n))
hist(x, freq = FALSE)
curve(dt(x, n-1), add = TRUE)

#Question 3

#On a IC = [x_barre +- u(1-alpha/2)*sqrt(o²/n)]

#Echantillon loi normale
n <- 1000
u <- 0
o <- 1
x <- rnorm(n, u, o)


#Test avec intervalle de 95%
alpha <- 0.05

IC <- c(mean(x) - qnorm(1-alpha/2)*sqrt(o^2/n), mean(x) + qnorm(1-alpha/2)*sqrt(o^2/n))

#Ou bien : IC <- c(mean(x) + c(-1, 1)*qnorm(1-alpha/2)*sqrt(o^2/n))


#Question 4

#Si o² pas connu, on a IC = [x_barre +- t(n-1, 1 - alpha/2)µ S* / sqrt(n)]

#Echantillon loi normale
n <- 100
u <- 0
o <- 1

x <- rnorm(n, u, o)


#Test avec intervalle de 95%
alpha <- 0.05

IC <- c(mean(x) - qt(1-alpha/2, n-1)*sqrt(var(x)/n), mean(x) + qt(1-alpha/2, n-1)*sqrt(var(x)/n))

#Vérification avec valeur exacte
t.test(x, conf.level = 1 - alpha)$conf.int

#Question 5

gen_IC <- function(x, alpha)
{
  n <- length(x)
  IC <- c(mean(x) - qt(1-alpha/2, n-1)*sqrt(var(x)/n), mean(x) + qt(1-alpha/2, n-1)*sqrt(var(x)/n))
  return(IC)
}

#Question 6

IC_real <- replicate(100, gen_IC(rnorm(n, u, o), alpha))

#Question 7

source("utils.R")
plot_ICs(IC_real, u)

#Question 8

#Comparaison pour 4 valeurs de n
n1 <- 10
n2 <- 100
n3 = 500
n4 = 1000

par(mfrow = c(2, 2))

IC_real <- replicate(n1, gen_IC(rnorm(n1, u, o), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main="n = 10")

IC_real <- replicate(n2, gen_IC(rnorm(n2, u, o), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main="n = 100")

IC_real <- replicate(n2, gen_IC(rnorm(n3, u, o), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main="n = 500")

IC_real <- replicate(n2, gen_IC(rnorm(n4, u, o), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main="n = 1000")

#Quand n augmente, la dispersion diminue.

#Question 9

o1 <- 1
o2 <- 5
o3 <- 10
o4 <- 20

n <- 100

par(mfrow = c(2, 2))

IC_real <- replicate(n, gen_IC(rnorm(n, u, o1), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main = "sd = 1")

IC_real <- replicate(n, gen_IC(rnorm(n, u, o2), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main = "sd = 5")

IC_real <- replicate(n, gen_IC(rnorm(n, u, o3), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main = "sd = 10")

IC_real <- replicate(n, gen_IC(rnorm(n, u, o4), alpha))
plot_ICs(IC_real, u, xlim=c(-4, 4), main = "sd = 20")

#Plus les dispersions sont faibles, plus les intervalles sont petits.

#Question 10

taux_recouvrement <- function(alpha)
{
  #Echantillon
  n <- 100
  u <- 40
  o <- 3
  x <- rnorm(n, u, o)
  #Génération intervalles de confiance
  ICs <- gen_IC(x, alpha)
  return(u > ICs[1] & u < ICs[2])
}

#Test
test <- replicate(100, taux_recouvrement(0.05))
mean(test)

# On a obtenu 95, ce qui correspond bien au fait que 95% des intervalles contiennent le paramètre.

#Question 11

slutsky <- function(p, n, k, alpha) {
  #Simulation d'une valeur TRUE/FALSE
  sim <- function() {
    x <- rbinom(n, 1, p) #n lois de Bernouilli
    phat <- mean(x)
    #Intervalle de confiance et vérification si p est dans IC
    IC <- phat + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(phat * (1 - phat)/n)
    return(p >= IC[1] & p <= IC[2])
  }
  mean(replicate(k, sim()))
}

#Question 12

p <- 0.02
k <- 10000
alpha <- 0.05
ns <- floor(10^seq(1, 4, length.out = 30)) # 30 points en échelle logarithmique
slutsky.prop <- sapply(ns, function(n) slutsky(p, n, k, alpha))
plot(log10(ns), slutsky.prop, type = "l", col = "blue", main = "Lemme de Slutsky : loi binomiale")

#Question 13

slutsky2 <- function(p, n, k, alpha) {
  #Simulation d'une valeur TRUE/FALSE
  sim <- function() {
    x <- rbinom(n, 1, p)
    moy <- mean(x)
    #Intervalle de confiance et vérification si p est dans IC
    IC <- ((2 * n * moy + qnorm(1 - alpha / 2)^2) + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(qnorm(1 - alpha/2)^2 + 4 * n * moy * (1 - moy)))/(2 * n + 2 * qnorm(1 - alpha / 2)^2)
    return(p >= IC[1] & p <= IC[2])
  }
  mean(replicate(k, sim()))
}

p <- 0.02
k <- 10000
alpha <- 0.05
ns <- floor(10^seq(1, 4, length.out = 30)) # 30 points en échelle logarithmique
slutsky2.prop <- sapply(ns, function(n) slutsky2(p, n, k, alpha))
plot(log10(ns), slutsky.prop, type = "l", col = "red", main = "Lemme de Slutsky : estimation de p avec une loi binomiale", ylab = "Probabilité")
lines(log10(ns), slutsky.prop2, col = "red")
