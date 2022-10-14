#Question 1

n <- 100

runifa <- function(n) 
{
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}

estim <- function(echantillon)
{
  return(2*mean(echantillon))
}


#Question 2
a <- replicate(1000, estim(runifa(n)))

#Question 3
b <- boxplot(a)

#Question 4
estim <- function(echantillon, k)
{
  return(((k+1)*mean(echantillon^k))^(1/k))
}
a <- replicate(1000, estim(runifa(n), 2))
a <- replicate(1000, estim(runifa(n), 3))
a <- replicate(1000, estim(runifa(n), 4))

#Question 5
runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}

n <- 1000
X <- runknown(n)
mean(X)
sd(X)
sqrt(32.36)

u <- 7.2
o <- sqrt(32.36)

#Question 6
h <- hist(runknown(n))

#Question 7
plot(ecdf(runknown(n)))

#Question 8
T <- (mean(X) - u)/(o/sqrt(n))

#Question 10
random.T <- function(n)
{
  u <- 7.2
  o <- sqrt(32.36)
  return ((mean(runknown(n)) - u)/(o/sqrt(n)))
}
t.10 <- replicate(10, random.T(n))
t.1000 <- replicate(1000, random.T(n))

#Question 11
plot(ecdf(runknown(n)))
plot(ecdf(t.1000))

#Question 12
curve(pnorm, add=TRUE)

#Question 14
f <- function(lambda, x)
{
  return (dexp(x, rate=lambda))
}

#Question 15
L <- function(lambda, x)
{
  return (prod(f(lambda, x)))
}

#Question 16
logL <- function(lambda, x)
{
  return (sum(log(f(lambda, x))))
}

#Question 17
x <- rexp(100, rate = 3)

L(2.8, x)
L(3.1, x)
logL(2.8, x)
logL(3.1, x)

#Question 18
lambdas <- seq(0, 6, 0.01)
logL.lambdas <- sapply(lambdas, function(lambda) logL(lambda, x))
plot(lambdas, logL.lambdas, type = "l")
#La fonction affiche les valeurs de log vraissemblances pour des valeurs
#de lambdas différentes entre 0 et 6

#Question 19
g <- function(lambda)
{
  x <- rexp(100, rate = 3)
  return(logL(lambda, x))
}
opt <- optimize(g, lower = 0, upper = 6, maximum =  TRUE)

#Question 20
sim.EMV <- function()
{
  x <- rexp(100, rate = 3)
  opt <- optimize(function(lambda) logL(lambda, x), lower = 0, upper = 6, maximum =  TRUE)
  return(opt$maximum)
}

#Question 21
lambdas_real <- replicate(10000, sim.EMV())
mean(lambdas_real)
var(lambdas_real)

mean(lambdas_real) - 3
(n/(n-1)) * 3 - 3

#Question 22
install.packages("pracma")
library(pracma)

#Question 23
sim.Fisher <- function()
{
  x <- rexp(1000, rate = 3)
  fisher <- grad(function(lambda) (logL(lambda, x)), 3)
  return (fisher^2)
}

#Question 24
fisher <- replicate(1000, sim.Fisher())
fisher_moy <- mean(fisher)
fisher_theorique <- 1000 / (3^2)

#Question 25
variance <- 1 / fisher_moy
variance_empirique <- 1 / fisher_theorique

#Question 26
grad2 <- function(f, x)
{
  f_premiere <- function(x) grad(f, x)
  f_seconde <- function(x) grad(f_premiere, x)
  return(f_seconde(x))
}

sim.Fisher2 <- function()
{
  x <- rexp(1000, rate = 3)
  fisher <- grad2(function(lambda) (logL(lambda, x)), 3)
  return (fisher)
}

fisher_2 <- -sim.Fisher2()

