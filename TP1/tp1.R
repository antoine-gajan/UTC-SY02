#Question 1

val <- log(640320**3 + 744)/sqrt(163)
print(val==pi)

#Question 2

notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)

#Question 3
notes <- c(notes, 4)

#Question 4
notes10 <- notes / 2
print(length(notes10[notes10 > 6]))

#Question 5
moyenne <- mean(notes[c(1, 3, length(notes))])
print(moyenne)

#Question 6
nombre <- length(notes[notes > 10])

#Question 7
notes_non_frac <- notes[notes == floor(notes)]
print(min(notes_non_frac))

#Question 8
notes2 <- notes - 2

#Question 9
print(length(notes2[notes2 < 0]))
notes2[notes2 < 0] = 0

#Question 10
adn <- factor(c("A", "C", "A", "A", "G", "A", "T", "G", "C", "C", "A", "T", "T", "G", "T", "C"), ordered  = TRUE)
print(levels(adn))
print(nlevels(adn))

#Question 11
A_nb <- adn[adn == "A"]
T_nb <- adn[adn == "T"]
C_nb <- adn[adn == "C"]
G_nb <- adn[adn == "G"]

#Question 12
sy02 <- read.csv("C:/Users/antoi/Desktop/UTC/SY02 - Méthodes statistiques pour l'ingénieur/sy02.data")
print(length(sy02))
print(ncol(sy02))
print(nrow(sy02))

#Question 13
print(head(sy02))
print(summary(sy02))

#Question14
sy02[, c(2, ncol(sy02))]

#Question 15
notes_EG <- sy02[sy02$correcteur.median == "EG", 'median']
print(mean(notes_EG))

#Question 16
etu_progression <- sy02[sy02$final > sy02$median, ]
print(nrow(etu_progression) / nrow(sy02) * 100)

#Question 17
notes_final <- sy02$final
mean(notes_final)
sd(notes_final)
var(notes_final)
median(notes_final)
max(notes_final)
min(notes_final)

#Question 18
summary(notes_final)

#Question 19
notes_median <- sy02$median
summary(notes_median)
IQR(notes_median)

#Question 20
notes_median <- sort(notes_median)
notes_median_tronquees <- notes_median[11:length(notes_median) - 10]
print(mean(notes_median_tronquees))

#Question 21
t <- table(sy02$correcteur.median)
barplot(t)

#Question 22
boxplot(sy02$final)

#Question 23
moustache_inf <- quantile(sy02$final, 0.25) - 1.5 * IQR(sy02$final)
print(length(notes_final[notes_final < moustache_inf]))

#Question 24
stem(sy02$moyenne)

#Question 25
hist(sy02$final)

#Question 26
h <- hist(sy02$final, breaks = c(0, 15, 20))

#Question 27
print(h$density)

#Question 28
print(h)
largeur_breaks <- diff(h$breaks)
print(sum(largeur_breaks * h$density))

#Question 29
plot(sy02$final ~ sy02$median)

#Question 30
boxplot(sy02$final ~ sy02$correcteur.final)

#Question 31
final_EG <- sy02$final[sy02$correcteur.final == "EG"]
boxplot(final_EG)

#Question 32
stripchart(sy02$final ~ sy02$correcteur.final, data = sy02)

#Question 33
stripchart(sy02$final ~sy02$correcteur.final, method = "jitter")
