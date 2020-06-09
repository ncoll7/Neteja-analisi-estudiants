
############# 2 ############# 

#Lectura del fitxer de dades
bd <- read.csv("StudentsPerformance.csv", header=T)

#Mirem les variables i quin format tenen
str(bd)

#Passem les variables de tipus chr a factors

bd$gender <- as.factor(bd$gender)
bd$race.ethnicity <- as.factor(bd$race.ethnicity)
bd$parental.level.of.education <- as.factor(bd$parental.level.of.education)
bd$lunch <- as.factor(bd$lunch)
bd$test.preparation.course <- as.factor(bd$test.preparation.course)

#Comprovem
str(bd)

#Re-orednem bd$parental.level.of.education

levels(bd$parental.level.of.education)
bd$parental.level.of.education <- factor(bd$parental.level.of.education, 
                                         levels = c("some high school", "high school", "some college", 
                                                    "associate's degree", "bachelor's degree", "master's degree"))
levels(bd$parental.level.of.education)

#Creem una nova variable amb la mitjana de les 3 notes

bd$mean.score <- (bd$math.score + bd$reading.score + bd$writing.score)/3


############# 3 ############# 

#Mirem si trobem presencia de valors faltants
sum(is.na(bd))
sum(bd == "")
sum(bd == " ")
sum(bd == "?")
sum(bd == 0)

#Identifiquem que la fila on hi ha el 0 és en una variable on hi poden aparèixer 0
bd[60,]

#Mirem que no hi hagin missings "camuflats en les categories"
levels(bd$gender)
levels(bd$race.ethnicity)
levels(bd$parental.level.of.education)
levels(bd$lunch)
levels(bd$test.preparation.course)

#Realitzem un resum estadísitc de les variables numèriques per mirar si hi ha presència d'outliers
summary(bd$math.score)

summary(bd$reading.score)

summary(bd$writing.score)



############# 4 ############# 


#Anàlisi univariant variables categòriques
par(mfrow=c(1,2))

barplot(table(bd$gender), main = "gender", cex.names = 1.2,  ylim = c(0,600))
barplot(table(bd$lunch), main = "lunch",cex.names = 1.2,  ylim = c(0,700))

barplot(table(bd$race.ethnicity), main = "race.ethnicity", cex.names = 1.2, las = 2, ylim = c(0,350))
barplot(table(bd$test.preparation.course), main = "test.preparation.course", cex.names = 1.2, ylim = c(0,700))

par(mfrow=c(1,1))

barplot(table(bd$parental.level.of.education), cex.names=0.8, main = "parental.level.of.education", ylim = c(0,250))


#Anàlisi univariant variables numèriques

par(mfrow=c(2,2))


hist(bd$math.score, ylim = c(0,300))
hist(bd$reading.score, ylim = c(0,300))
hist(bd$writing.score, ylim = c(0,300))
hist(bd$mean.score, ylim = c(0,300))

par(mfrow=c(2,2))
boxplot(bd$math.score, main = "math.score")
boxplot(bd$reading.score, main = "reading.score")
boxplot(bd$writing.score, main = "writing.score")
boxplot(bd$writing.score, main = "mean.score")



#Comprovació de normalitat
#math
qqnorm(bd$math.score)
qqline(bd$math.score, col=3)

ks.test(bd$math.score, pnorm, mean(bd$math.score), sd(bd$math.score))
shapiro.test(bd$math.score)

#reading
qqnorm(bd$reading.score)
qqline(bd$reading.score, col=3)

ks.test(bd$reading.score, pnorm, mean(bd$reading.score), sd(bd$reading.score))
shapiro.test(bd$reading.score)

#writing
qqnorm(bd$writing.score)
qqline(bd$writing.score, col=3)

ks.test(bd$writing.score, pnorm, mean(bd$writing.score), sd(bd$writing.score))
shapiro.test(bd$writing.score)

#mean
qqnorm(bd$mean.score)
qqline(bd$writing.score, col=3)

ks.test(bd$mean.score, pnorm, mean(bd$mean.score), sd(bd$mean.score))
shapiro.test(bd$mean.score)


#provem la transformació BoxCox
library(DescTools)

bd$math.score.norm <- BoxCox(bd$math.score, lambda = BoxCoxLambda(bd$math.score))
bd$reading.score.norm <- BoxCox(bd$reading.score, lambda = BoxCoxLambda(bd$reading.score))
bd$writing.score.norm <- BoxCox(bd$writing.score, lambda = BoxCoxLambda(bd$writing.score))
bd$mean.score.norm <- BoxCox(bd$mean.score, lambda = BoxCoxLambda(bd$mean.score))

#prova d'homocedasticitat
library(car)

leveneTest(math.score ~ test.preparation.course, data = bd)
fligner.test(math.score ~ test.preparation.course, data = bd) 

leveneTest(reading.score ~ test.preparation.course, data = bd)
fligner.test(reading.score ~ test.preparation.course, data = bd) 

leveneTest(writing.score ~ test.preparation.course, data = bd)
fligner.test(writing.score ~ test.preparation.course, data = bd) 



############# Estudi 1 ############# 

#Representació gràfica
library(ggplot2)
theme_set(
  theme_classic() + 
    theme(legend.position = "top")
)


ggplot(bd, aes(x = math.score)) +
  geom_histogram(aes(color = test.preparation.course, fill = test.preparation.course), 
                 position = "identity", bins = 30, alpha = 0.3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  ggtitle("math.score per test.preparation.course")

ggplot(bd, aes(x = reading.score)) +
  geom_histogram(aes(color = test.preparation.course, fill = test.preparation.course), 
                 position = "identity", bins = 30, alpha = 0.3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  ggtitle("reading.score per test.preparation.course")

ggplot(bd, aes(x = writing.score)) +
  geom_histogram(aes(color = test.preparation.course, fill = test.preparation.course), 
                 position = "identity", bins = 30, alpha = 0.3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  ggtitle("writing.score per test.preparation.course")


#Tests
x <- bd$math.score[bd$test.preparation.course == "completed"]
y <- bd$math.score[bd$test.preparation.course == "none"]
mean(x)
mean(y)
t.test(x, y, alternative = "greater", var.equal = TRUE)


x <- bd$reading.score[bd$test.preparation.course == "completed"]
y <- bd$reading.score[bd$test.preparation.course == "none"]
mean(x)
mean(y)
t.test(x, y, alternative = "greater", var.equal = TRUE)


x <- bd$writing.score[bd$test.preparation.course == "completed"]
y <- bd$writing.score[bd$test.preparation.course == "none"]
mean(x)
mean(y)
wilcox.test(x, y, alternative = "greater", paired = FALSE)



############# Estudi 2 ############# 
pairs(bd[,6:8])
cor(bd[,6:8])
cor.test(bd$math.score,bd$reading.score)
cor.test(bd$math.score,bd$writing.score)
cor.test(bd$writing.score,bd$reading.score)


############# Estudi 3 ############# 

model <- lm(mean.score ~ gender+race.ethnicity+parental.level.of.education+lunch+test.preparation.course, data = bd)
plot(model)

summary(model)



