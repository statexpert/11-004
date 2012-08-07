library(psych)
library(reshape)

# Импорт данных
dt <- read.table("data.csv", sep = ";", header = TRUE)
levels(dt$Группа) <- c("Контрольная", "Экспериментальня")
colnames(dt)[4:7] <- c("Замер.1", "Замер.2", "Замер.3", "Замер.4")

# Предварительные проверки
attach(dt)
print(describe.by(Замер.1, Группа))
boxplot(Замер.1 ~ Группа, data = dt, xlab = "Группы")
title(main = "Сравнение контрольной и\nэкспериментальной групп\nдо начала исследования")
var.test(Замер.1 ~ Группа)
print(wilcox.test(Замер.1 ~ Группа))
detach(dt)

# Подготовка данных к ANOVA с повторными измерениями
adt <- melt(dt, id = c("Фамилия", "Группа"), measure = c(4:7))
colnames(adt) <- c("Испытуемый", "Группа", "Замер", "Баллы")

# ANOVA с повторными измерениями
# Тест на гомогенность групп
bartlett.test(Баллы ~ Замер + Группа, adt)
var.test(Баллы ~ Группа, adt)
attach(adt)
boxplot(Баллы ~ Замер + Группа)
aov.out <- aov(Баллы ~ (Группа * Замер) + Error(Испытуемый/(Замер), data = adt))
print(summary(aov.out))
print(model.tables(aov.out, "means"))
interaction.plot(Замер, Группа, Баллы, type = "b", pch = c(1, 2), ylim = range(Баллы), lwd = 3, col = rainbow(2))
detach(adt)

# Множественные сравнения
# Внутри групп
adtk <- adt[adt$Группа == "Контрольная",]
attach(adtk)
print(pairwise.wilcox.test(Баллы, Замер, paired = TRUE))
detach(adtk)
adte <- adt[adt$Группа == "Экспериментальня",]
attach(adte)
print(pairwise.wilcox.test(Баллы, Замер, paired = TRUE))
detach(adte)
# Между группами
print(sapply(dt[,4:7], FUN = function (x) wilcox.test(formula = x ~ dt$Группа, data = dt)))