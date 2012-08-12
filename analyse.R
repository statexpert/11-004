library("psych")

# Импорт данных
dt <- read.table("data.csv", sep = ";", header = TRUE)
levels(dt$Группа) <- c("Контрольная", "Экспериментальная")
colnames(dt)[4:7] <- c("Замер.1", "Замер.2", "Замер.3", "Замер.4")

# Предварительные проверки
print(describe.by(dt$Замер.1, dt$Группа))
boxplot(Замер.1 ~ Группа, xlab = "Группы", data = dt)
title(main = "Сравнение контрольной и\nэкспериментальной групп\nдо начала исследования")
var.test(Замер.1 ~ Группа, data = dt) # проверка на однородность
print(oneway.test(Замер.1 ~ Группа, data = dt)) # проверка значимости различий

# Подготовка данных к ANOVA с повторными измерениями
adt <- reshape(data=dt[2:7], idvar="Фамилия", varying=list(3:6), v.names="Баллы", direction="long")
colnames(adt)[c(1, 3)] <- c("Испытуемый", "Замер")

# ANOVA с повторными измерениями для 4-х замеров
# Тест на гомогенность групп
bartlett.test(Баллы ~ Замер + Группа, data = adt)
var.test(Баллы ~ Группа, data = adt)
boxplot(Баллы ~ Замер + Группа, data = adt)
aov.out <- aov(Баллы ~ (Группа * Замер) + Error(Испытуемый/(Замер), data = adt))
print(summary(aov.out))
print(model.tables(aov.out, "means"))
with(adt, interaction.plot(Замер, Группа, Баллы, type = "b", pch = c(1, 2), ylim = range(Баллы), lwd = 3, col = rainbow(2)))

# Множественные сравнения
# Внутри групп
adtk <- adt[adt$Группа == "Контрольная",]
print(pairwise.wilcox.test(adtk$Баллы, adtk$Замер, paired = TRUE))
adte <- adt[adt$Группа == "Экспериментальная",]
print(pairwise.wilcox.test(adte$Баллы, adte$Замер, paired = TRUE))
# Между группами
print(sapply(dt[,4:7], FUN = function (x) wilcox.test(formula = x ~ dt$Группа, data = dt)))

# Подготовка данных к ANOVA с повторными измерениями
rdt <- reshape(data=dt[c(2,3,8,9)], idvar="Фамилия", varying=list(3:4), v.names="Баллы", direction="long")
colnames(rdt)[c(1, 3)] <- c("Испытуемый", "Замер")

# ANOVA с повторными измерениями для рейтингов
# Тест на гомогенность групп
bartlett.test(Баллы ~ Замер + Группа, data = rdt)
var.test(Баллы ~ Группа, data = rdt)
boxplot(Баллы ~ Замер + Группа, data = rdt)
aov.out <- aov(Баллы ~ (Группа * Замер) + Error(Испытуемый/(Замер), data = rdt))
print(summary(aov.out))
print(model.tables(aov.out, "means"))
with(rdt, interaction.plot(Замер, Группа, Баллы, type = "b", pch = c(1, 2), ylim = range(Баллы), lwd = 3, col = rainbow(2)))

# Множественные сравнения
# Внутри групп
rdtk <- rdt[rdt$Группа == "Контрольная",]
print(wilcox.test(rdtk$Баллы ~ rdtk$Замер, paired = TRUE))
rdte <- rdt[rdt$Группа == "Экспериментальная",]
print(wilcox.test(rdte$Баллы ~ rdte$Замер, paired = TRUE))
# Между группами
print(sapply(dt[,8:9], FUN = function (x) wilcox.test(formula = x ~ dt$Группа, data = dt)))

# Анализ распределения подтверждения разряда
cdt <- table(dt$Группа, dt$Подтверждение)
colnames(cdt) <- c("Не подтвердили", "Подтвердил")
print(chisq.test(cdt))