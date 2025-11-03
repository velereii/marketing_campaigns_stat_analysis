# 1. Выявление выбросов в наборе данных
sales <- my_dataset$SalesInThousands
boxplot(sales)

# Находим выбросы методом Тьюки
lb <- quantile(sales,0.25, na.rm = TRUE) - 1.5*IQR(sales, na.rm = TRUE)
lb
rb <- quantile(sales,0.75, na.rm = TRUE) + 1.5*IQR(sales, na.rm = TRUE)
rb
vybrosy <- subset(sales, sales < lb | sales > rb)
vybrosy
# 19.26, 17.34 - выбросы
# Подвыборка без выбросов:
sales1 <- subset(sales, sales >= lb & sales <= rb)
boxplot(sales1)

# Проверяем выборку на нормальность
shapiro.test(sales)

# Находим выбросы методом z-оценок
vybrosy_z <- subset(sales, abs((sales-mean(sales))/sd(sales))>3)
vybrosy_z
# Выбросы не обнаружены



# 2.1
# Используем критерий Шапиро–Уилка
shapiro.test(sales)
shapiro.test(my_dataset$AgeOfStore)

# Используем критерий Крамера–фон Мизеса
install.packages('nortest') 
library('nortest')
cvm.test(sales)
cvm.test(my_dataset$AgeOfStore)

# Построение гистограммы и графика Q-Q Plot
hist(sales)
install.packages("car")
library(car)
qqPlot(sales, pch = 19)

hist(my_dataset$AgeOfStore)
qqPlot(my_dataset$AgeOfStore, pch = 19)



# 2.2
# 2.2.1 Точечные оценки параметров распределений
# Выборочное среднее
mean(sales)
# Выборочная дисперсия
var(sales)
# Выборочное среднеквадратическое отклонение
sd(sales)
# Выборочная медианa
median(sales)
# Выборочный коэффициент асимметрии A:
install.packages("e1071")
library(e1071)
skewness(sales)
# A < 0, следовательно левый хвост распределения «тяжелее».
# Выборочный коэффициент эксцесса E:
kurtosis(sales)
# E < 0, следовательно хвосты распред-ия «тяжелее», а пик более «приплюснутый», чем у нормального распp-ия.

# Выборочное среднее
mean(my_dataset$AgeOfStore)
# Выборочная дисперсия
var(my_dataset$AgeOfStore)
# Выборочное среднеквадратическое отклонение
sd(my_dataset$AgeOfStore)
# Выборочная медианa
median(my_dataset$AgeOfStore)
# Выборочный коэффициент асимметрии A:
install.packages("e1071")
library(e1071)
skewness(my_dataset$AgeOfStore)
# A > 0, следовательно правый хвост распределения «тяжелее».
# Выборочный коэффициент эксцесса E:
kurtosis(my_dataset$AgeOfStore)
# E > 0, следовательно хвосты распред-ия «легче», а пик острее, чем у нормального распределения.


# 2.2.2 Интервальные оценки неизвестных параметров нормального распределения
# Доверительный интервал
t.test(sales, conf.level = 0.95, alternative = "two.sided")$conf.int
t.test(my_dataset$AgeOfStore, conf.level = 0.95, alternative = "two.sided")$conf.int


# 2.3 Проверка гипотез
# 2.3.1
# Проверка на нормальность
small <- subset(my_dataset, my_dataset$MarketSize=='Small')
shapiro.test(small$SalesInThousand)
large <- subset(my_dataset, my_dataset$MarketSize=='Large')
shapiro.test(large$SalesInThousand)
# Проверка равенства двух дисперсий
var.test(small$SalesInThousands, large$SalesInThousands, conf.level = 0.95, alternative = "two.sided")
# Проверка равенства математических ожиданий
t.test(large$SalesInThousands, small$SalesInThousands, mu=0, alternative = "greater", var.equal = TRUE)

# 2.3.2
kruskal.test(my_dataset, my_dataset$Promotion)

# 2.3.3
# Проверка на нормальность
promo1 <- subset(my_dataset, my_dataset$Promotion==1)
shapiro.test(promo1$SalesInThousand)
promo2 <- subset(my_dataset, my_dataset$Promotion==2)
shapiro.test(promo2$SalesInThousand)
# Проверка равенства медиан
wilcox.test(promo1$SalesInThousands, promo2$SalesInThousands, mu=0, conf.level = 0.95, alternative = "greater")

# 2.3.4
shapiro.test(my_dataset$AgeOfStore)
prop.test(length(subset(my_dataset$AgeOfStore, my_dataset$AgeOfStore>10)), length(my_dataset$AgeOfStore), p=0.5, conf.level = 0.95, alternative = "less")

# 2.3.5
kruskal.test(my_dataset, my_dataset$week)


# 3. Построение парной и множественной регрессии
# 3.1
#Коэффициент корреляции для SalesInThousands и AgeOfStore
cor(my_dataset$SalesInThousands, my_dataset$AgeOfStore, method = "pearson")
# cor= 0.1022755, корреляция слабая положительная
plot(SalesInThousands~AgeOfStore, my_dataset)

#Коэффициент корреляции для SalesInThousands и week
cor(my_dataset$SalesInThousands, my_dataset$week, method = "pearson")
# cor= -0.02801419, корреляция слабая отрицательная
plot(SalesInThousands~week, my_dataset)

#Коэффициент корреляции для SalesInThousands и Promotion
cor(my_dataset$SalesInThousands, my_dataset$Promotion, method = "pearson")
# cor= 0.2649495, корреляция положительная
plot(SalesInThousands~LocationID, my_dataset)

#Коэффициент корреляции для SalesInThousands и LocationID
cor(my_dataset$SalesInThousands, my_dataset$LocationID, method = "pearson")
# cor= 0.2649495, корреляция положительная
plot(SalesInThousands~LocationID, my_dataset)

# Попарная диаграмма рассеяния 
install.packages("corrgram")
library("corrgram")
corrgram(my_dataset, lower.panel=panel.conf, upper.panel=panel.pts)

# 3.1
# Mодель парной линейной регрессии для SalesInThousands и AgeOfStore
lm(SalesInThousands~AgeOfStore, my_dataset)
# Уравнение парной регрессии: SalesInThousands = 46.2870 + 0.1558*AgeOfStore
summary(lm(SalesInThousands~AgeOfStore, my_dataset))
# Pr(>|t|) < 0.05 -> модель значима 
# Коэффициент детерминации 0.01046 -> 1,04% изменения количества продаж объясняется тем, как давно магазин уже функционирует
plot(SalesInThousands~AgeOfStore, my_dataset)
abline(lm(SalesInThousands~AgeOfStore, my_dataset), ,col="red")

# Проверка предпосылок регрессионного анализа
# Проверка равенства среднего остатков нулю
toch_znach <- lm(SalesInThousands~AgeOfStore, my_dataset)$residuals
toch_znach
# Графическая интерпретация остатков
plot(toch_znach)
t.test(toch_znach, mu=0, conf.level=0.95, alternative="t")
# Так как p-value > 0.05, не отвергаем гипотезу о том, что среднее остатков = 0

# Проверка гомоскедастичности:
# H0: дисперсия = const
# H1: дисперсия != const (то есть с ростом значений х разброс значений у меняется)
install.packages("lmtest")
library(lmtest)
bptest(lm(SalesInThousands~AgeOfStore, my_dataset), studentize = FALSE)
# p-value < 0,05 -> H0 отвергаем, принимаем H1: дисперсия != const
gqtest(lm(SalesInThousands~AgeOfStore, my_dataset), alternative = "t")
# p-value < 0,05 -> H0 отвергаем, принимаем H1: дисперсия != const
# Гомоскедастичность отсутствует

# Проверка автокорреляции:
# H0: нет автокорреляции
# H1: есть автокорреляция
dwtest(lm(SalesInThousands~AgeOfStore, my_dataset), alternative = "t")
# p-value < 0,05 -> остатки друг от друга зависят, есть автокорреляции
bgtest(lm(SalesInThousands~AgeOfStore, my_dataset), order = 5) 
# p-value < 0,05 -> остатки друг от друга зависят, есть автокорреляции

# Проверка на нормальность остатков:
shapiro.test(toch_znach)
# Остатки нормальные, предпосылки регр. анализа выполнены



# Mодель парной линейной регрессии для SalesInThousands и week
lm(SalesInThousands~week, my_dataset)
# Уравнение парной регрессии: SalesInThousands = 48.2874 - 0.2553*week
summary(lm(SalesInThousands~week, my_dataset))
# Pr(>|t|) < 0.05 -> модель значима 
# Коэффициент детерминации 0.0007848 -> 0,08% изменения количества продаж объясняется тем, на какой неделе проводилась марк. кампания
plot(SalesInThousands~week, my_dataset)
abline(lm(SalesInThousands~week, my_dataset), ,col="red")

# Проверка предпосылок регрессионного анализа
# Проверка равенства среднего остатков нулю
toch_znach <- lm(SalesInThousands~week, my_dataset)$residuals
toch_znach
# Графическая интерпретация остатков
plot(toch_znach)
t.test(toch_znach, mu=0, conf.level=0.95, alternative="t")
# Так как p-value > 0.05, не отвергаем гипотезу о том, что среднее остатков = 0

# Проверка гомоскедастичности:
# H0: дисперсия = const
# H1: дисперсия != const (то есть с ростом значений х разброс значений у меняется)
install.packages("lmtest")
library(lmtest)
bptest(lm(SalesInThousands~week, my_dataset), studentize = FALSE)
# p-value > 0,05 -> H0 принимаем: дисперсия = const
gqtest(lm(SalesInThousands~week, my_dataset), alternative = "t")
# p-value < 0,05 -> H0 отвергаем, принимаем H1: дисперсия != const
# Гомоскедастичность присутствует

# Проверка автокорреляции:
# H0: нет автокорреляции
# H1: есть автокорреляция
dwtest(lm(SalesInThousands~week, my_dataset), alternative = "t")
# p-value < 0,05 -> остатки друг от друга зависят, есть автокорреляции
bgtest(lm(SalesInThousands~week, my_dataset), order = 5) 
# p-value < 0,05 -> остатки друг от друга зависят, есть автокорреляции

# Проверка на нормальность остатков:
shapiro.test(toch_znach)
# Остатки нормальные, предпосылки регр. анализа выполнены



# Дисперсионный анализ для SalesInThousands  и Promotion 
mean(my_dataset$SalesInThousands)
Promotion1 <- subset(my_dataset$SalesInThousands, my_dataset$Promotion==1)
Promotion2 <- subset(my_dataset$SalesInThousands, my_dataset$Promotion==2)
Promotion3 <- subset(my_dataset$SalesInThousands, my_dataset$Promotion==3)
mean(Promotion1) # 51.96465
mean(Promotion2) # 43.1209
mean(Promotion3) # 48.53654

#Предпосылки дисперсионного анализа
#Проверка выборок на нормальность
shapiro.test(Promotion1)
shapiro.test(Promotion2)
shapiro.test(Promotion3)
# Данные в двух из трех выборках распределены ненормально, поэтому нельзя применять дисперсионный анализ

#Проверка равенства дисперсий
# H0: равные дисперсии
# H1: не равные дисперсии
bartlett.test(my_dataset$SalesInThousands, my_dataset$Promotion)
# p-value > 0.05 -> дисперсии равны

aov(my_dataset$SalesInThousands~my_dataset$Promotion)
aov(my_dataset$SalesInThousands~my_dataset$Promotion)$coefficients
aov(my_dataset$SalesInThousands~my_dataset$Promotion)$fitted.values
aov(my_dataset$SalesInThousands~as.factor(my_dataset$Promotion))$coefficients
aov(my_dataset$SalesInThousands~as.factor(my_dataset$Promotion)$fitted.values
aov(my_dataset$SalesInThousands~as.factor(my_dataset$Promotion)$residuals

rr <- aov(my_dataset$SalesInThousands~as.factor(my_dataset$Promotion))$residuals
mean(rr)
t.test(rr,mu=0)
shapiro.test(rr)
# Cреднее остатков равняется нулю и распределены не нормально -> выводы дисперсионного анализа не корректны


# 3.2
# Модель множественной регрессии: зависимость количества продаж от возраста магазина, типа марк. кампании и недели, на которой она проводилась
lm(SalesInThousands~Promotion+AgeOfStore+week, my_dataset)
summary(lm(SalesInThousands~Promotion+AgeOfStore+week, my_dataset))
# Adjusted R-squared = 0.024, изменение  колличества продаж на 2,4% объясняется совокупным изменением возраста магазинатипа марк. кампании и недели, на которой она проводилась
# В большей степени количество продаж зависит от типа маркетинговой кампании
# Коэффициент детерминации:  0.03, изменения итоговой цены на 3% объясняются совокупным влиянием всех исследуемых факторов


# Проверка предпосылок регрессионного анализа
# Проверка равенства среднего остатков нулю
# Точные значения остатков
ost <- lm(SalesInThousands~Promotion+AgeOfStore+week, my_dataset)$residuals
ost
plot(ost)
# Графическая интерпретация остатков: видно, что среднее близко к нулю
t.test(ost, mu = 0, conf.level = 0.95, alternative = "t")
# Нет оснований отвергнуть гипотезу, что среднее остатков равно нулю

# Проверка гомоскедастичности:
# H0: дисперсия = const
# H1: дисперсия != const (то есть с ростом значений х разброс значений у меняется)
install.packages("lmtest")
library(lmtest)
bptest(lm(SalesInThousands~as.factor(Promotion)+as.factor(AgeOfStore)+as.factor(week), data=my_dataset), studentize = FALSE)
# Breusch-Pagan test: p-value < 0,05, H0 отвергаем
gqtest(lm(SalesInThousands~as.factor(Promotion)+as.factor(AgeOfStore)+as.factor(week), data=my_dataset), alternative = "t")
#Goldfeld-Quandt test: p-value < 0,05, H0 отвергаем
#отсутствует гомоскедастичность

plot(lm(SalesInThousands~as.factor(Promotion)+as.factor(AgeOfStore)+as.factor(week), data=my_dataset))

# Проверка автокорреляции
dwtest(lm(SalesInThousands~as.factor(Promotion)+as.factor(AgeOfStore)+as.factor(week), data=my_dataset), alternative = "t")
# Durbin-Watson test: остатки друг от друга зависят, есть автокорреляция
bgtest(lm(SalesInThousands~as.factor(Promotion)+as.factor(AgeOfStore)+as.factor(week), data=my_dataset), order = 5) 
# Остатки друг от друга зависят, есть автокорреляция

# Проверка на нормальность остатков
shapiro.test(ost)
#p-value > 0.05  остатки нормальные

# Мультиколлинеарность
install.packages("car")
library(car)
vif(lm(SalesInThousands~Promotion+AgeOfStore+week, my_dataset))
# Все коэффициенты меньше 5 -> отсутствует мультиколлинеарность


#Качество модели множественной регрессии
install.packages("MLmetrics")
library(MLmetrics)
MAPE(lm(SalesInThousands~Promotion+AgeOfStore+week, my_dataset)$fitted.value, my_dataset$SalesInThousands)*100
# Средняя ошибка аппроксимации =  19.04119% >10%. Плохой подбор модели



