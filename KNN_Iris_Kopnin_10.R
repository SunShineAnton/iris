            #метод KNN
library(ggplot2)
library(gridExtra)
library(vroom)
library(class)
library(caret)
######
#устанавливаем рабочую директорию и парсим файлы данных
setwd("/Users/antonkopnin/Desktop/Учёба/Шлеев/Method_R_doc/Lab10")

#импорт данных
Date <- read.csv("iris.csv",header = TRUE,quote=",")

#изучаем структуру данных
str(Date)
head(Date)
tail(Date)
summary(Date) # сводная таблица с опистельными статистиками
#можно заметить, что данные необходимо обработать

#обработка данных
iris <-as.data.frame(sapply(Date, function(x) gsub("\"", "", x)))

#повторно смотрим структуру данных
str(iris)
head(iris)
tail(iris)
summary(iris) # сводная таблица с опистельными статистиками

#проверка на наличие пропущенных значений (NA)
na_check <- function(iris){
  if (sum(is.na(iris)) > 0) 
    print(paste(c("Обнаружено"), round(sum(is.na(iris)), digits = 2), c("пропущенных значений")))
  else
    print("Пропущенных данных не обнаружено")
}

na_check(iris)

#но корректно оторьарить таблтцу у меня не получилось, поэтому взять данные ириса из r
######

#подключение данных
data(iris)

a <- qplot(Sepal.Length,Sepal.Width, data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "green", se = FALSE) 

b <- qplot(Petal.Length, Petal.Width,data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "blue", se = FALSE)

grid.arrange(a, b, nrow = 2)

#метод KNN (к-ближайших соседей)
set.seed(3) #генерация случайных чисел

iris$Species <- factor(iris$Species) # преобразуем столбец с меткой класса в фактор

class_vector <- iris[, 5] #столбец в виде фактора с метками класса операции для обучающих данных
iris <- as.data.frame(scale(iris[, -5])) # провели масштабирование данных за искючением столбца с меткой класса операции
iris <- data.frame(iris, class_vector)

# проверка данных на отмасштабирование
str(iris)

# делим дата-сет на обучающую и тестовую выборки
index <- sample(1:nrow(iris), round(0.6*nrow(iris)))
train <- iris[index, ]
test <- iris[-index, ]

knn.iris <- class::knn(train = train[, -5], test = test[, -5], 
                       cl = train[, "class_vector"], k = 5, prob = TRUE)
(table(Факт = test$Species, Прогноз = knn.iris))

Toshnost <- mean(knn.iris == test$class_vector)
paste("Точность= ", round(100*Acc, 2), "%", sep = "")

print (paste("Точность= ", round(100*Acc, 2), "%", sep = ""))

confusionMatrix(knn.iris, test$class_vector, positive = "1")
