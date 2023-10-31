setwd("/home/noel/Documentos/AprendizajeEstadistico/")
datos = read.csv("salud.csv", sep=",")
datos
menores_50 = subset(datos)
ncol(menores_50)
mean(menores_50$Cholesterol)
table(menores_50$Diet)
mean(menores_50$Income)
sd(menores_50$Income)
menores_50
mean(menores_50$Exercise.Hours.Per.Week)
mean(menores_50$Sedentary.Hours.Per.Day)
boxplot(menores_50$Exercise.Hours.Per.Week ~ menores_50$Obesity, data = menores_50, xlab = "Obesidad", ylab = "Horas de ejercicio por semana")
tabla = table(menores_50$Diet, menores_50$Obesity)
tabla
barplot(tabla, beside = TRUE, legend = rownames(tabla), xlab = "Obesidad", ylab = "Frecuencia", main = "Frecuencia de dieta por obesidad")
table(menores_50$Obesity)
library(help ="datasets")
