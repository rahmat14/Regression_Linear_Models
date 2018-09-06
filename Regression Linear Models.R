#Install Packages
library(ggplot2)
library(xlsx)
library(car)

#Setting Lokasi Analisis Data 
setwd("C:/Users/ASUS/Documents/Folder Data")
data <- read.xlsx("DataLatihanRegresiLineraBerganda.xlsx", sheetName = "Sheet1")

#Preparetion Data
head(data)
View(data)
summary()

#Nilai Korelasi antar Variabel  
cor(data$Jam.Belajar..Jam. , data$Uang.Jajan..Ribu.Rupiah. , method = "pearson")
cor(data$Jam.Belajar..Jam. , data$Indeks.Prestasi , method = "pearson")
cor(data$Uang.Jajan..Ribu.Rupiah. , data$Indeks.Prestasi , method = "pearson")
ggplot(data, aes(data$Jam.Belajar..Jam. , data$Indeks.Prestasi)) + geom_point()
ggplot(data, aes(data$Uang.Jajan..Ribu.Rupiah. , data$Indeks.Prestasi)) + geom_point()

#Model Regresi Linear
RegresiLinear<- lm(formula = Indeks.Prestasi ~ data$Jam.Belajar..Jam. + data$Uang.Jajan..Ribu.Rupiah. , data = data)
summary(RegresiLinear)
b0 = RegresiLinear$coefficients[1]
b1 = RegresiLinear$coefficients[2]
b2 = RegresiLinear$coefficients[3]

#Menguji kecocokan model regresi linear
ModelRegresi = b0 + b1*data$Jam.Belajar..Jam. + b2*data$Uang.Jajan..Ribu.Rupiah.
Residual.model <- resid(RegresiLinear)
qqPlot(Residual.model, dist = "norm", main = "normal qq model")
vif(RegresiLinear)
Hasil.Estimasi <- ModelRegresi
Tabel = cbind(data, Hasil.Estimasi)
View(Tabel)

#Prediksi data menggunakan model Regresi Linear 
prediksi <- read.xlsx("DataTestingRegresi.xlsx", sheetName = "Sheet1")
ModelRegresi = b0 + b1*prediksi$Jam.Belajar..Jam. + b2*prediksi$Uang.Jajan..Ribu.Rupiah.
hasil.prediksi <- ModelRegresi
prediksi$Indeks.Prestasi <- hasil.prediksi
View(prediksi)
