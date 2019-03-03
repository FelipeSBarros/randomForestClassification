# instalando as bibliotecas:
# install.packages("randomForest")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("raster")
library(randomForest)
library(sf)
library(tidyverse)
library(raster)

# Carregando os dados: ----
# raster
img <- stack(list.files("./LC08_L1TP_220068_20180906_20180912_01_T1/Reflectancia/", pattern = ".TIF$", full.names = T)[3:6])

plotRGB(img, 4, 3, 2, stretch = 'hist')
names(img) <- paste0(rep('band', nlayers(img)), 1:nlayers(img))

# amostras
amostras <- read_sf("./Amostras/Amostras.shp")
plot(amostras, add=T)

# Extração de dados: ----
valsTrain <- raster::extract(img, amostras)
head(valsTrain)

valsTrain <- data.frame(valsTrain, amostras$class)
head(valsTrain)
names(valsTrain)[ncol(valsTrain)] <- "class"

valsTrain$class <- as.factor(valsTrain$class)
class(valsTrain$class)

# Criando modelo randomForest ----
rf.mdl <- randomForest(valsTrain$class ~., data = valsTrain)
rf.mdl

getTree(rf.mdl, k=1)

varImpPlot(rf.mdl)

# Classificação ----
rf.class <- raster::predict(img, rf.mdl, progress = "text", type = "response")

# Salvando classificação: ----
writeRaster(rf.class, "rf_Classification.tif", overwrite = TRUE)
