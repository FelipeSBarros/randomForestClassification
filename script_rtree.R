## instalando e carregando bibliotecas ----
# install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("sf")
#install.packages("tidyverse")
#install.packages("raster")
library(rpart.plot)
library(rpart)
library(sf)
library(raster)
library(tidyverse)

## Carregando dados ----
# Carregando img
img <- stack(list.files("./LC08_L1TP_220068_20180906_20180912_01_T1/Reflectancia/", pattern = ".TIF$", full.names = TRUE)[3:6])

plotRGB(img, 4, 3, 2, stretch = "hist")

names(img) <-
  paste0(rep('band', nlayers(img)),1:nlayers(img))

# Carregando amostras
amostras <- read_sf("./Amostras/Amostras.shp")

plot(amostras, add=TRUE)

# Amostras para treino e teste ----
teste <- amostras %>% group_by(class) %>% sample_n(size = 3)
amostras <- amostras %>% filter(!id %in%(teste$id))

# Extração dos dados ----
valsTrain <- raster::extract(img, amostras)
valsTest <- raster::extract(img, teste)

valsTrain <- data.frame(valsTrain, amostras$class)
valsTest <- data.frame(valsTest, teste$class)
head(valsTrain)

# Renomenado coluna class
colnames(valsTrain)[length(colnames(valsTrain))] <- "class"
colnames(valsTest)[length(colnames(valsTest))] <- "class"

valsTrain$class <- as.factor(valsTrain$class)
valsTest$class <- as.factor(valsTest$class)

# Modelo rpart ----
fitrpart <- rpart(valsTrain$class ~ .,data = valsTrain,
                  method="class", model = TRUE,  minsplit = 2, 
                  minbucket = 3)
print(fitrpart) # display the results 
summary(fitrpart) # detailed summary of splits

# plot tree 
prp(fitrpart, uniform=TRUE, 
    main="rpart tree")

rpart.plot(fitrpart, uniform=TRUE, 
           main="rpart tree")

# Predição ----
rpart_classification <- raster::predict(img, fitrpart, progress = "text", "class", overwrite = TRUE)

# Salvando resultado ----
writeRaster(rpart_classification, "rpart_classification.tif")
