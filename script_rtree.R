## instalando e carregando bibliotecas ----
# install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("sf")
library(sf)
#install.packages("raster")
library(raster)


## Carregando dados ----
# Carregando img
img <- stack(list.files("./LC08_L1TP_220068_20180906_20180912_01_T1/Reflectancia/", pattern = ".TIF$", full.names = TRUE)[3:6])


plotRGB(img, 4, 3, 2, stretch = "hist")

names(img) <-
  paste0(rep('band', nlayers(img)),1:nlayers(img))

# Carregando amostras
amostras <- read_sf("./Amostras/Amostras.shp")

plot(amostras, add=TRUE)

# Extração dos dados ----
valsTrain <- raster::extract(img, amostras)

valsTrain <- data.frame(valsTrain, amostras$class)
head(valsTrain)

# Renomeando coluna class
colnames(valsTrain)[length(colnames(valsTrain))] <- "class"

valsTrain$class <- as.factor(valsTrain$class)

# Modelo rpart ----
fitrpart <- rpart(valsTrain$class ~ ., data = valsTrain,
                  method="class", minsplit = 2, 
                  minbucket = 3)

print(fitrpart)

rpart.plot::prp(fitrpart)

rpart.plot(fitrpart)

# predicao
rpart_pred <- raster::predict(img, fitrpart, progress = "text", type = "class")

plot(rpart_pred)

writeRaster(rpart_pred, "classificacao_rpart.tif")
