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

# Amostras de treino de teste ----
teste <- amostras %>% group_by(class) %>% sample_n(size = 3)
treino <- amostras %>% filter(! id %in% (teste$id))
nrow(treino)
nrow(teste)

# Extração de dados: ----
valsTrain <- raster::extract(img, treino)
valsTest <- raster::extract(img, teste)

head(valsTrain)
head(valsTest)

valsTrain <- data.frame(valsTrain, treino$class)
valsTest <- data.frame(valsTest, teste$class)
head(valsTrain)
head(valsTest)
names(valsTrain)[ncol(valsTrain)] <- "class"
names(valsTest)[ncol(valsTest)] <- "class"

valsTrain$class <- as.factor(valsTrain$class)
valsTest$class <- as.factor(valsTest$class)
class(valsTrain$class)

# Criando modelo randomForest ----
rf.mdl <- randomForest(valsTrain$class ~., data = valsTrain, xtest = subset(valsTest, select = -class), ytest = valsTest$class, keep.forest = TRUE)
rf.mdl

getTree(rf.mdl, k=1)

varImpPlot(rf.mdl)

# Classificação ----
rf.class <- raster::predict(img, rf.mdl, progress = "text", type = "response")

# Salvando classificação: ----
writeRaster(rf.class, "rf_ClassificationCrossValidation.tif", overwrite = TRUE)
