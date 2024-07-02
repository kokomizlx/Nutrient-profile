rm(list = ls()) # 清空环境变量
pacman::p_load(sf,raster,ggplot2, ggspatial, tidyverse, rgdal, tmap)#所需要的包

library(raster)
library(rasterVis)
library(data.table)
SWPO_2020_Y <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "SWPO_A")) 

# 删除 SWPO_A 列中值为 0 的整行
SWPO_2020_Y <- SWPO_2020_Y[SWPO_A != 0]

# 将Yield(kg/ha)转化成营养成分（.../100g)
# 插列
# Sweer potato, boiled
SWPO_2020_Y$Water_g <- SWPO_2020_Y$SWPO_A*76*10
SWPO_2020_Y$Ca_g <- SWPO_2020_Y$SWPO_A*19*10
SWPO_2020_Y$Zn_g <- SWPO_2020_Y$SWPO_A*0.2*10

coords = SWPO_2020_Y[,c("x","y")] #坐标
cell_size = 0.083 #网格单元的大小，以度为单位
Yields = data.frame(SWPO_2020_Y$SWPO_A)
Y_sp <- SpatialPointsDataFrame(coords=coords, data=Yields) #存储空间点数据和相关属性数据的框架。它是sp包中的一个类
names(Y_sp) <- "Sweet potato_Yields_2020"

lon_min <- -159.708; lon_max <- 178.958; lat_min <- -47.2083; lat_max <- 69.9583
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1 #计算出网格的列数和行数，基于给定的经纬度范围和网格单元大小
grid <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=cell_size, crs="+proj=longlat +datum=WGS84")

Y_Water_g <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Water_g'], fun=mean)
Y_Ca_g<- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Ca_g'], fun=mean)
Y_Zn_g <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Zn_g'], fun=mean)

plot(Y_Water_g, main="Water_g",xlab="Longitude", ylab="Latitude")
plot(Y_Ca_g, main="Ca_g",xlab="Longitude", ylab="Latitude")
plot(Y_Zn_g, main="Zn_g",xlab="Longitude", ylab="Latitude")
