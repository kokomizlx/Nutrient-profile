rm(list = ls()) # 清空环境变量
setwd("path/r") # 设置工作目录
pacman::p_load(sf,raster,ggplot2, ggspatial, tidyverse, rgdal, tmap)#所需要的包
shp <- sf::read_sf("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/World_countries.shp")#读入矢量边界（实际读入的是shx文件）
baseDir<- "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/3_results/Sweet potato" #存储路径

### CSV to RASTER
library(raster)
library(rasterVis)
SWPO_2020_Y <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "SWPO_A")) 
SWPO_2020_Y_2 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "SWPO_A")) 

coords = SWPO_2020_Y[,c("x","y")] #坐标
Yields = data.frame(SWPO_2020_Y$SWPO_A)
Y_pts <- SpatialPointsDataFrame(coords=coords, data=Yields)
names(Y_pts) <- "Sweet potato_Yields_2020"

cell_size = 0.083 #网格单元的大小，以度为单位
lon_min <- -159.708; lon_max <- 178.958; lat_min <- -47.2083; lat_max <- 69.9583
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1 #计算出网格的列数和行数，基于给定的经纬度范围和网格单元大小
grid <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=cell_size, crs="+proj=longlat +datum=WGS84")

Y <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'SWPO_A'], fun=mean)
plot(Y)

# 删除 SWPO_A 列中值为 0 的整行
SWPO_2020_Y_2 <- SWPO_2020_Y_2[SWPO_A != 0]
coords_2 = SWPO_2020_Y_2[,c("x","y")] #坐标
Yields_2 = data.frame(SWPO_2020_Y_2$SWPO_A)
Y_pts_2 <- SpatialPointsDataFrame(coords=coords_2, data=Yields_2)
names(Y_pts_2) <- "Sweet potato_Yields_2020_2"

Y_2 <- rasterize(SWPO_2020_Y_2[, c("x", "y")], grid, SWPO_2020_Y_2[, 'SWPO_A'], fun=mean)
plot(Y_2)

#和实际raster对比
refer_SWPO_2020_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Sweet potato/SPAM_2020_global_Y_SWPO_A.tif")
plot(refer_SWPO_2020_Y)

library(RColorBrewer)
plot(us_fire, col=brewer.pal(9,"PuBuGn"), sub="Sweet potato_Yield")

### CSV to Shp
## SP
# 导入csv
library("data.table")    
SWPO_2020_Y <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "SWPO_A")) 
coords = SWPO_2020_Y[,c("x","y")] #坐标
Yields = data.frame(SWPO_2020_Y$SWPO_A)

# wgs1984 at https://spatialreference.org/ref/epsg/wgs-84/proj4
wgs1984 = sp::CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs") #Coordinate Reference System
pointsSP = sp::SpatialPointsDataFrame(coords = coords,
                                   data = SWPO_2020_Y,
                                   proj4string = wgs1984)
tmap::qtm(pointsSP)

# write out the file
sf::st_write(sf::st_as_sf(pointsSP), paste0(baseDir,"/fiel2.shp"), driver = "ESRI Shapefile")

library(raster)
# Read this shape file with the sf library.
library(sf)
my_sf <- read_sf(paste0(baseDir,"/fiel2.shp"))

# Plot it
library(ggplot2)
ggplot(my_sf["SWPO_A"]) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()

map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
ggplot(my_sf)
plot(my_sf)

plot(my_sf["SWPO_A"], breaks = "jenks")

## SF
# SWPO_2020_Y 是你的数据框，其中包含了要转换的数据。
# coords = c("x", "y") 指定了经度和纬度在数据框中的列名。
# crs = 4326 设置了坐标系为 WGS 84，即经纬度坐标系
pointsSF <- sf::st_as_sf(SWPO_2020_Y, coords = c("x", "y"), crs = 4326)
class(pointsSF)
tmap::qtm(pointsSF)
sf::st_write(pointsSF, paste0(baseDir,"/file1.shp"))

# 导入原始栅格
SWPO_2020_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Sweet potato/SPAM_2020_global_Y_SWPO_A.tif")
