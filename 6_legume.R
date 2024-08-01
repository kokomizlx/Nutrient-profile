rm(list = ls()) # 清空环境变量
pacman::p_load(sf,raster,ggplot2, ggspatial, tidyverse, rgdal, tmap)#所需要的包
library(raster)
library(rasterVis)
library(data.table)

cell_size = 0.083 # 网格单元大小，以度为单位
lon_min <- -159.708; lon_max <- 178.958; lat_min <- -47.2083; lat_max <- 69.9583
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1 #计算出网格的列数和行数，基于给定的经纬度范围和网格单元大小
grid <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=cell_size, crs="+proj=longlat +datum=WGS84")
world <- map_data("world") # 获取世界地图坐标

# Data input
# 用的都是Yield数据
# 20 Soybean
soybean_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "soyb"))
soybean_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "soyb_a"))
soybean_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "SOYB_A"))

soybean_2000 <- soybean_2000[soyb != 0]
soybean_2010 <- soybean_2010[soyb_a != 0]
soybean_2020 <- soybean_2020[SOYB_A != 0]

coords_2000 = soybean_2000[,c("x","y")] #坐标
coords_2010 = soybean_2010[,c("x","y")]
coords_2020 = soybean_2020[,c("x","y")]

# 0300299	Soybean, seeds, dried, raw
soybean_2000$Water_g <- soybean_2000$soyb*9.5*10
soybean_2010$Water_g <- soybean_2010$soyb_a*9.5*10
soybean_2020$Water_g <- soybean_2020$SOYB_A*9.5*10

soybean_2000$fatty_acids_g <- soybean_2000$soyb*17.92*10
soybean_2010$fatty_acids_g <- soybean_2010$soyb_a*17.92*10
soybean_2020$fatty_acids_g <- soybean_2020$SOYB_A*17.92*10

Yields_soybean_2000 = data.frame(soybean_2000$soyb)
Yields_soybean_2010 = data.frame(soybean_2010$soyb_a)
Yields_soybean_2020 = data.frame(soybean_2020$SOYB_A)

Yields_soybean_2000_sp <- SpatialPointsDataFrame(coords=coords_2000, data=Yields_soybean_2000) #存储空间点数据和相关属性数据的框架。它是sp包中的一个类
Yields_soybean_2010_sp <- SpatialPointsDataFrame(coords=coords_2010, data=Yields_soybean_2010) 
Yields_soybean_2000_sp <- SpatialPointsDataFrame(coords=coords_2000, data=Yields_soybean_2000) 

Water_soybean_2000 <- rasterize(soybean_2000[, c("x", "y")], grid, soybean_2000[, 'Water_g'], fun=mean)
Water_soybean_2010 <- rasterize(soybean_2010[, c("x", "y")], grid, soybean_2010[, 'Water_g'], fun=mean)
Water_soybean_2020 <- rasterize(soybean_2020[, c("x", "y")], grid, soybean_2020[, 'Water_g'], fun=mean)

Fattyacids_soybean_2000 <- rasterize(soybean_2000[, c("x", "y")], grid, soybean_2000[, 'fatty_acids_g'], fun=mean)
Fattyacids_soybean_2010 <- rasterize(soybean_2010[, c("x", "y")], grid, soybean_2010[, 'fatty_acids_g'], fun=mean)
Fattyacids_soybean_2020 <- rasterize(soybean_2020[, c("x", "y")], grid, soybean_2020[, 'fatty_acids_g'], fun=mean)

# Water
plot(Water_soybean_2000, main="Water_soybean_2000", xlab="Longitude", ylab="Latitude") # 首先绘制图形
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_soybean_2000.png", width = 800, height = 500)  # 然后将图形保存到特定路径
plot(Water_soybean_2000, main="Water_soybean_2000", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Water_soybean_2010, main="Water_soybean_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_soybean_2010.png", width = 800, height = 500) 
plot(Water_soybean_2010, main="Water_soybean_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Water_soybean_2020, main="Water_soybean_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_soybean_2020.png", width = 800, height = 500) 
plot(Water_soybean_2020, main="Water_soybean_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fatty acids
plot(Fattyacids_soybean_2000, main="Fattyacids_soybean_2000", xlab="Longitude", ylab="Latitude") # 首先绘制图形
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fattyacids_soybean_2000.png", width = 800, height = 500)  # 然后将图形保存到特定路径
plot(Fattyacids_soybean_2000, main="Fattyacids_soybean_2000", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fattyacids_soybean_2010, main="Fattyacids_soybean_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fattyacids_soybean_2010.png", width = 800, height = 500) 
plot(Fattyacids_soybean_2010, main="Fattyacids_soybean_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fattyacids_soybean_2020, main="Fattyacids_soybean_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fattyacids_soybean_2020.png", width = 800, height = 500) 
plot(Fattyacids_soybean_2020, main="Fattyacids_soybean_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# 17 Pigeon Pea
# pige_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
pige_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "pige_a"))
pige_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "PIGE_A")) 

pige_2010 <- pige_2010[pige_a != 0]
pige_2020 <- pige_2020[PIGE_A != 0]

# 0300529	Pigeon pea, seeds, raw
# Energy
pige_2010$Energy_kJ <- pige_2010$pige_a*1439.86*10
pige_2020$Energy_kJ <- pige_2020$PIGE_A*1439.86*10

Energy_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Energy_kJ'], fun=mean)
Energy_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Energy_kJ'], fun=mean)

plot(Energy_pige_2010, main="Energy_Pigeon pea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Energy_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Energy_pige_2010, main="Energy_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Energy_pige_2020, main="Energy_Pigeon pea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Energy_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Energy_pige_2020, main="Energy_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Water
pige_2010$Water_g <- pige_2010$pige_a*11.45*10
pige_2020$Water_g <- pige_2020$PIGE_A*11.45*10

Water_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Water_g'], fun=mean)
Water_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Water_g'], fun=mean)

plot(Water_pige_2010, main="Water_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Water_pige_2010, main="Water_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Water_pige_2020, main="Water_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Water_pige_2020, main="Water_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Protein
pige_2010$Protein_g <- pige_2010$pige_a*18.96*10 # protein, total; calculated from total nitrogen 
pige_2020$Protein_g <- pige_2020$PIGE_A*18.96*10

Protein_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Protein_g'], fun=mean)
Protein_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Protein_g'], fun=mean)

plot(Protein_pige_2010, main="Protein_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Protein_pige_2010, main="Protein_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Protein_pige_2020, main="Protein_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Protein_pige_2020, main="Protein_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fat
pige_2010$Fat_g <- pige_2010$pige_a*2.13*10 # fat, total; derived by analysis using continuous extraction
pige_2020$Fat_g <- pige_2020$PIGE_A*2.13*10

Fat_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Fat_g'], fun=mean)
Fat_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Fat_g'], fun=mean)

plot(Fat_pige_2010, main="Fat_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Fat_pige_2010, main="Fat_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fat_pige_2020, main="Fat_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Fat_pige_2020, main="Fat_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Carbohydrate
pige_2010$Carbohydrate_g <- pige_2010$pige_a*64*10
pige_2020$Carbohydrate_g <- pige_2020$PIGE_A*64*10

Carbohydrate_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Carbohydrate_g'], fun=mean)
Carbohydrate_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Carbohydrate_g'], fun=mean)

plot(Carbohydrate_pige_2010, main="Carbohydrate_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Carbohydrate_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Carbohydrate_pige_2010, main="Carbohydrate_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Carbohydrate_pige_2020, main="Carbohydrate_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Carbohydrate_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Carbohydrate_pige_2020, main="Carbohydrate_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fibre
pige_2010$Fibre_g <- pige_2010$pige_a*21.31*10 # fibre, total dietary; determined gravimetrically by the AOAC total dietary fibre method 
pige_2020$Fibre_g <- pige_2020$PIGE_A*21.31*10 # (Prosky and similar methods)

Fibre_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Fibre_g'], fun=mean)
Fibre_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Fibre_g'], fun=mean)

plot(Fibre_pige_2010, main="Fibre_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Fibre_pige_2010, main="Fibre_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fibre_pige_2020, main="Fibre_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Fibre_pige_2020, main="Fibre_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Mineral elements
# Ca
pige_2010$Ca_mg <- pige_2010$pige_a*129.34*10
pige_2020$Ca_mg <- pige_2020$PIGE_A*129.34*10

Ca_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Ca_mg'], fun=mean)
Ca_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Ca_mg'], fun=mean)

plot(Ca_pige_2010, main="Ca_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Ca_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Ca_pige_2010, main="Ca_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Ca_pige_2020, main="Ca_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Ca_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Ca_pige_2020, main="Ca_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# P
pige_2010$P_mg <- pige_2010$pige_a*269.19*10
pige_2020$P_mg <- pige_2020$PIGE_A*269.19*10

P_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'P_mg'], fun=mean)
P_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'P_mg'], fun=mean)

plot(P_pige_2010, main="P_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/P_Pigeon pea_2010.png", width = 800, height = 500) 
plot(P_pige_2010, main="P_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(P_pige_2020, main="P_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/P_Pigeon pea_2020.png", width = 800, height = 500) 
plot(P_pige_2020, main="P_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Mg
pige_2010$Mg_mg <- pige_2010$pige_a*166*10
pige_2020$Mg_mg <- pige_2020$PIGE_A*166*10

Mg_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Mg_mg'], fun=mean)
Mg_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Mg_mg'], fun=mean)

plot(Mg_pige_2010, main="Mg_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Mg_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Mg_pige_2010, main="Mg_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Mg_pige_2020, main="Mg_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Mg_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Mg_pige_2020, main="Mg_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# K
pige_2010$K_mg <- pige_2010$pige_a*1214.8*10
pige_2020$K_mg <- pige_2020$PIGE_A*1214.8*10

K_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'K_mg'], fun=mean)
K_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'K_mg'], fun=mean)

plot(K_pige_2010, main="K_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/K_Pigeon pea_2010.png", width = 800, height = 500) 
plot(K_pige_2010, main="K_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(K_pige_2020, main="K_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/K_Pigeon pea_2020.png", width = 800, height = 500) 
plot(K_pige_2020, main="K_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Na
pige_2010$Na_mg <- pige_2010$pige_a*1.62*10
pige_2020$Na_mg <- pige_2020$PIGE_A*1.62*10

Na_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Na_mg'], fun=mean)
Na_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Na_mg'], fun=mean)

plot(Na_pige_2010, main="Na_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Na_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Na_pige_2010, main="Na_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Na_pige_2020, main="Na_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Na_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Na_pige_2020, main="Na_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Trace elements
# Fe
pige_2010$Fe_mg <- pige_2010$pige_a*1.94*10
pige_2020$Fe_mg <- pige_2020$PIGE_A*1.94*10

Fe_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Fe_mg'], fun=mean)
Fe_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Fe_mg'], fun=mean)

plot(Fe_pige_2010, main="Fe_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fe_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Fe_pige_2010, main="Fe_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fe_pige_2020, main="Fe_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fe_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Fe_pige_2020, main="Fe_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Cu
pige_2010$Cu_mg <- pige_2010$pige_a*0.57*10
pige_2020$Cu_mg <- pige_2020$PIGE_A*0.57*10

Cu_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Cu_mg'], fun=mean)
Cu_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Cu_mg'], fun=mean)

plot(Cu_pige_2010, main="Cu_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Cu_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Cu_pige_2010, main="Cu_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Cu_pige_2020, main="Cu_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Cu_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Cu_pige_2020, main="Cu_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Zn
pige_2010$Zn_mg <- pige_2010$pige_a*2.02*10
pige_2020$Zn_mg <- pige_2020$PIGE_A*2.02*10

Zn_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Zn_mg'], fun=mean)
Zn_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Zn_mg'], fun=mean)

plot(Zn_pige_2010, main="Zn_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Zn_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Zn_pige_2010, main="Zn_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Zn_pige_2020, main="Zn_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Zn_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Zn_pige_2020, main="Zn_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

#Vitamin 暂时是加起来
pige_2010$Vitamin_mg <- pige_2010$pige_a*(1.06+2.69+0.07+1.47)*10
pige_2020$Vitamin_mg <- pige_2020$PIGE_A*(1.06+2.69+0.07+1.47)*10

Vitamin_pige_2010 <- rasterize(pige_2010[, c("x", "y")], grid, pige_2010[, 'Vitamin_mg'], fun=mean)
Vitamin_pige_2020 <- rasterize(pige_2020[, c("x", "y")], grid, pige_2020[, 'Vitamin_mg'], fun=mean)

plot(Vitamin_pige_2010, main="Vitamin_pige_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Vitamin_Pigeon pea_2010.png", width = 800, height = 500) 
plot(Vitamin_pige_2010, main="Vitamin_Pigeon pea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Vitamin_pige_2020, main="Vitamin_pige_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Vitamin_Pigeon pea_2020.png", width = 800, height = 500) 
plot(Vitamin_pige_2020, main="Vitamin_Pigeon pea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# 18 Lentil
# lentil_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# lentil_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                      select = c("x", "y", "lent_a"))
lentil_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "lent_a"))
lentil_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "LENT_A"))

lentil_2010 <- lentil_2010[lent_a != 0]
lentil_2020 <- lentil_2020[LENT_A != 0]

# 0300198	Lentil, seeds, raw
lentil_2010$Water_g <- lentil_2010$lent_a*10.4*10
lentil_2020$Water_g <- lentil_2020$LENT_A*10.4*10

Water_lentil_2010 <- rasterize(lentil_2010[, c("x", "y")], grid, lentil_2010[, 'Water_g'], fun=mean)
Water_lentil_2020 <- rasterize(lentil_2020[, c("x", "y")], grid, lentil_2020[, 'Water_g'], fun=mean)

plot(Water_lentil_2010, main="Water_lentil_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_lentil_2010.png", width = 800, height = 500) 
plot(Water_lentil_2010, main="Water_lentil_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Water_lentil_2020, main="Water_lentil_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_lentil_2020.png", width = 800, height = 500) 
plot(Water_lentil_2020, main="Water_lentil_2020", xlab="Longitude", ylab="Latitude")
dev.off()

lentil_2010$Protein_g <- lentil_2010$lent_a*22.7*10 # protein, total; calculated from total nitrogen 
lentil_2020$Protein_g <- lentil_2020$LENT_A*22.7*10

Protein_lentil_2010 <- rasterize(lentil_2010[, c("x", "y")], grid, lentil_2010[, 'Protein_g'], fun=mean)
Protein_lentil_2020 <- rasterize(lentil_2020[, c("x", "y")], grid, lentil_2020[, 'Protein_g'], fun=mean)

plot(Protein_lentil_2010, main="Protein_lentil_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_lentil_2010.png", width = 800, height = 500) 
plot(Protein_lentil_2010, main="Protein_lentil_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Protein_lentil_2020, main="Protein_lentil_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_lentil_2020.png", width = 800, height = 500) 
plot(Protein_lentil_2020, main="Protein_lentil_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# 16 Cowpea
cowpea_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "cowp_a"))
cowpea_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "COWP_A")) 

cowpea_2010 <- cowpea_2010[cowp_a != 0]
cowpea_2020 <- cowpea_2020[COWP_A != 0]

# 300527 Cowpea, seeds, raw
# Energy
cowpea_2010$Energy_g <- cowpea_2010$cowp_a*1419.07*10 # energy, total metabolizable; calculated from the energy-producing food components
cowpea_2020$Energy_g <- cowpea_2020$COWP_A*1419.07*10 # (original as from source)

Energy_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Energy_g'], fun=mean)
Energy_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Energy_g'], fun=mean)

plot(Energy_cowpea_2010, main="Energy_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Energy_cowpea_2010.png", width = 800, height = 500) 
plot(Energy_cowpea_2010, main="Energy_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Energy_cowpea_2020, main="Energy_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Energy_cowpea_2020.png", width = 800, height = 500) 
plot(Energy_cowpea_2020, main="Energy_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Water
cowpea_2010$Water_g <- cowpea_2010$cowp_a*12.7*10 
cowpea_2020$Water_g <- cowpea_2020$COWP_A*12.7*10

Water_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Water_g'], fun=mean)
Water_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Water_g'], fun=mean)

plot(Water_cowpea_2010, main="Water_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_cowpea_2010.png", width = 800, height = 500) 
plot(Water_cowpea_2010, main="Water_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Water_cowpea_2020, main="Water_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_cowpea_2020.png", width = 800, height = 500) 
plot(Water_cowpea_2020, main="Water_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Protein
cowpea_2010$Protein_g <- cowpea_2010$cowp_a*20.21*10 # protein, total; calculated from total nitrogen 
cowpea_2020$Protein_g <- cowpea_2020$COWP_A*20.21*10

Protein_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Protein_g'], fun=mean)
Protein_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Protein_g'], fun=mean)

plot(Protein_cowpea_2010, main="Protein_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_cowpea_2010.png", width = 800, height = 500) 
plot(Protein_cowpea_2010, main="Protein_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Protein_cowpea_2020, main="Protein_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_cowpea_2020.png", width = 800, height = 500) 
plot(Protein_cowpea_2020, main="Protein_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fat
cowpea_2010$Fat_g <- cowpea_2010$cowp_a*2.37*10 # fat, total; derived by analysis using continuous extraction  
cowpea_2020$Fat_g <- cowpea_2020$COWP_A*2.37*10

Fat_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Fat_g'], fun=mean)
Fat_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Fat_g'], fun=mean)

plot(Fat_cowpea_2010, main="Fat_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_cowpea_2010.png", width = 800, height = 500) 
plot(Fat_cowpea_2010, main="Fat_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fat_cowpea_2020, main="Fat_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_cowpea_2020.png", width = 800, height = 500) 
plot(Fat_cowpea_2020, main="Fat_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Carbohydrate
cowpea_2010$Carbohydrate_g <- cowpea_2010$cowp_a*61.24*10 # carbohydrate, total; calculated by difference
cowpea_2020$Carbohydrate_g <- cowpea_2020$COWP_A*61.24*10

Carbohydrate_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Carbohydrate_g'], fun=mean)
Carbohydrate_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Carbohydrate_g'], fun=mean)

plot(Carbohydrate_cowpea_2010, main="Carbohydrate_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Carbohydrate_cowpea_2010.png", width = 800, height = 500) 
plot(Carbohydrate_cowpea_2010, main="Carbohydrate_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Carbohydrate_cowpea_2020, main="Carbohydrate_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Carbohydrate_cowpea_2020.png", width = 800, height = 500) 
plot(Carbohydrate_cowpea_2020, main="Carbohydrate_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fibre 
cowpea_2010$Fibre_g <- cowpea_2010$cowp_a*23.59*10 # fibre, total dietary; determined gravimetrically by the AOAC total dietary fibre method 
cowpea_2020$Fibre_g <- cowpea_2020$COWP_A*23.59*10 # (Prosky and similar methods)

Fibre_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Fibre_g'], fun=mean)
Fibre_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Fibre_g'], fun=mean)

plot(Fibre_cowpea_2010, main="Fibre_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_cowpea_2010.png", width = 800, height = 500) 
plot(Fibre_cowpea_2010, main="Fibre_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fibre_cowpea_2020, main="Fibre_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_cowpea_2020.png", width = 800, height = 500) 
plot(Fibre_cowpea_2020, main="Fibre_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Mineral elements
# Ca
cowpea_2010$Ca_mg <- cowpea_2010$cowp_a*77.52*10 
cowpea_2020$Ca_mg <- cowpea_2020$COWP_A*77.52*10

Ca_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Ca_mg'], fun=mean)
Ca_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Ca_mg'], fun=mean)

plot(Ca_cowpea_2010, main="Ca_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Ca_cowpea_2010.png", width = 800, height = 500) 
plot(Ca_cowpea_2010, main="Ca_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Ca_cowpea_2020, main="Ca_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Ca_cowpea_2020.png", width = 800, height = 500) 
plot(Ca_cowpea_2020, main="Ca_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# P
cowpea_2010$P_mg <- cowpea_2010$cowp_a*354.52*10 
cowpea_2020$P_mg <- cowpea_2020$COWP_A*354.52*10

P_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'P_mg'], fun=mean)
P_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'P_mg'], fun=mean)

plot(P_cowpea_2010, main="P_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/P_cowpea_2010.png", width = 800, height = 500) 
plot(P_cowpea_2010, main="P_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(P_cowpea_2020, main="P_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/P_cowpea_2020.png", width = 800, height = 500) 
plot(P_cowpea_2020, main="P_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Mg
cowpea_2010$Mg_mg <- cowpea_2010$cowp_a*178.39*10 
cowpea_2020$Mg_mg <- cowpea_2020$COWP_A*178.39*10

Mg_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Mg_mg'], fun=mean)
Mg_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Mg_mg'], fun=mean)

plot(Mg_cowpea_2010, main="Mg_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Mg_cowpea_2010.png", width = 800, height = 500) 
plot(Mg_cowpea_2010, main="Mg_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Mg_cowpea_2020, main="Mg_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Mg_cowpea_2020.png", width = 800, height = 500) 
plot(Mg_cowpea_2020, main="Mg_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# K
cowpea_2010$K_mg <- cowpea_2010$cowp_a*1082.74*10 
cowpea_2020$K_mg <- cowpea_2020$COWP_A*1082.74*10

K_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'K_mg'], fun=mean)
K_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'K_mg'], fun=mean)

plot(K_cowpea_2010, main="K_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/K_cowpea_2010.png", width = 800, height = 500) 
plot(K_cowpea_2010, main="K_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(K_cowpea_2020, main="K_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/K_cowpea_2020.png", width = 800, height = 500) 
plot(K_cowpea_2020, main="K_cowpea_2020", xlab="Longitude", ylab="Latitude")

# Na
cowpea_2010$Na_mg <- cowpea_2010$cowp_a*10.31*10 
cowpea_2020$Na_mg <- cowpea_2020$COWP_A*10.31*10

Na_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Na_mg'], fun=mean)
Na_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Na_mg'], fun=mean)

plot(Na_cowpea_2010, main="Na_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Na_cowpea_2010.png", width = 800, height = 500) 
plot(Na_cowpea_2010, main="Na_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Na_cowpea_2020, main="Na_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Na_cowpea_2020.png", width = 800, height = 500) 
plot(Na_cowpea_2020, main="Na_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Trace elements
# Fe
cowpea_2010$Fe_mg <- cowpea_2010$cowp_a*5.13*10 
cowpea_2020$Fe_mg <- cowpea_2020$COWP_A*5.13*10

Fe_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Fe_mg'], fun=mean)
Fe_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Fe_mg'], fun=mean)

plot(Fe_cowpea_2010, main="Fe_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fe_cowpea_2010.png", width = 800, height = 500) 
plot(Fe_cowpea_2010, main="Fe_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fe_cowpea_2020, main="Fe_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fe_cowpea_2020.png", width = 800, height = 500) 
plot(Fe_cowpea_2020, main="Fe_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Cu
cowpea_2010$Cu_mg <- cowpea_2010$cowp_a*0.7*10 
cowpea_2020$Cu_mg <- cowpea_2020$COWP_A*0.7*10

Cu_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Cu_mg'], fun=mean)
Cu_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Cu_mg'], fun=mean)

plot(Cu_cowpea_2010, main="Cu_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Cu_cowpea_2010.png", width = 800, height = 500) 
plot(Cu_cowpea_2010, main="Cu_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Cu_cowpea_2020, main="Cu_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Cu_cowpea_2020.png", width = 800, height = 500) 
plot(Cu_cowpea_2020, main="Cu_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Zn
cowpea_2010$Zn_mg <- cowpea_2010$cowp_a*3.88*10 
cowpea_2020$Zn_mg <- cowpea_2020$COWP_A*3.88*10

Zn_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Zn_mg'], fun=mean)
Zn_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Zn_mg'], fun=mean)

plot(Zn_cowpea_2010, main="Zn_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Zn_cowpea_2010.png", width = 800, height = 500) 
plot(Zn_cowpea_2010, main="Zn_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Zn_cowpea_2020, main="Zn_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Zn_cowpea_2020.png", width = 800, height = 500) 
plot(Zn_cowpea_2020, main="Zn_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Vitamin
cowpea_2010$Vitamin_mg <- cowpea_2010$cowp_a*(0.14+0.03+0.26)*10 
cowpea_2020$Vitamin_mg <- cowpea_2020$COWP_A*(0.14+0.03+0.26)*10

Vitamin_cowpea_2010 <- rasterize(cowpea_2010[, c("x", "y")], grid, cowpea_2010[, 'Vitamin_mg'], fun=mean)
Vitamin_cowpea_2020 <- rasterize(cowpea_2020[, c("x", "y")], grid, cowpea_2020[, 'Vitamin_mg'], fun=mean)

plot(Vitamin_cowpea_2010, main="Vitamin_cowpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Vitamin_cowpea_2010.png", width = 800, height = 500) 
plot(Vitamin_cowpea_2010, main="Vitamin_cowpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Vitamin_cowpea_2020, main="Vitamin_cowpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Vitamin_cowpea_2020.png", width = 800, height = 500) 
plot(Vitamin_cowpea_2020, main="Vitamin_cowpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# 15 Chickpea
# chickpea_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
chickpea_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                       select = c("x", "y", "chic_a"))
chickpea_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "CHIC_A")) 

chickpea_2010 <- chickpea_2010[chic_a != 0]
chickpea_2020 <- chickpea_2020[CHIC_A != 0]

# 0300259	Chickpea, seeds, mature, water-soaked, raw
# Water
chickpea_2010$Water_g <- chickpea_2010$chic_a*46.89*10 
chickpea_2020$Water_g <- chickpea_2020$CHIC_A*46.89*10

Water_chickpea_2010 <- rasterize(chickpea_2010[, c("x", "y")], grid, chickpea_2010[, 'Water_g'], fun=mean)
Water_chickpea_2020 <- rasterize(chickpea_2020[, c("x", "y")], grid, chickpea_2020[, 'Water_g'], fun=mean)

plot(Water_chickpea_2010, main="Water_chickpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_chickpea_2010.png", width = 800, height = 500) 
plot(Water_chickpea_2010, main="Water_chickpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Water_chickpea_2020, main="Water_chickpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Water_chickpea_2020.png", width = 800, height = 500) 
plot(Water_chickpea_2020, main="Water_chickpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Protein
chickpea_2010$Protein_g <- chickpea_2010$chic_a*13.95*10 # protein, total; calculated from total nitrogen 
chickpea_2020$Protein_g <- chickpea_2020$CHIC_A*13.95*10

Protein_chickpea_2010 <- rasterize(chickpea_2010[, c("x", "y")], grid, chickpea_2010[, 'Protein_g'], fun=mean)
Protein_chickpea_2020 <- rasterize(chickpea_2020[, c("x", "y")], grid, chickpea_2020[, 'Protein_g'], fun=mean)

plot(Protein_chickpea_2010, main="Protein_chickpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_chickpea_2010.png", width = 800, height = 500) 
plot(Protein_chickpea_2010, main="Protein_chickpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Protein_chickpea_2020, main="Protein_chickpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_chickpea_2020.png", width = 800, height = 500) 
plot(Protein_chickpea_2020, main="Protein_chickpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fat
chickpea_2010$Fat_g <- chickpea_2010$chic_a*3.27*10 
chickpea_2020$Fat_g <- chickpea_2020$CHIC_A*3.27*10

Fat_chickpea_2010 <- rasterize(chickpea_2010[, c("x", "y")], grid, chickpea_2010[, 'Fat_g'], fun=mean)
Fat_chickpea_2020 <- rasterize(chickpea_2020[, c("x", "y")], grid, chickpea_2020[, 'Fat_g'], fun=mean)

plot(Fat_chickpea_2010, main="Fat_chickpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_chickpea_2010.png", width = 800, height = 500) 
plot(Fat_chickpea_2010, main="Fat_chickpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fat_chickpea_2020, main="Fat_chickpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_chickpea_2020.png", width = 800, height = 500) 
plot(Fat_chickpea_2020, main="Fat_chickpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Carbohydrate
chickpea_2010$Carbohydrate_g <- chickpea_2010$chic_a*34.21*10 # carbohydrate, total; calculated by difference
chickpea_2020$Carbohydrate_g <- chickpea_2020$CHIC_A*34.21*10

Carbohydrate_chickpea_2010 <- rasterize(chickpea_2010[, c("x", "y")], grid, chickpea_2010[, 'Carbohydrate_g'], fun=mean)
Carbohydrate_chickpea_2020 <- rasterize(chickpea_2020[, c("x", "y")], grid, chickpea_2020[, 'Carbohydrate_g'], fun=mean)

plot(Carbohydrate_chickpea_2010, main="Carbohydrate_chickpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Carbohydrate_chickpea_2010.png", width = 800, height = 500) 
plot(Carbohydrate_chickpea_2010, main="Carbohydrate_chickpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Carbohydrate_chickpea_2020, main="Carbohydrate_chickpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Carbohydrate_chickpea_2020.png", width = 800, height = 500) 
plot(Carbohydrate_chickpea_2020, main="Carbohydrate_chickpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fatty acids
chickpea_2010$Fattyacids_g <- chickpea_2010$chic_a*(0.3+0.61+1.62)*10 
chickpea_2020$Fattyacids_g <- chickpea_2020$CHIC_A*(0.3+0.61+1.62)*10

Fattyacids_chickpea_2010 <- rasterize(chickpea_2010[, c("x", "y")], grid, chickpea_2010[, 'Fattyacids_g'], fun=mean)
Fattyacids_chickpea_2020 <- rasterize(chickpea_2020[, c("x", "y")], grid, chickpea_2020[, 'Fattyacids_g'], fun=mean)

plot(Fattyacids_chickpea_2010, main="Fattyacids_chickpea_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fattyacids_chickpea_2010.png", width = 800, height = 500) 
plot(Fattyacids_chickpea_2010, main="Fattyacids_chickpea_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fattyacids_chickpea_2020, main="Fattyacids_chickpea_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fattyacids_chickpea_2020.png", width = 800, height = 500) 
plot(Fattyacids_chickpea_2020, main="Fattyacids_chickpea_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# 14 Bean
bean_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "bean"))
bean_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "bean_a"))
bean_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "bean_a"))
bean_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "BEAN_A")) 

bean_2000 <- bean_2000[bean != 0]
bean_2010 <- bean_2010[bean_a != 0]
bean_2020 <- bean_2020[BEAN_A != 0]

# 0300157	Beans, seeds, raw
# Protein
bean_2000$Protein_g <- bean_2000$bean*22.38*10
bean_2010$Protein_g <- bean_2010 $bean_a*22.38*10
bean_2020$Protein_g <- bean_2020$BEAN_A*22.38*10

Protein_bean_2000 <- rasterize(bean_2000[, c("x", "y")], grid, bean_2000[, 'Protein_g'], fun=mean)
Protein_bean_2010 <- rasterize(bean_2010[, c("x", "y")], grid, bean_2010[, 'Protein_g'], fun=mean)
Protein_bean_2020 <- rasterize(bean_2020[, c("x", "y")], grid, bean_2020[, 'Protein_g'], fun=mean)

plot(Protein_bean_2000, main="Protein_bean_2000", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_bean_2000.png", width = 800, height = 500) 
plot(Protein_bean_2000, main="Protein_bean_2000", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Protein_bean_2010, main="Protein_bean_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_bean_2010.png", width = 800, height = 500) 
plot(Protein_bean_2010, main="Protein_bean_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Protein_bean_2020, main="Protein_bean_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Protein_bean_2020.png", width = 800, height = 500) 
plot(Protein_bean_2020, main="Protein_bean_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fat
bean_2000$Fat_g <- bean_2000$bean*1.05*10 # fat, total; derived by analysis using continuous extraction
bean_2010$Fat_g <- bean_2010 $bean_a*1.05*10
bean_2020$Fat_g <- bean_2020$BEAN_A*1.05*10

Fat_bean_2000 <- rasterize(bean_2000[, c("x", "y")], grid, bean_2000[, 'Fat_g'], fun=mean)
Fat_bean_2010 <- rasterize(bean_2010[, c("x", "y")], grid, bean_2010[, 'Fat_g'], fun=mean)
Fat_bean_2020 <- rasterize(bean_2020[, c("x", "y")], grid, bean_2020[, 'Fat_g'], fun=mean)

plot(Fat_bean_2000, main="Fat_bean_2000", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_bean_2000.png", width = 800, height = 500) 
plot(Fat_bean_2000, main="Fat_bean_2000", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fat_bean_2010, main="Fat_bean_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_bean_2010.png", width = 800, height = 500) 
plot(Fat_bean_2010, main="Fat_bean_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fat_bean_2020, main="Fat_bean_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fat_bean_2020.png", width = 800, height = 500) 
plot(Fat_bean_2020, main="Fat_bean_2020", xlab="Longitude", ylab="Latitude")
dev.off()

# Fibre
bean_2000$Fibre_g <- bean_2000$bean*37.83*10 # fibre; determined by neutral detergent method
bean_2010$Fibre_g <- bean_2010 $bean_a*37.83*10
bean_2020$Fibre_g <- bean_2020$BEAN_A*37.83*10

Fibre_bean_2000 <- rasterize(bean_2000[, c("x", "y")], grid, bean_2000[, 'Fibre_g'], fun=mean)
Fibre_bean_2010 <- rasterize(bean_2010[, c("x", "y")], grid, bean_2010[, 'Fibre_g'], fun=mean)
Fibre_bean_2020 <- rasterize(bean_2020[, c("x", "y")], grid, bean_2020[, 'Fibre_g'], fun=mean)

plot(Fibre_bean_2000, main="Fibre_bean_2000", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_bean_2000.png", width = 800, height = 500) 
plot(Fibre_bean_2000, main="Fibre_bean_2000", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fibre_bean_2010, main="Fibre_bean_2010", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_bean_2010.png", width = 800, height = 500) 
plot(Fibre_bean_2010, main="Fibre_bean_2010", xlab="Longitude", ylab="Latitude")
dev.off()

plot(Fibre_bean_2020, main="Fibre_bean_2020", xlab="Longitude", ylab="Latitude") 
png(filename = "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results/Fibre_bean_2020.png", width = 800, height = 500) 
plot(Fibre_bean_2020, main="Fibre_bean_2020", xlab="Longitude", ylab="Latitude")
dev.off()
