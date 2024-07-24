rm(list = ls()) # 清空环境变量
pacman::p_load(sf,raster,ggplot2, ggspatial, tidyverse, rgdal, tmap)#所需要的包
library(raster)
library(rasterVis)
library(data.table)

cell_size = 0.083 # 网格单元大小，以度为单位
lon_min <- -159.708; lon_max <- 178.958; lat_min <- -47.2083; lat_max <- 69.9583
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1 #计算出网格的列数和行数，基于给定的经纬度范围和网格单元大小
grid <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=cell_size, crs="+proj=longlat +datum=WGS84")

# Data input
# 用的都是Yield数据
# 1 Wheat
wheat_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "whea"))
wheat_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "whea_a"))
wheat_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "whea_a"))
wheat_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "WHEA_A")) 

wheat_2000 <- wheat_2000[whea != 0]
wheat_2005 <- wheat_2005[whea_a != 0]
wheat_2010 <- wheat_2010[whea_a != 0]
wheat_2020 <- wheat_2020[WHEA_A != 0]

# 2 Rice
rice_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "rice"))
rice_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "rice_a"))
rice_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "rice_a"))
rice_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "RICE_A"))  

rice_2000 <- rice_2000[rice != 0]
rice_2005 <- rice_2005[rice_a != 0]
rice_2010 <- rice_2010[rice_a != 0]
rice_2020 <- rice_2020[RICE_A != 0]

# 3 Maize
maize_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                  select = c("x", "y", "maiz"))
maize_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                  select = c("x", "y", "maiz_a"))
maize_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "maiz_a"))
maize_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "MAIZ_A"))  

maize_2000 <- maize_2000[maiz != 0]
maize_2005 <- maize_2005[maiz_a != 0]
maize_2010 <- maize_2010[maiz_a != 0]
maize_2020 <- maize_2020[MAIZ_A != 0]

# 4 Barley
barley_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "barl"))
barley_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "barl_a"))
barley_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "barl_a"))
barley_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "BARL_A"))  

barley_2000 <- barley_2000[barl != 0]
barley_2005 <- barley_2005[barl_a != 0]
barley_2010 <- barley_2010[barl_a != 0]
barley_2020 <- barley_2020[BARL_A != 0]

# 5 Small Millet
mill_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                    select = c("x", "y", "mill"))
mill_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                    select = c("x", "y", "smil_a"))
mill_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                    select = c("x", "y", "smil_a"))
mill_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                    select = c("x", "y", "MILL_A"))  

mill_2000 <- mill_2000[mill != 0]
mill_2005 <- mill_2005[smil_a != 0]
mill_2010 <- mill_2010[smil_a != 0]
mill_2020 <- mill_2020[MILL_A != 0]

# 6 Pearl Millet
# pmil_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                   select = c("x", "y", "swpy"))
# pmil_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                   select = c("x", "y", "swpo_a"))
pmil_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "pmil_a"))
pmil_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "PMIL_A"))  

pmil_2010 <- pmil_2010[pmil_a != 0]
pmil_2020 <- pmil_2020[PMIL_A != 0]

# 7 Sorghum
sorghum_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "sorg"))
sorghum_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                  select = c("x", "y", "sorg_a"))
sorghum_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "sorg_a"))
sorghum_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "SORG_A"))  

sorghum_2000 <- sorghum_2000[sorg != 0]
sorghum_2005 <- sorghum_2005[sorg_a != 0]
sorghum_2010 <- sorghum_2010[sorg_a != 0]
sorghum_2020 <- sorghum_2020[SORG_A != 0]

# 8 Other Cereals
# ocer_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                   select = c("x", "y", "swpy"))
ocer_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "ocer_a"))
ocer_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "ocer_a"))
ocer_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OCER_A"))  

ocer_2005 <- ocer_2005[ocer_a != 0]
ocer_2010 <- ocer_2010[ocer_a != 0]
ocer_2020 <- ocer_2020[OCER_A != 0]

# 9 Potato
potato_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                    select = c("x", "y", "pota"))
potato_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                  select = c("x", "y", "pota_a"))
potato_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "pota_a"))
potato_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "POTA_A"))  

potato_2000 <- potato_2000[pota != 0]
potato_2005 <- potato_2005[pota_a != 0]
potato_2010 <- potato_2010[pota_a != 0]
potato_2020 <- potato_2020[POTA_A != 0]

# 10 Sweet Potato
swpo_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "swpy"))
swpo_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "swpo_a"))
swpo_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "swpo_a"))
swpo_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "SWPO_A")) 

swpo_2000 <- swpo_2000[swpy != 0]
swpo_2005 <- swpo_2005[swpo_a != 0]
swpo_2010 <- swpo_2010[swpo_a != 0]
swpo_2020 <- swpo_2020[SWPO_A != 0]

# 11 Yams
# yams_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
yams_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "yams_a"))
yams_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "yams_a"))
yams_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "YAMS_A")) 

yams_2005 <- yams_2005[yams_a != 0]
yams_2010 <- yams_2010[yams_a != 0]
yams_2020 <- yams_2020[YAMS_A != 0]

# 12 Cassava
cassava_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "cass"))
cassava_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "cass_a"))
cassava_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "cass_a"))
cassava_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "CASS_A")) 

cassava_2000 <- cassava_2000[cass != 0]
cassava_2005 <- cassava_2005[cass_a != 0]
cassava_2010 <- cassava_2010[cass_a != 0]
cassava_2020 <- cassava_2020[CASS_A != 0]

# 13 Other Roots
# orts_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                       select = c("x", "y", "swpy"))
orts_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                      select = c("x", "y", "orts_a"))
orts_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "orts_a"))
orts_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "ORTS_A")) 

orts_2005 <- orts_2005[orts_a != 0]
orts_2010 <- orts_2010[orts_a != 0]
orts_2020 <- orts_2020[ORTS_A != 0]

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
bean_2005 <- bean_2005[bean_a != 0]
bean_2010 <- bean_2010[bean_a != 0]
bean_2020 <- bean_2020[BEAN_A != 0]

# 0300157	Beans, seeds, raw
bean_2000$Protein_g <- bean_2000$bean*22.38*10
bean_2005$Protein_g <- bean_2005$bean_a*22.38*10
bean_2010$Protein_g <- bean_2010 $bean_a*22.38*10
bean_2020$Protein_g <- bean_2020$BEAN_A*22.38*10

bean_2000$Fat_g <- bean_2000$bean*1.05*10 # fat, total; derived by analysis using continuous extraction
bean_2005$Fat_g <- bean_2005$bean_a*1.05*10
bean_2010$Fat_g <- bean_2010 $bean_a*1.05*10
bean_2020$Fat_g <- bean_2020$BEAN_A*1.05*10

bean_2000$Fibre_g <- bean_2000$bean*37.83*10 # fibre; determined by neutral detergent method
bean_2005$Fibre_g <- bean_2005$bean_a*37.83*10
bean_2010$Fibre_g <- bean_2010 $bean_a*37.83*10
bean_2020$Fibre_g <- bean_2020$BEAN_A*37.83*10

# 15 Chickpea
# chickpea_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
chickpea_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "chic_a"))
chickpea_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "chic_a"))
chickpea_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "CHIC_A")) 

chickpea_2005 <- chickpea_2005[chic_a != 0]
chickpea_2010 <- chickpea_2010[chic_a != 0]
chickpea_2020 <- chickpea_2020[CHIC_A != 0]

# 0300259	Chickpea, seeds, mature, water-soaked, raw
chickpea_2005$Water_g <- chickpea_2005$chic_a*46.89*10  
chickpea_2010$Water_g <- chickpea_2010$chic_a*46.89*10 
chickpea_2020$Water_g <- chickpea_2020$CHIC_A*46.89*10

chickpea_2005$Protein_g <- chickpea_2005$chic_a*13.95*10 # protein, total; calculated from total nitrogen 
chickpea_2010$Protein_g <- chickpea_2010$chic_a*13.95*10 
chickpea_2020$Protein_g <- chickpea_2020$CHIC_A*13.95*10

chickpea_2005$Fat_g <- chickpea_2005$chic_a*3.27*10  
chickpea_2010$Fat_g <- chickpea_2010$chic_a*3.27*10 
chickpea_2020$Fat_g <- chickpea_2020$CHIC_A*3.27*10

chickpea_2005$Carbohydrate_g <- chickpea_2005$chic_a*34.21*10  # carbohydrate, total; calculated by difference
chickpea_2010$Carbohydrate_g <- chickpea_2010$chic_a*34.21*10 
chickpea_2020$Carbohydrate_g <- chickpea_2020$CHIC_A*34.21*10

chickpea_2005$Fatti_acids_g <- chickpea_2005$chic_a*(0.3+0.61+1.62)*10  
chickpea_2010$Fatti_acids_g <- chickpea_2010$chic_a*(0.3+0.61+1.62)*10 
chickpea_2020$Fatti_acids_g <- chickpea_2020$CHIC_A*(0.3+0.61+1.62)*10

# 16 Cowpea
# cowpea_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
cowpea_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                       select = c("x", "y", "cowp_a"))
cowpea_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                       select = c("x", "y", "cowp_a"))
cowpea_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "COWP_A")) 

cowpea_2005 <- cowpea_2005[cowp_a != 0]
cowpea_2010 <- cowpea_2010[cowp_a != 0]
cowpea_2020 <- cowpea_2020[COWP_A != 0]

#0300527 Cowpea, seeds, raw
cowpea_2005$Energy_g <- cowpea_2005$cowp_a*1419.07*10 # energy, total metabolizable; calculated from the energy-producing food components 
cowpea_2010$Energy_g <- cowpea_2010$cowp_a*1419.07*10 # (original as from source)
cowpea_2020$Energy_g <- cowpea_2020$COWP_A*1419.07*10

cowpea_2005$Water_g <- cowpea_2005$cowp_a*12.7*10  
cowpea_2010$Water_g <- cowpea_2010$cowp_a*12.7*10 
cowpea_2020$Water_g <- cowpea_2020$COWP_A*12.7*10

cowpea_2005$Protein_g <- cowpea_2005$cowp_a*20.21*10 # protein, total; calculated from total nitrogen 
cowpea_2010$Protein_g <- cowpea_2010$cowp_a*20.21*10 
cowpea_2020$Protein_g <- cowpea_2020$COWP_A*20.21*10

cowpea_2005$Fat_g <- cowpea_2005$cowp_a*2.37*10 # fat, total; derived by analysis using continuous extraction  
cowpea_2010$Fat_g <- cowpea_2010$cowp_a*2.37*10 
cowpea_2020$Fat_g <- cowpea_2020$COWP_A*2.37*10

cowpea_2005$Carbohydrate_g <- cowpea_2005$cowp_a*61.24*10  # carbohydrate, total; calculated by difference
cowpea_2010$Carbohydrate_g <- cowpea_2010$cowp_a*61.24*10 
cowpea_2020$Carbohydrate_g <- cowpea_2020$COWP_A*61.24*10

cowpea_2005$Fibre_g <- cowpea_2005$cowp_a*23.59*10 # fibre, total dietary; determined gravimetrically by the AOAC total dietary fibre method 
cowpea_2010$Fibre_g <- cowpea_2010$cowp_a*23.59*10 # (Prosky and similar methods) 
cowpea_2020$Fibre_g <- cowpea_2020$COWP_A*23.59*10

# Mineral elements
cowpea_2005$Ca_mg <- cowpea_2005$cowp_a*77.52*10  
cowpea_2010$Ca_mg <- cowpea_2010$cowp_a*77.52*10 
cowpea_2020$Ca_mg <- cowpea_2020$COWP_A*77.52*10

cowpea_2005$P_mg <- cowpea_2005$cowp_a*354.52*10  
cowpea_2010$P_mg <- cowpea_2010$cowp_a*354.52*10 
cowpea_2020$P_mg <- cowpea_2020$COWP_A*354.52*10

cowpea_2005$Mg_mg <- cowpea_2005$cowp_a*178.39*10  
cowpea_2010$Mg_mg <- cowpea_2010$cowp_a*178.39*10 
cowpea_2020$Mg_mg <- cowpea_2020$COWP_A*178.39*10

cowpea_2005$K_mg <- cowpea_2005$cowp_a*1082.74*10  
cowpea_2010$K_mg <- cowpea_2010$cowp_a*1082.74*10 
cowpea_2020$K_mg <- cowpea_2020$COWP_A*1082.74*10

cowpea_2005$Na_mg <- cowpea_2005$cowp_a*10.31*10  
cowpea_2010$Na_mg <- cowpea_2010$cowp_a*10.31*10 
cowpea_2020$Na_mg <- cowpea_2020$COWP_A*10.31*10

# Trace elements
cowpea_2005$Fe_mg <- cowpea_2005$cowp_a*5.13*10  
cowpea_2010$Fe_mg <- cowpea_2010$cowp_a*5.13*10 
cowpea_2020$Fe_mg <- cowpea_2020$COWP_A*5.13*10

cowpea_2005$Cu_mg <- cowpea_2005$cowp_a*0.7*10  
cowpea_2010$Cu_mg <- cowpea_2010$cowp_a*0.7*10 
cowpea_2020$Cu_mg <- cowpea_2020$COWP_A*0.7*10

cowpea_2005$Zn_mg <- cowpea_2005$cowp_a*3.88*10  
cowpea_2010$Zn_mg <- cowpea_2010$cowp_a*3.88*10 
cowpea_2020$Zn_mg <- cowpea_2020$COWP_A*3.88*10

# Vitamin
cowpea_2005$Vitamin_mg <- cowpea_2005$cowp_a*(0.14+0.03+0.26)*10  
cowpea_2010$Vitamin_mg <- cowpea_2010$cowp_a*(0.14+0.03+0.26)*10 
cowpea_2020$Vitamin_mg <- cowpea_2020$COWP_A*(0.14+0.03+0.26)*10

# 17 Pigeon Pea
# pige_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
pige_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "pige_a"))
pige_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "pige_a"))
pige_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "PIGE_A")) 

pige_2005 <- pige_2005[pige_a != 0]
pige_2010 <- pige_2010[pige_a != 0]
pige_2020 <- pige_2020[PIGE_A != 0]

# 0300529	Pigeon pea, seeds, raw
pige_2005$Energy_kJ <- pige_2005$pige_a*1439.86*10
pige_2010$Energy_kJ <- pige_2010$pige_a*1439.86*10
pige_2020$Energy_kJ <- pige_2020$PIGE_A*1439.86*10

pige_2005$Water_g <- pige_2005$pige_a*11.45*10
pige_2010$Water_g <- pige_2010$pige_a*11.45*10
pige_2020$Water_g <- pige_2020$PIGE_A*11.45*10

pige_2005$Protein_g <- pige_2005$pige_a*18.96*10 # protein, total; calculated from total nitrogen 
pige_2010$Protein_g <- pige_2010$pige_a*18.96*10
pige_2020$Protein_g <- pige_2020$PIGE_A*18.96*10

pige_2005$Fat_g <- pige_2005$pige_a*2.13*10 # fat, total; derived by analysis using continuous extraction
pige_2010$Fat_g <- pige_2010$pige_a*2.13*10
pige_2020$Fat_g <- pige_2020$PIGE_A*2.13*10

pige_2005$Carbohydrate_g <- pige_2005$pige_a*64*10
pige_2010$Carbohydrate_g <- pige_2010$pige_a*64*10
pige_2020$Carbohydrate_g <- pige_2020$PIGE_A*64*10

pige_2005$Fibre_g <- pige_2005$pige_a*21.31*10 # fibre, total dietary; determined gravimetrically by the AOAC total dietary fibre method 
pige_2010$Fibre_g <- pige_2010$pige_a*21.31*10 # (Prosky and similar methods)
pige_2020$Fibre_g <- pige_2020$PIGE_A*21.31*10

# Mineral elements
pige_2005$Ca_mg <- pige_2005$pige_a*129.34*10 
pige_2010$Ca_mg <- pige_2010$pige_a*129.34*10
pige_2020$Ca_mg <- pige_2020$PIGE_A*129.34*10

pige_2005$P_mg <- pige_2005$pige_a*269.19*10 
pige_2010$P_mg <- pige_2010$pige_a*269.19*10
pige_2020$P_mg <- pige_2020$PIGE_A*269.19*10

pige_2005$Mg_mg <- pige_2005$pige_a*166*10 
pige_2010$Mg_mg <- pige_2010$pige_a*166*10
pige_2020$Mg_mg <- pige_2020$PIGE_A*166*10

pige_2005$K_mg <- pige_2005$pige_a*1214.8*10 
pige_2010$K_mg <- pige_2010$pige_a*1214.8*10
pige_2020$K_mg <- pige_2020$PIGE_A*1214.8*10

pige_2005$Na_mg <- pige_2005$pige_a*1.62*10 
pige_2010$Na_mg <- pige_2010$pige_a*1.62*10
pige_2020$Na_mg <- pige_2020$PIGE_A*1.62*10

# Trace elements
pige_2005$Fe_mg <- pige_2005$pige_a*1.94*10 
pige_2010$Fe_mg <- pige_2010$pige_a*1.94*10
pige_2020$Fe_mg <- pige_2020$PIGE_A*1.94*10

pige_2005$Cu_mg <- pige_2005$pige_a*0.57*10 
pige_2010$Cu_mg <- pige_2010$pige_a*0.57*10
pige_2020$Cu_mg <- pige_2020$PIGE_A*0.57*10

pige_2005$Zn_mg <- pige_2005$pige_a*2.02*10 
pige_2010$Zn_mg <- pige_2010$pige_a*2.02*10
pige_2020$Zn_mg <- pige_2020$PIGE_A*2.02*10

#Vitamin 暂时是加起来
pige_2005$Vitamin_mg <- pige_2005$pige_a*(1.06+2.69+0.07+1.47)*10 
pige_2010$Vitamin_mg <- pige_2010$pige_a*(1.06+2.69+0.07+1.47)*10
pige_2020$Vitamin_mg <- pige_2020$PIGE_A*(1.06+2.69+0.07+1.47)*10

# 18 Lentil
# lentil_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
lentil_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "lent_a"))
lentil_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "lent_a"))
lentil_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "LENT_A"))

lentil_2005 <- lentil_2005[lent_a != 0]
lentil_2010 <- lentil_2010[lent_a != 0]
lentil_2020 <- lentil_2020[LENT_A != 0]

# 0300198	Lentil, seeds, raw
lentil_2005$Water_g <- lentil_2005$lent_a*10.4*10 
lentil_2010$Water_g <- lentil_2010$lent_a*10.4*10
lentil_2020$Water_g <- lentil_2020$LENT_A*10.4*10

lentil_2005$Protein_g <- lentil_2005$lent_a*22.7*10 # protein, total; calculated from total nitrogen 
lentil_2010$Protein_g <- lentil_2010$lent_a*22.7*10
lentil_2020$Protein_g <- lentil_2020$LENT_A*22.7*10

# 19 Other Pulses
# opul_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
opul_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "opul_a"))
opul_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "opul_a"))
opul_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OPUL_A"))

opul_2005 <- opul_2005[opul_a != 0]
opul_2010 <- opul_2010[opul_a != 0]
opul_2020 <- opul_2020[OPUL_A != 0]

# 20 Soybean
soybean_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "soyb"))
soybean_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "soyb_a"))
soybean_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "soyb_a"))
soybean_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "SOYB_A"))

soybean_2000 <- soybean_2000[soyb != 0]
soybean_2005 <- soybean_2005[soyb_a != 0]
soybean_2010 <- soybean_2010[soyb_a != 0]
soybean_2020 <- soybean_2020[SOYB_A != 0]

# 0300299	Soybean, seeds, dried, raw
soybean_2000$Water_g <- soybean_2000$soyb*9.5*10
soybean_2005$Water_g <- soybean_2005$soyb_a*9.5*10
soybean_2010$Water_g <- soybean_2010$soyb_a*9.5*10
soybean_2020$Water_g <- soybean_2020$SOYB_A*9.5*10

soybean_2000$fatty_acids_g <- soybean_2000$soyb*17.92*10
soybean_2005$fatty_acids_g <- soybean_2005$soyb_a*17.92*10
soybean_2010$fatty_acids_g <- soybean_2010$soyb_a*17.92*10
soybean_2020$fatty_acids_g <- soybean_2020$SOYB_A*17.92*10

coords_soybean_2000 = soybean_2000[,c("x","y")] #坐标
coords_soybean_2005 = soybean_2005[,c("x","y")]
coords_soybean_2010 = soybean_2010[,c("x","y")]
coords_soybean_2020 = soybean_2020[,c("x","y")]

Yields_soybean_2000 = data.frame(soybean_2000$soyb)
Yields_soybean_2005 = data.frame(soybean_2005$soyb_a)
Yields_soybean_2010 = data.frame(soybean_2010$soyb_a)
Yields_soybean_2020 = data.frame(soybean_2020$SOYB_A)

Yields_soybean_2000_sp <- SpatialPointsDataFrame(coords=coords_soybean_2000, data=Yields_soybean_2000) #存储空间点数据和相关属性数据的框架。它是sp包中的一个类
Yields_soybean_2005_sp <- SpatialPointsDataFrame(coords=coords_soybean_2005, data=Yields_soybean_2005) 
Yields_soybean_2010_sp <- SpatialPointsDataFrame(coords=coords_soybean_2010, data=Yields_soybean_2010) 
Yields_soybean_2000_sp <- SpatialPointsDataFrame(coords=coords_soybean_2000, data=Yields_soybean_2000) 

Y_Water_g <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Water_g'], fun=mean)
Y_Ca_g<- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Ca_g'], fun=mean)
Y_Zn_g <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Zn_g'], fun=mean)

# 21 Groundnut
groundnut_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "grou"))
groundnut_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                      select = c("x", "y", "grou_a"))
groundnut_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "grou_a"))
groundnut_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "GROU_A"))

groundnut_2000 <- groundnut_2000[grou != 0]
groundnut_2005 <- groundnut_2005[grou_a != 0]
groundnut_2010 <- groundnut_2010[grou_a != 0]
groundnut_2020 <- groundnut_2020[GROU_A != 0]

# 22 Coconut
# coconut_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                         select = c("x", "y", "swpy"))
coconut_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                        select = c("x", "y", "cnut_a"))
coconut_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "cnut_a"))
coconut_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "CNUT_A"))

coconut_2005 <- coconut_2005[cnut_a != 0]
coconut_2010 <- coconut_2010[cnut_a != 0]
coconut_2020 <- coconut_2020[CNUT_A != 0]

# 23 Oilpalm
# oilpalm_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                       select = c("x", "y", "swpy"))
oilpalm_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                      select = c("x", "y", "oilp_a"))
oilpalm_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "oilp_a"))
oilpalm_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "OILP_A"))

oilpalm_2005 <- oilpalm_2005[oilp_a != 0]
oilpalm_2010 <- oilpalm_2010[oilp_a != 0]
oilpalm_2020 <- oilpalm_2020[OILP_A != 0]

# 24 Sunflower
# sunflower_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                       select = c("x", "y", "swpy"))
sunflower_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                      select = c("x", "y", "sunf_a"))
sunflower_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "sunf_a"))
sunflower_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "SUNF_A"))

sunflower_2005 <- sunflower_2005[sunf_a != 0]
sunflower_2010 <- sunflower_2010[sunf_a != 0]
sunflower_2020 <- sunflower_2020[SUNF_A != 0]

# 25 Rapeseed
# rapeseed_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                         select = c("x", "y", "swpy"))
rapeseed_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                        select = c("x", "y", "rape_a"))
rapeseed_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "rape_a"))
rapeseed_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "RAPE_A"))

rapeseed_2005 <- rapeseed_2005[rape_a != 0]
rapeseed_2010 <- rapeseed_2010[rape_a != 0]
rapeseed_2020 <- rapeseed_2020[RAPE_A != 0]

# 26 Sesame Seed
# sesame_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
sesame_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                       select = c("x", "y", "sesa_a"))
sesame_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                       select = c("x", "y", "sesa_a"))
sesame_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "SESA_A"))

sesame_2005 <- sesame_2005[sesa_a != 0]
sesame_2010 <- sesame_2010[sesa_a != 0]
sesame_2020 <- sesame_2020[SESA_A != 0]

# 27 Other Oil Crops
ooil_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "ooil"))
ooil_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "ooil_a"))
ooil_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "ooil_a"))
ooil_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OOIL_A"))

ooil_2000 <- ooil_2000[ooil != 0]
ooil_2005 <- ooil_2005[ooil_a != 0]
ooil_2010 <- ooil_2010[ooil_a != 0]
ooil_2020 <- ooil_2020[OOIL_A != 0]

# 28 Sugarcane
sugarcane_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "sugc"))
sugarcane_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "sugc_a"))
sugarcane_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "sugc_a"))
sugarcane_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "SUGC_A"))

sugarcane_2000 <- sugarcane_2000[sugc != 0]
sugarcane_2005 <- sugarcane_2005[sugc_a != 0]
sugarcane_2010 <- sugarcane_2010[sugc_a != 0]
sugarcane_2020 <- sugarcane_2020[SUGC_A != 0]

# 29 Sugarbeet
sugarbeet_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "sugb"))
sugarbeet_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                        select = c("x", "y", "sugb_a"))
sugarbeet_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "sugb_a"))
sugarbeet_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "SUGB_A"))

sugarbeet_2000 <- sugarbeet_2000[sugb != 0]
sugarbeet_2005 <- sugarbeet_2005[sugb_a != 0]
sugarbeet_2010 <- sugarbeet_2010[sugb_a != 0]
sugarbeet_2020 <- sugarbeet_2020[SUGB_A != 0]

# 30 Cotton
cotton_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "cott"))
cotton_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                        select = c("x", "y", "cott_a"))
cotton_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "cott_a"))
cotton_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "COTT_A"))

cotton_2000 <- cotton_2000[cott != 0]
cotton_2005 <- cotton_2005[cott_a != 0]
cotton_2010 <- cotton_2010[cott_a != 0]
cotton_2020 <- cotton_2020[COTT_A != 0]

# 31 Other Fibre Crops
# ofib_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
ofib_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "ofib_a"))
ofib_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "ofib_a"))
ofib_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OFIB_A"))

ofib_2005 <- ofib_2005[ofib_a != 0]
ofib_2010 <- ofib_2010[ofib_a != 0]
ofib_2020 <- ofib_2020[OFIB_A != 0]

# 32 Arabic Coffee
coff_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "coff")) # only coffee
coff_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "acof_a"))
coff_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "acof_a"))
coff_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "COFF_A"))

coff_2000 <- coff_2000[coff != 0]
coff_2005 <- coff_2005[acof_a != 0]
coff_2010 <- coff_2010[acof_a != 0]
coff_2020 <- coff_2020[COFF_A != 0]

# 33 Robust Coffee
# rcof_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
rcof_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "rcof_a"))
rcof_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "rcof_a"))
rcof_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "RCOF_A"))

rcof_2005 <- rcof_2005[rcof_a != 0]
rcof_2010 <- rcof_2010[rcof_a != 0]
rcof_2020 <- rcof_2020[RCOF_A != 0]

# 34 Cocoa
# coco_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
coco_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "coco_a"))
coco_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "coco_a"))
coco_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "COCO_A"))

coco_2005 <- coco_2005[coco_a != 0]
coco_2010 <- coco_2010[coco_a != 0]
coco_2020 <- coco_2020[COCO_A != 0]

# 35 Tea
# teas_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
teas_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "teas_a"))
teas_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "teas_a"))
teas_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TEAS_A"))

teas_2005 <- teas_2005[teas_a != 0]
teas_2010 <- teas_2010[teas_a != 0]
teas_2020 <- teas_2020[TEAS_A != 0]

# 36 Tobacco
# tobacco_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
tobacco_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "toba_a"))
tobacco_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "toba_a"))
tobacco_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TOBA_A"))

tobacco_2005 <- tobacco_2005[toba_a != 0]
tobacco_2010 <- tobacco_2010[toba_a != 0]
tobacco_2020 <- tobacco_2020[TOBA_A != 0]

# 37 Banana
banana_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "banp")) # banana + plantain
banana_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                      select = c("x", "y", "bana_a"))
banana_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "bana_a"))
banana_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "BANA_A"))

banana_2000 <- banana_2000[banp != 0]
banana_2005 <- banana_2005[bana_a != 0]
banana_2010 <- banana_2010[bana_a != 0]
banana_2020 <- banana_2020[BANA_A != 0]

# 38 Plantain
# plantain_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
plantain_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "plnt_a"))
plantain_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "plnt_a"))
plantain_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "PLNT_A"))

plantain_2005 <- plantain_2005[plnt_a != 0]
plantain_2010 <- plantain_2010[plnt_a != 0]
plantain_2020 <- plantain_2020[PLNT_A != 0]

# 39 Citrus
# citrus_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
# citrus_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
# citrus_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                        select = c("x", "y", "swpo_a"))
citrus_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "CITR_A"))

citrus_2020 <- citrus_2020[CITR_A != 0]

# 40 Other Tropical Fruit
# trof_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
trof_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "trof_a"))
trof_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "trof_a"))
trof_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "TROF_A"))

trof_2005 <- trof_2005[trof_a != 0]
trof_2010 <- trof_2010[trof_a != 0]
trof_2020 <- trof_2020[TROF_A != 0]

# 41 Temperate Fruit
# temf_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
temf_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "temf_a"))
temf_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "temf_a"))
temf_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TEMF_A"))

temf_2005 <- temf_2005[temf_a != 0]
temf_2010 <- temf_2010[temf_a != 0]
temf_2020 <- temf_2020[TEMF_A != 0]

# 42 Tomato
# tomato_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# tomato_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# tomato_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                    select = c("x", "y", "swpo_a"))
tomato_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TOMA_A"))

tomato_2020 <- tomato_2020[TOMA_A != 0]

# 43 Onion
# onion_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
# onion_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
# onion_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                      select = c("x", "y", "swpo_a"))
onion_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "ONIO_A"))

onion_2020 <- onion_2020[ONIO_A != 0]

# 44 Other Vegetables
# vege_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                     select = c("x", "y", "swpy"))
vege_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                    select = c("x", "y", "vege_a"))
vege_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                    select = c("x", "y", "vege_a"))
vege_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                    select = c("x", "y", "VEGE_A"))

vege_2005 <- vege_2005[vege_a != 0]
vege_2010 <- vege_2010[vege_a != 0]
vege_2020 <- vege_2020[VEGE_A != 0]

# 45 Rubber
# rubber_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# rubber_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# rubber_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                    select = c("x", "y", "swpo_a"))
rubber_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "RUBB_A"))
rubber_2020 <- rubber_2020[RUBB_A != 0]

# 46 Rest Of Crops
rest_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "othe"))
rest_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "rest_a"))
rest_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "rest_a"))
rest_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "REST_A"))

rest_2000 <- rest_2000[othe != 0]
rest_2005 <- rest_2005[rest_a != 0]
rest_2010 <- rest_2010[rest_a != 0]
rest_2020 <- rest_2020[REST_A != 0]

# 删除 SWPO_A 列中值为 0 的整行
SWPO_2000_Y <- SWPO_2000_Y[swpy != 0]
SWPO_2010_Y <- SWPO_2010_Y[swpo_a != 0]
SWPO_2020_Y <- SWPO_2020_Y[SWPO_A != 0]

# 将Yield(kg/ha)转化成营养成分（.../100g)
# 插列
# Sweer potato, boiled
SWPO_2000_Y$Water_g <- SWPO_2000_Y$swpy*76*10
SWPO_2000_Y$Ca_g <- SWPO_2000_Y$swpy*19*10
SWPO_2000_Y$Zn_g <- SWPO_2000_Y$swpy*0.2*10

SWPO_2010_Y$Water_g <- SWPO_2010_Y$swpo_a*76*10
SWPO_2010_Y$Ca_g <- SWPO_2010_Y$swpo_a*19*10
SWPO_2010_Y$Zn_g <- SWPO_2010_Y$swpo_a*0.2*10

SWPO_2020_Y$Water_g <- SWPO_2020_Y$SWPO_A*76*10
SWPO_2020_Y$Ca_g <- SWPO_2020_Y$SWPO_A*19*10
SWPO_2020_Y$Zn_g <- SWPO_2020_Y$SWPO_A*0.2*10

coords_2000 = SWPO_2000_Y[,c("x","y")] #坐标
coords_2010 = SWPO_2010_Y[,c("x","y")]
coords_2020 = SWPO_2020_Y[,c("x","y")]

cell_size = 0.083 #网格单元的大小，以度为单位

Yields_2000 = data.frame(SWPO_2000_Y$swpy)
Yields_2010 = data.frame(SWPO_2010_Y$swpo_a)
Yields_2020 = data.frame(SWPO_2020_Y$SWPO_A)

Y_sp_2000 <- SpatialPointsDataFrame(coords=coords_2000, data=Yields_2000) #存储空间点数据和相关属性数据的框架。它是sp包中的一个类
Y_sp_2010 <- SpatialPointsDataFrame(coords=coords_2010, data=Yields_2010)
Y_sp_2020 <- SpatialPointsDataFrame(coords=coords_2020, data=Yields_2020)

lon_min <- -176.2083; lon_max <- 179.5417; lat_min <- -54.79167; lat_max <- 69.9583
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1 #计算出网格的列数和行数，基于给定的经纬度范围和网格单元大小
grid <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=cell_size, crs="+proj=longlat +datum=WGS84")

Y_Water_2000 <- rasterize(SWPO_2000_Y[, c("x", "y")], grid, SWPO_2000_Y[, 'Water_g'], fun=mean)
Y_Water_2010 <- rasterize(SWPO_2010_Y[, c("x", "y")], grid, SWPO_2010_Y[, 'Water_g'], fun=mean)
Y_Water_2020 <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Water_g'], fun=mean)

Y_Ca_2000<- rasterize(SWPO_2000_Y[, c("x", "y")], grid, SWPO_2000_Y[, 'Ca_g'], fun=mean)
Y_Ca_2010<- rasterize(SWPO_2010_Y[, c("x", "y")], grid, SWPO_2010_Y[, 'Ca_g'], fun=mean)
Y_Ca_2020<- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Ca_g'], fun=mean)

Y_Zn_2000 <- rasterize(SWPO_2000_Y[, c("x", "y")], grid, SWPO_2000_Y[, 'Zn_g'], fun=mean)
Y_Zn_2010 <- rasterize(SWPO_2010_Y[, c("x", "y")], grid, SWPO_2010_Y[, 'Zn_g'], fun=mean)
Y_Zn_2020 <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'Zn_g'], fun=mean)

plot(Y_Water_2000, main="Water_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_Water_2010, main="Water_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_Water_2020, main="Water_g/ha",xlab="Longitude", ylab="Latitude")

plot(Y_Ca_2000, main="Ca_mg/ha",xlab="Longitude", ylab="Latitude")
plot(Y_Ca_2010, main="Ca_mg/ha",xlab="Longitude", ylab="Latitude")
plot(Y_Ca_2020, main="Ca_mg/ha",xlab="Longitude", ylab="Latitude")

plot(Y_Zn_2000, main="Zn_mg/ha",xlab="Longitude", ylab="Latitude")
plot(Y_Zn_2010, main="Zn_mg/ha",xlab="Longitude", ylab="Latitude")
plot(Y_Zn_2020, main="Zn_mg/ha",xlab="Longitude", ylab="Latitude")

# Sweet potato, red, unpeeled, raw
SWPO_2000_Y$PROT_g_raw <- SWPO_2000_Y$swpy*0.549699*10
SWPO_2000_Y$FAT_g_raw <- SWPO_2000_Y$swpy*0.629037*10
SWPO_2000_Y$FIBER_g_raw <- SWPO_2000_Y$swpy*1.290187*10

SWPO_2010_Y$PROT_g_raw <- SWPO_2010_Y$swpo_a*0.549699*10
SWPO_2010_Y$FAT_g_raw <- SWPO_2010_Y$swpo_a*0.629037*10
SWPO_2010_Y$FIBER_g_raw <- SWPO_2010_Y$swpo_a*1.290187*10

SWPO_2020_Y$PROT_g_raw <- SWPO_2020_Y$SWPO_A*0.549699*10
SWPO_2020_Y$FAT_g_raw <- SWPO_2020_Y$SWPO_A*0.629037*10
SWPO_2020_Y$FIBER_g_raw <- SWPO_2020_Y$SWPO_A*1.290187*10

Y_PROT_2000 <- rasterize(SWPO_2000_Y[, c("x", "y")], grid, SWPO_2000_Y[, 'PROT_g_raw'], fun=mean)
Y_PROT_2010 <- rasterize(SWPO_2010_Y[, c("x", "y")], grid, SWPO_2010_Y[, 'PROT_g_raw'], fun=mean)
Y_PROT_2020 <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'PROT_g_raw'], fun=mean)

Y_FAT_2000 <- rasterize(SWPO_2000_Y[, c("x", "y")], grid, SWPO_2000_Y[, 'FAT_g_raw'], fun=mean)
Y_FAT_2010 <- rasterize(SWPO_2010_Y[, c("x", "y")], grid, SWPO_2010_Y[, 'FAT_g_raw'], fun=mean)
Y_FAT_2020 <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'FAT_g_raw'], fun=mean)

Y_FIBER_2000 <- rasterize(SWPO_2000_Y[, c("x", "y")], grid, SWPO_2000_Y[, 'FIBER_g_raw'], fun=mean)
Y_FIBER_2010 <- rasterize(SWPO_2010_Y[, c("x", "y")], grid, SWPO_2010_Y[, 'FIBER_g_raw'], fun=mean)
Y_FIBER_2020 <- rasterize(SWPO_2020_Y[, c("x", "y")], grid, SWPO_2020_Y[, 'FIBER_g_raw'], fun=mean)

plot(Y_PROT_2000, main="PROT_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_PROT_2010, main="PROT_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_PROT_2020, main="PROT_g/ha",xlab="Longitude", ylab="Latitude")

plot(Y_FAT_2000, main="FAT_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_FAT_2010, main="FAT_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_FAT_2020, main="FAT_g/ha",xlab="Longitude", ylab="Latitude")

plot(Y_FIBER_2000, main="FIBER_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_FIBER_2010, main="FIBER_g/ha",xlab="Longitude", ylab="Latitude")
plot(Y_FIBER_2020, main="FIBER_g/ha",xlab="Longitude", ylab="Latitude")
