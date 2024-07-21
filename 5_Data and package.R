rm(list = ls()) # 清空环境变量
pacman::p_load(sf,raster,ggplot2, ggspatial, tidyverse, rgdal, tmap)#所需要的包
library(raster)
library(rasterVis)
library(data.table)

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

# 2 Rice
rice_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "rice"))
rice_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "rice_a"))
rice_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "rice_a"))
rice_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "RICE_A"))  
  
# 3 Maize
maize_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                  select = c("x", "y", "maiz"))
maize_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                  select = c("x", "y", "maiz_a"))
maize_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "maiz_a"))
maize_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "MAIZ_A"))  

# 4 Barley
barley_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "barl"))
barley_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                   select = c("x", "y", "barl_a"))
barley_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "barl_a"))
barley_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "BARL_A"))  

# 5 Small Millet
mill_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                    select = c("x", "y", "mill"))
mill_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                    select = c("x", "y", "smil_a"))
mill_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                    select = c("x", "y", "smil_a"))
mill_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                    select = c("x", "y", "MILL_A"))  
# 6 Pearl Millet
# pmil_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                   select = c("x", "y", "swpy"))
# pmil_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
#                   select = c("x", "y", "swpo_a"))
pmil_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "pmil_a"))
pmil_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "PMIL_A"))  

# 7 Sorghum
sorghum_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "sorg"))
sorghum_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                  select = c("x", "y", "sorg_a"))
sorghum_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "sorg_a"))
sorghum_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "SORG_A"))  

# 8 Other Cereals
# ocer_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                   select = c("x", "y", "swpy"))
ocer_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                     select = c("x", "y", "ocer_a"))
ocer_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "ocer_a"))
ocer_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OCER_A"))  

# 9 Potato
potato_2000 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                    select = c("x", "y", "pota"))
potato_2005 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2005_global_Y_TA.csv", 
                  select = c("x", "y", "pota_a"))
potato_2010 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                  select = c("x", "y", "pota_a"))
potato_2020 = fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                  select = c("x", "y", "POTA_A"))  

# 10 Sweet Potato
swpo_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "swpy"))
swpo_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "swpo_a"))
swpo_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "swpo_a"))
swpo_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "SWPO_A")) 

# 11 Yams
# yams_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
yams_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "yams_a"))
yams_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "yams_a"))
yams_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "YAMS_A")) 

# 12 Cassava
cassava_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "cass"))
cassava_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "cass_a"))
cassava_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "cass_a"))
cassava_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "CASS_A")) 

# 13 Other Roots
# orts_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                       select = c("x", "y", "swpy"))
orts_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "orts_a"))
orts_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "orts_a"))
orts_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "ORTS_A")) 

# 14 Bean
bean_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "bean"))
bean_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "bean_a"))
bean_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "bean_a"))
bean_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "BEAN_A")) 

# 15 Chickpea
# chickpea_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
chickpea_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "chic_a"))
chickpea_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "chic_a"))
chickpea_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "CHIC_A")) 

# 16 Cowpea
# cowpea_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
cowpea_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                       select = c("x", "y", "cowp_a"))
cowpea_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                       select = c("x", "y", "cowp_a"))
cowpea_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "COWP_A")) 

# 17 Pigeon Pea
# pige_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
pige_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "pige_a"))
pige_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "pige_a"))
pige_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "PIGE_A")) 

# 18 Lentil
# lentil_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
lentil_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "lent_a"))
lentil_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "lent_a"))
lentil_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "LENT_A"))

# 19 Other Pulses
# opul_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
opul_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "opul_a"))
opul_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "opul_a"))
opul_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OPUL_A"))

# 20 Soybean
soybean_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "soyb"))
soybean_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "soyb_a"))
soybean_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "soyb_a"))
soybean_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "SOYB_A"))

# 21 Groundnut
groundnut_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "grou"))
groundnut_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "grou_a"))
groundnut_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "grou_a"))
groundnut_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "GROU_A"))

# 22 Coconut
# coconut_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                         select = c("x", "y", "swpy"))
coconut_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "cnut_a"))
coconut_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "cnut_a"))
coconut_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "CNUT_A"))

# 23 Oilpalm
# oilpalm_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                       select = c("x", "y", "swpy"))
oilpalm_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "oilp_a"))
oilpalm_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "oilp_a"))
oilpalm_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "OILP_A"))

# 24 Sunflower
# sunflower_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                       select = c("x", "y", "swpy"))
sunflower_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "sunf_a"))
sunflower_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "sunf_a"))
sunflower_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "SUNF_A"))

# 25 Rapeseed
# rapeseed_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                         select = c("x", "y", "swpy"))
rapeseed_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "rape_a"))
rapeseed_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "rape_a"))
rapeseed_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "RAPE_A"))

# 26 Sesame Seed
# sesame_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
sesame_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                       select = c("x", "y", "sesa_a"))
sesame_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                       select = c("x", "y", "sesa_a"))
sesame_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "SESA_A"))

# 27 Other Oil Crops
ooil_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "ooil"))
ooil_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "ooil_a"))
ooil_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "ooil_a"))
ooil_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "SWPO_A"))

# 28 Sugarcane
sugarcane_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "sugc"))
sugarcane_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "sugc_a"))
sugarcane_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "sugc_a"))
sugarcane_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "SUGC_A"))

# 29 Sugarbeet
sugarbeet_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "sugb"))
sugarbeet_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "sugb_a"))
sugarbeet_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "sugb_a"))
sugarbeet_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "SUGB_A"))

# 30 Cotton
cotton_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "cott"))
cotton_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                        select = c("x", "y", "cott_a"))
cotton_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                        select = c("x", "y", "cott_a"))
cotton_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                        select = c("x", "y", "COTT_A"))

# 31 Other Fibre Crops
# ofib_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
ofib_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "ofib_a"))
ofib_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "ofib_a"))
ofib_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "OFIB_A"))

# 32 Arabic Coffee
coff_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "coff")) # only coffee
coff_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "acof_a"))
coff_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "acof_a"))
coff_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "COFF_A"))

# 33 Robust Coffee
# rcof_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
rcof_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "rcof_a"))
rcof_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "rcof_a"))
rcof_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "RCOF_A"))

# 34 Cocoa
# coco_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
coco_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "coco_a"))
coco_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "coco_a"))
coco_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "COCO_A"))

# 35 Tea
# teas_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
teas_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "teas_a"))
teas_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "teas_a"))
teas_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TEAS_A"))

# 36 Tobacco
# tobacco_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
tobacco_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "toba_a"))
tobacco_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "toba_a"))
tobacco_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TOBA_A"))

# 37 Banana
banana_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "banp")) # banana + plantain
banana_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                      select = c("x", "y", "bana_a"))
banana_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                      select = c("x", "y", "bana_a"))
banana_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                      select = c("x", "y", "BANA_A"))

# 38 Plantain
# plantain_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
plantain_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "plnt_a"))
plantain_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "plnt_a"))
plantain_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "PLNT_A"))

# 39 Citrus
# citrus_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
# citrus_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                        select = c("x", "y", "swpy"))
# citrus_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                        select = c("x", "y", "swpo_a"))
citrus_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                       select = c("x", "y", "CITR_A"))

# 40 Other Tropical Fruit
# trof_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
trof_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "trof_a"))
trof_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "trof_a"))
trof_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "TROF_A"))

# 41 Temperate Fruit
# temf_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
temf_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                   select = c("x", "y", "temf_a"))
temf_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                   select = c("x", "y", "temf_a"))
temf_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TEMF_A"))

# 42 Tomato
# tomato_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# tomato_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# tomato_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                    select = c("x", "y", "swpo_a"))
tomato_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "TOMA_A"))

# 43 Onion
# onion_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
# onion_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                      select = c("x", "y", "swpy"))
# onion_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                      select = c("x", "y", "swpo_a"))
onion_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "ONIO_A"))

# 44 Other Vegetables
# vege_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                     select = c("x", "y", "swpy"))
vege_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                    select = c("x", "y", "vege_a"))
vege_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                    select = c("x", "y", "vege_a"))
vege_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                    select = c("x", "y", "VEGE_A"))

# 45 Rubber
# rubber_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# rubber_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
#                    select = c("x", "y", "swpy"))
# rubber_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
#                    select = c("x", "y", "swpo_a"))
rubber_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                   select = c("x", "y", "RUBB_A"))

# 46 Rest Of Crops
rest_2000 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "othe"))
rest_2005 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2000_global_Y_TA.csv", 
                     select = c("x", "y", "rest_a"))
rest_2010 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2010_global_Y_TA.csv", 
                     select = c("x", "y", "rest_a"))
rest_2020 <- fread("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_Y_TA.csv", 
                     select = c("x", "y", "REST_A"))

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
