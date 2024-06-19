# 世界地图矢量文件和其他数据集叠加
# 教程链接：https://www.bilibili.com/video/BV15a411n7Eu/?p=1&vd_source=7b7c737e7bab76b094d60365f927d49e
# 教程链接：https://mp.weixin.qq.com/s/LD2ZaCU-c9BT4OXZ4U6Ibw

# 时间序列分析
# 教程链接

# 清空环境中的所有变量
rm(list = ls())

# 空间数据准备
library(raster)
library(terra)
library(ggplot2)
library(maps)

#根据你的实际文件路径修改下面的代码
setwd("F:/2024-06-19/1_r")

# 获取世界地图坐标
world <- map_data("world")

# 绘制世界地图
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  )


plot(SWPO_A_2020)
# -------------------------------
pacman::p_load(sf, raster,ggplot2, ggspatial)#所需要的包
shp <- sf::read_sf("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/World_countries.shp")#读入矢量边界（实际读入的是shx文件）

SWPO_2005_Y <- raster("F:/2024-06-19/2_tiff/Sweet potato/SPAM_2005_global_Y_TA_SWPO_A.tif")
SWPO_2005_H <- raster("F:/2024-06-19/2_tiff/Sweet potato/SPAM_2005_global_H_TA_SWPO_A.tif")
SWPO_2010_Y <- raster("F:/2024-06-19/2_tiff/Sweet potato/SPAM_2010_global_Y_SWPO_A.tif")
SWPO_2010_H <- raster("F:/2024-06-19/2_tiff/Sweet potato/SPAM_2010_global_H_SWPO_A.tif")
SWPO_2020_Y <- raster("F:/2024-06-19/2_tiff/Sweet potato/SPAM_2020_global_Y_SWPO_A.tif")
SWPO_2020_H <- raster("F:/2024-06-19/2_tiff/Sweet potato/SPAM_2020_global_H_SWPO_A.tif")

BNA_2005_Y <- raster("F:/2024-06-19/2_tiff/Banana/SPAM_2005_global_Y_BANA_A.tif")
BNA_2005_H <- raster("F:/2024-06-19/2_tiff/Banana/SPAM_2005_global_H_BANA_A.tif")
BNA_2010_Y <- raster("F:/2024-06-19/2_tiff/Banana/SPAM_2010_global_Y_BANA_A.tif")
BNA_2010_H <- raster("F:/2024-06-19/2_tiff/Banana/SPAM_2010_global_H_BANA_A.tif")
BNA_2020_Y <- raster("F:/2024-06-19/2_tiff/Banana/SPAM_2020_global_Y_BANA_A.tif")
BNA_2020_H <- raster("F:/2024-06-19/2_tiff/Banana/SPAM_2020_global_H_BANA_A.tif")

VEGE_2005_Y <- raster("F:/2024-06-19/2_tiff/Other vegetables/SPAM_2005_global_Y_VEGE_A.tif")
VEGE_2005_H <- raster("F:/2024-06-19/2_tiff/Other vegetables/SPAM_2005_global_H_VEGE_A.tif")
VEGE_2010_Y <- raster("F:/2024-06-19/2_tiff/Other vegetables/SPAM_2010_global_Y_VEGE_A.tif")
VEGE_2010_H <- raster("F:/2024-06-19/2_tiff/Other vegetables/SPAM_2010_global_H_VEGE_A.tif")
VEGE_2020_Y <- raster("F:/2024-06-19/2_tiff/Other vegetables/SPAM_2020_global_Y_VEGE_A.tif")
VEGE_2020_H <- raster("F:/2024-06-19/2_tiff/Other vegetables/SPAM_2020_global_H_VEGE_A.tif")

TROF_2005_Y <- raster("F:/2024-06-19/2_tiff/Other tropical fruits/SPAM_2005_global_Y_TROF_A.tif")
TROF_2005_H <- raster("F:/2024-06-19/2_tiff/Other tropical fruits/SPAM_2005_global_H_TROF_A.tif")
TROF_2010_Y <- raster("F:/2024-06-19/2_tiff/Other tropical fruits/SPAM_2010_global_Y_TROF_A.tif")
TROF_2010_H <- raster("F:/2024-06-19/2_tiff/Other tropical fruits/SPAM_2010_global_H_TROF_A.tif")
TROF_2020_Y <- raster("F:/2024-06-19/2_tiff/Other tropical fruits/SPAM_2020_global_Y_TROF_A.tif")
TROF_2020_H <- raster("F:/2024-06-19/2_tiff/Other tropical fruits/SPAM_2020_global_H_TROF_A.tif")

df=as.data.frame(TROF_2020_H,xy=T) 

colnames(df)=c("x","y","LandCover")#数据转化为dataframe
ggplot(df  %>% na.omit() ) +
  geom_raster(aes(x,y,fill=LandCover))+
  scale_fill_gradientn(colours =rainbow(10))+
  labs(x=NULL,y=NULL)+
  geom_sf(size = .2, fill = "transparent", color = "black", data = shp)+ #绘制矢量边界
  annotation_scale(location = "bl") + #设置比例尺
  annotation_north_arrow(location="tl",
                         style = north_arrow_nautical(
                           fill = c("grey40","white"),
                           line_col = "grey20"))+  # 添加指北针
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),# 去除地图网格
        panel.background = element_blank(),
        legend.title = element_blank())+
  theme(axis.ticks.length=unit(-0.1, "cm"),  
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")) )+
  theme(axis.text.x = element_text(angle=0,hjust=1),   # 旋转坐标轴label的方向
        text = element_text(size = 12, face = "bold", family="serif"),  
        panel.spacing = unit(0,"lines") ) #分面间隔设为零

ggsave("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/3_results/Other tropical fruits/TROF_2020_H.jpg",dpi=300)
# ggsave("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/3_results/Sweet potato/BNA_2005_Y.jpg",dpi=300)#保存图片

# 统一两幅栅格的坐标系、分辨率、行列号 作者：孤独的王大老实 https://www.bilibili.com/read/cv15936502/ 出处：bilibili
BNA_2005_Y_2 <- resample(BNA_2005_Y, BNA_2020_Y, method="bilinear")
BNA_2005_2020_Y <- overlay(BNA_2005_Y_2, BNA_2020_Y, fun=function(x,y){return(y-x)}) # 栅格相减https://blog.csdn.net/qq_32832803/article/details/110792274
ggsave("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/3_results/Banana/Banana_2005_2020.jpg",dpi=300)

# 检查两个栅格对象的范围
extent1 <- extent(BNA_2005_Y_2)
extent2 <- extent(BNA_2020_Y)

# 两幅栅格的相关分析和线性回归
# https://www.bilibili.com/read/cv15936502/
# Import packages
library(raster)
library(tidyverse) 
# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
# Load data
BHI_2005 = raster("F:/2024-06-19/2_tiff/BHI/BILBI_P_BHIv2_Habitat_2005.tif")
BHI_2010 = raster("F:/2024-06-19/2_tiff/BHI/BILBI_P_BHIv2_Habitat_2010.tif")
BHI_2020 = raster("F:/2024-06-19/2_tiff/BHI/BILBI_P_BHIv2_Habitat_2020.tif")

# Resample the ndvi based on bt
BNA_2005_Y_2 <- resample(BNA_2005_Y, BHI_2005, method="bilinear")
BNA_2010_Y_2 <- resample(BNA_2010_Y, BHI_2010, method="bilinear")
BNA_2020_Y_2 <- resample(BNA_2020_Y, BHI_2020, method="bilinear")

BNA_2005_H_2 <- resample(BNA_2005_H, BHI_2005, method="bilinear")
BNA_2010_H_2 <- resample(BNA_2010_H, BHI_2010, method="bilinear")
BNA_2020_H_2 <- resample(BNA_2020_H, BHI_2020, method="bilinear")

SWPO_2005_Y_2 <- resample(SWPO_2005_Y, BHI_2005, method="bilinear")
SWPO_2010_Y_2 <- resample(SWPO_2010_Y, BHI_2010, method="bilinear")
SWPO_2020_Y_2 <- resample(SWPO_2020_Y, BHI_2020, method="bilinear")

SWPO_2005_H_2 <- resample(SWPO_2005_H, BHI_2005, method="bilinear")
SWPO_2010_H_2 <- resample(SWPO_2010_H, BHI_2010, method="bilinear")
SWPO_2020_H_2 <- resample(SWPO_2020_H, BHI_2020, method="bilinear")

VEGE_2005_Y_2 <- resample(VEGE_2005_Y, BHI_2005, method="bilinear")
VEGE_2010_Y_2 <- resample(VEGE_2010_Y, BHI_2010, method="bilinear")
VEGE_2020_Y_2 <- resample(VEGE_2020_Y, BHI_2020, method="bilinear")

VEGE_2005_H_2 <- resample(VEGE_2005_H, BHI_2005, method="bilinear")
VEGE_2010_H_2 <- resample(VEGE_2010_H, BHI_2010, method="bilinear")
VEGE_2020_H_2 <- resample(VEGE_2020_H, BHI_2020, method="bilinear")

TROF_2005_Y_2 <- resample(TROF_2005_Y, BHI_2005, method="bilinear")
TROF_2010_Y_2 <- resample(TROF_2010_Y, BHI_2010, method="bilinear")
TROF_2020_Y_2 <- resample(TROF_2020_Y, BHI_2020, method="bilinear")

TROF_2005_H_2 <- resample(TROF_2005_H, BHI_2005, method="bilinear")
TROF_2010_H_2 <- resample(TROF_2010_H, BHI_2010, method="bilinear")
TROF_2020_H_2 <- resample(TROF_2020_H, BHI_2020, method="bilinear")

# 把缺失的点去除
# Masking rasters
BHI_BNA_2005_Y_m <- mask(BHI_2005, BNA_2005_Y_2)
BHI_BNA_2010_Y_m <- mask(BHI_2010, BNA_2010_Y_2)
BHI_BNA_2020_Y_m <- mask(BHI_2020, BNA_2020_Y_2)

BNA_2005_Y_m <- mask(BNA_2005_Y_2, BHI_2005)
BNA_2010_Y_m <- mask(BNA_2010_Y_2, BHI_2010)
BNA_2020_Y_m <- mask(BNA_2020_Y_2, BHI_2020)

BHI_BNA_2005_H_m <- mask(BHI_2005, BNA_2005_H_2)
BHI_BNA_2010_H_m <- mask(BHI_2010, BNA_2010_H_2)
BHI_BNA_2020_H_m <- mask(BHI_2020, BNA_2020_H_2)

BNA_2005_H_m <- mask(BNA_2005_H_2, BHI_2005)
BNA_2010_H_m <- mask(BNA_2010_H_2, BHI_2010)
BNA_2020_H_m <- mask(BNA_2020_H_2, BHI_2020)

BHI_SWPO_2005_Y_m <- mask(BHI_2005, SWPO_2005_Y_2)
BHI_SWPO_2010_Y_m <- mask(BHI_2010, SWPO_2010_Y_2)
BHI_SWPO_2020_Y_m <- mask(BHI_2020, SWPO_2020_Y_2)

SWPO_2005_Y_m <- mask(SWPO_2005_Y_2, BHI_2005)
SWPO_2010_Y_m <- mask(SWPO_2010_Y_2, BHI_2010)
SWPO_2020_Y_m <- mask(SWPO_2020_Y_2, BHI_2020)

BHI_SWPO_2005_H_m <- mask(BHI_2005, SWPO_2005_H_2)
BHI_SWPO_2010_H_m <- mask(BHI_2010, SWPO_2010_H_2)
BHI_SWPO_2020_H_m <- mask(BHI_2020, SWPO_2020_H_2)

SWPO_2005_H_m <- mask(SWPO_2005_H_2, BHI_2005)
SWPO_2010_H_m <- mask(SWPO_2010_H_2, BHI_2010)
SWPO_2020_H_m <- mask(SWPO_2020_H_2, BHI_2020)

BHI_VEGE_2005_Y_m <- mask(BHI_2005, VEGE_2005_Y_2)
BHI_VEGE_2010_Y_m <- mask(BHI_2010, VEGE_2010_Y_2)
BHI_VEGE_2020_Y_m <- mask(BHI_2020, VEGE_2020_Y_2)

VEGE_2005_Y_m <- mask(VEGE_2005_Y_2, BHI_2005)
VEGE_2010_Y_m <- mask(VEGE_2010_Y_2, BHI_2010)
VEGE_2020_Y_m <- mask(VEGE_2020_Y_2, BHI_2020)

BHI_VEGE_2005_H_m <- mask(BHI_2005, VEGE_2005_H_2)
BHI_VEGE_2010_H_m <- mask(BHI_2010, VEGE_2010_H_2)
BHI_VEGE_2020_H_m <- mask(BHI_2020, VEGE_2020_H_2)

VEGE_2005_H_m <- mask(VEGE_2005_H_2, BHI_2005)
VEGE_2010_H_m <- mask(VEGE_2010_H_2, BHI_2010)
VEGE_2020_H_m <- mask(VEGE_2020_H_2, BHI_2020)

BHI_TROF_2005_Y_m <- mask(BHI_2005, TROF_2005_Y_2)
BHI_TROF_2010_Y_m <- mask(BHI_2010, TROF_2010_Y_2)
BHI_TROF_2020_Y_m <- mask(BHI_2020, TROF_2020_Y_2)

TROF_2005_Y_m <- mask(TROF_2005_Y_2, BHI_2005)
TROF_2010_Y_m <- mask(TROF_2010_Y_2, BHI_2010)
TROF_2020_Y_m <- mask(TROF_2020_Y_2, BHI_2020)

BHI_TROF_2005_H_m <- mask(BHI_2005, TROF_2005_H_2)
BHI_TROF_2010_H_m <- mask(BHI_2010, TROF_2010_H_2)
BHI_TROF_2020_H_m <- mask(BHI_2020, TROF_2020_H_2)

TROF_2005_H_m <- mask(TROF_2005_H_2, BHI_2005)
TROF_2010_H_m <- mask(TROF_2010_H_2, BHI_2010)
TROF_2020_H_m <- mask(TROF_2020_H_2, BHI_2020)
########################数据跑到了这里
# Plot
plot(BHI_2005_m)
plot(BHI_2010_m)
plot(BHI_2020_m)

plot(BNA_2005_Y_m)
plot(BNA_2010_Y_m)
plot(BNA_2020_Y_m)

plot(SWPO_2005_Y_m)
plot(SWPO_2010_Y_m)
plot(SWPO_2020_Y_m)

plot(TROF_2005_Y_m)
plot(TROF_2010_Y_m)
plot(TROF_2020_Y_m)

# 把栅格转为数据框并进行相关性分析
overlay_BHI_BNA_2005_Y <- stack(BHI_BNA_2005_Y_m, BNA_2005_Y_m)
overlay_BHI_BNA_2010_Y <- stack(BHI_BNA_2010_Y_m, BNA_2010_Y_m)
overlay_BHI_BNA_2020_Y <- stack(BHI_BNA_2020_Y_m, BNA_2020_Y_m)

overlay_BHI_BNA_2005_H <- stack(BHI_BNA_2005_H_m, BNA_2005_H_m)
overlay_BHI_BNA_2010_H <- stack(BHI_BNA_2010_H_m, BNA_2010_H_m)
overlay_BHI_BNA_2020_H <- stack(BHI_BNA_2020_H_m, BNA_2020_H_m)

overlay_BHI_SWPO_2005_Y <- stack(BHI_SWPO_2005_Y_m, SWPO_2005_Y_m)
overlay_BHI_SWPO_2010_Y <- stack(BHI_SWPO_2010_Y_m, SWPO_2010_Y_m)
overlay_BHI_SWPO_2020_Y <- stack(BHI_SWPO_2020_Y_m, SWPO_2020_Y_m)

overlay_BHI_SWPO_2005_H <- stack(BHI_SWPO_2005_H_m, SWPO_2005_H_m)
overlay_BHI_SWPO_2010_H <- stack(BHI_SWPO_2010_H_m, SWPO_2010_H_m)
overlay_BHI_SWPO_2020_H <- stack(BHI_SWPO_2020_H_m, SWPO_2020_H_m)

overlay_BHI_VEGE_2005_Y <- stack(BHI_VEGE_2005_Y_m, VEGE_2005_Y_m)
overlay_BHI_VEGE_2010_Y <- stack(BHI_VEGE_2010_Y_m, VEGE_2010_Y_m)
overlay_BHI_VEGE_2020_Y <- stack(BHI_VEGE_2020_Y_m, VEGE_2020_Y_m)

overlay_BHI_VEGE_2005_H <- stack(BHI_VEGE_2005_H_m, VEGE_2005_H_m)
overlay_BHI_VEGE_2010_H <- stack(BHI_VEGE_2010_H_m, VEGE_2010_H_m)
overlay_BHI_VEGE_2020_H <- stack(BHI_VEGE_2020_H_m, VEGE_2020_H_m)

overlay_BHI_TROF_2005_Y <- stack(BHI_TROF_2005_Y_m, TROF_2005_Y_m)
overlay_BHI_TROF_2010_Y <- stack(BHI_TROF_2010_Y_m, TROF_2010_Y_m)
overlay_BHI_TROF_2020_Y <- stack(BHI_TROF_2020_Y_m, TROF_2020_Y_m)

overlay_BHI_TROF_2005_H <- stack(BHI_TROF_2005_H_m, TROF_2005_H_m)
overlay_BHI_TROF_2010_H <- stack(BHI_TROF_2010_H_m, TROF_2010_H_m)
overlay_BHI_TROF_2020_H <- stack(BHI_TROF_2020_H_m, TROF_2020_H_m)

overlay_BHI_BNA_2005_Y <- data.frame(na.omit(values(overlay_BHI_BNA_2005_Y)))
overlay_BHI_BNA_2010_Y <- data.frame(na.omit(values(overlay_BHI_BNA_2010_Y)))
overlay_BHI_BNA_2020_Y <- data.frame(na.omit(values(overlay_BHI_BNA_2020_Y)))

overlay_BHI_BNA_2005_H <- data.frame(na.omit(values(overlay_BHI_BNA_2005_H)))
overlay_BHI_BNA_2010_H <- data.frame(na.omit(values(overlay_BHI_BNA_2010_H)))
overlay_BHI_BNA_2020_H <- data.frame(na.omit(values(overlay_BHI_BNA_2020_H)))

overlay_BHI_SWPO_2005_Y <- data.frame(na.omit(values(overlay_BHI_SWPO_2005_Y)))
overlay_BHI_SWPO_2010_Y <- data.frame(na.omit(values(overlay_BHI_SWPO_2010_Y)))
overlay_BHI_SWPO_2020_Y <- data.frame(na.omit(values(overlay_BHI_SWPO_2020_Y)))

overlay_BHI_SWPO_2005_H <- data.frame(na.omit(values(overlay_BHI_SWPO_2005_H)))
overlay_BHI_SWPO_2010_H <- data.frame(na.omit(values(overlay_BHI_SWPO_2010_H)))
overlay_BHI_SWPO_2020_H <- data.frame(na.omit(values(overlay_BHI_SWPO_2020_H)))

overlay_BHI_VEGE_2005_Y <- data.frame(na.omit(values(overlay_BHI_VEGE_2005_Y)))
overlay_BHI_VEGE_2010_Y <- data.frame(na.omit(values(overlay_BHI_VEGE_2010_Y)))
overlay_BHI_VEGE_2020_Y <- data.frame(na.omit(values(overlay_BHI_VEGE_2020_Y)))

overlay_BHI_VEGE_2005_H <- data.frame(na.omit(values(overlay_BHI_VEGE_2005_H)))
overlay_BHI_VEGE_2010_H <- data.frame(na.omit(values(overlay_BHI_VEGE_2010_H)))
overlay_BHI_VEGE_2020_H <- data.frame(na.omit(values(overlay_BHI_VEGE_2020_H)))

overlay_BHI_TROF_2005_Y <- data.frame(na.omit(values(overlay_BHI_TROF_2005_Y)))
overlay_BHI_TROF_2010_Y <- data.frame(na.omit(values(overlay_BHI_TROF_2010_Y)))
overlay_BHI_TROF_2020_Y <- data.frame(na.omit(values(overlay_BHI_TROF_2020_Y)))

overlay_BHI_TROF_2005_H <- data.frame(na.omit(values(overlay_BHI_TROF_2005_H)))
overlay_BHI_TROF_2010_H <- data.frame(na.omit(values(overlay_BHI_TROF_2010_H)))
overlay_BHI_TROF_2020_H <- data.frame(na.omit(values(overlay_BHI_TROF_2020_H)))

names(overlay_BHI_BNA_2005_Y) <- c("BHI_2005", "BNA_2005_Y")
names(overlay_BHI_BNA_2010_Y) <- c("BHI_2010", "BNA_2010_Y")
names(overlay_BHI_BNA_2020_Y) <- c("BHI_2020", "BNA_2020_Y")

names(overlay_BHI_BNA_2005_H) <- c("BHI_2005", "BNA_2005_H")
names(overlay_BHI_BNA_2010_H) <- c("BHI_2010", "BNA_2010_H")
names(overlay_BHI_BNA_2020_H) <- c("BHI_2020", "BNA_2020_H")

names(overlay_BHI_SWPO_2005_Y) <- c("BHI_2005", "SWPO_2005_Y")
names(overlay_BHI_SWPO_2010_Y) <- c("BHI_2010", "SWPO_2010_Y")
names(overlay_BHI_SWPO_2020_Y) <- c("BHI_2020", "SWPO_2020_Y")

names(overlay_BHI_SWPO_2005_H) <- c("BHI_2005", "SWPO_2005_H")
names(overlay_BHI_SWPO_2010_H) <- c("BHI_2010", "SWPO_2010_H")
names(overlay_BHI_SWPO_2020_H) <- c("BHI_2020", "SWPO_2020_H")

names(overlay_BHI_VEGE_2005_Y) <- c("BHI_2005", "VEGE_2005_Y")
names(overlay_BHI_VEGE_2010_Y) <- c("BHI_2010", "VEGE_2010_Y")
names(overlay_BHI_VEGE_2020_Y) <- c("BHI_2020", "VEGE_2020_Y")

names(overlay_BHI_VEGE_2005_H) <- c("BHI_2005", "VEGE_2005_H")
names(overlay_BHI_VEGE_2010_H) <- c("BHI_2010", "VEGE_2010_H")
names(overlay_BHI_VEGE_2020_H) <- c("BHI_2020", "VEGE_2020_H")

names(overlay_BHI_TROF_2005_Y) <- c("BHI_2005", "TROF_2005_Y")
names(overlay_BHI_TROF_2010_Y) <- c("BHI_2010", "TROF_2010_Y")
names(overlay_BHI_TROF_2020_Y) <- c("BHI_2020", "TROF_2020_Y")

names(overlay_BHI_TROF_2005_H) <- c("BHI_2005", "TROF_2005_H")
names(overlay_BHI_TROF_2010_H) <- c("BHI_2010", "TROF_2010_H")
names(overlay_BHI_TROF_2020_H) <- c("BHI_2020", "TROF_2020_H")

# correlation test
cor.test(overlay_BHI_BNA_2005_Y[,1], overlay_BHI_BNA_2005_Y[,2]) 
cor.test(overlay_BHI_BNA_2010_Y[,1], overlay_BHI_BNA_2010_Y[,2]) 
cor.test(overlay_BHI_BNA_2020_Y[,1], overlay_BHI_BNA_2020_Y[,2]) 

cor.test(overlay_BHI_BNA_2005_H[,1], overlay_BHI_BNA_2005_H[,2]) 
cor.test(overlay_BHI_BNA_2010_H[,1], overlay_BHI_BNA_2010_H[,2]) 
cor.test(overlay_BHI_BNA_2020_H[,1], overlay_BHI_BNA_2020_H[,2]) 

cor.test(overlay_BHI_SWPO_2005_Y[,1], overlay_BHI_SWPO_2005_Y[,2]) 
cor.test(overlay_BHI_SWPO_2010_Y[,1], overlay_BHI_SWPO_2010_Y[,2]) 
cor.test(overlay_BHI_SWPO_2020_Y[,1], overlay_BHI_SWPO_2020_Y[,2]) 

cor.test(overlay_BHI_SWPO_2005_H[,1], overlay_BHI_SWPO_2005_H[,2]) 
cor.test(overlay_BHI_SWPO_2010_H[,1], overlay_BHI_SWPO_2010_H[,2]) 
cor.test(overlay_BHI_SWPO_2020_H[,1], overlay_BHI_SWPO_2020_H[,2]) 

cor.test(overlay_BHI_VEGE_2005_Y[,1], overlay_BHI_VEGE_2005_Y[,2]) 
cor.test(overlay_BHI_VEGE_2010_Y[,1], overlay_BHI_VEGE_2010_Y[,2]) 
cor.test(overlay_BHI_VEGE_2020_Y[,1], overlay_BHI_VEGE_2020_Y[,2]) 

cor.test(overlay_BHI_VEGE_2005_H[,1], overlay_BHI_VEGE_2005_H[,2]) 
cor.test(overlay_BHI_VEGE_2010_H[,1], overlay_BHI_VEGE_2010_H[,2]) 
cor.test(overlay_BHI_VEGE_2020_H[,1], overlay_BHI_VEGE_2020_H[,2]) 

cor.test(overlay_BHI_TROF_2005_Y[,1], overlay_BHI_TROF_2005_Y[,2]) 
cor.test(overlay_BHI_TROF_2010_Y[,1], overlay_BHI_TROF_2010_Y[,2]) 
cor.test(overlay_BHI_TROF_2020_Y[,1], overlay_BHI_TROF_2020_Y[,2]) 

cor.test(overlay_BHI_TROF_2005_H[,1], overlay_BHI_TROF_2005_H[,2]) 
cor.test(overlay_BHI_TROF_2010_H[,1], overlay_BHI_TROF_2010_H[,2]) 
cor.test(overlay_BHI_TROF_2020_H[,1], overlay_BHI_TROF_2020_H[,2]) 

# linear regression (alternative)
linear_BHI_BNA_2005_Y <- lm(overlay_BHI_BNA_2005_Y[,1] ~ overlay_BHI_BNA_2005_Y[,2])
linear_BHI_BNA_2010_Y <- lm(overlay_BHI_BNA_2010_Y[,1] ~ overlay_BHI_BNA_2010_Y[,2])
linear_BHI_BNA_2020_Y <- lm(overlay_BHI_BNA_2020_Y[,1] ~ overlay_BHI_BNA_2020_Y[,2])

summary(linear_BHI_BNA_2005_Y) 
summary(linear_BHI_BNA_2010_Y)
summary(linear_BHI_BNA_2020_Y)

linear_BHI_BNA_2005_H <- lm(overlay_BHI_BNA_2005_H[,1] ~ overlay_BHI_BNA_2005_H[,2])
linear_BHI_BNA_2010_H <- lm(overlay_BHI_BNA_2010_H[,1] ~ overlay_BHI_BNA_2010_H[,2])
linear_BHI_BNA_2020_H <- lm(overlay_BHI_BNA_2020_H[,1] ~ overlay_BHI_BNA_2020_H[,2])

summary(linear_BHI_BNA_2005_H) 
summary(linear_BHI_BNA_2010_H)
summary(linear_BHI_BNA_2020_H)

linear_BHI_SWPO_2005_Y <- lm(overlay_BHI_SWPO_2005_Y[,1] ~ overlay_BHI_SWPO_2005_Y[,2])
linear_BHI_SWPO_2010_Y <- lm(overlay_BHI_SWPO_2010_Y[,1] ~ overlay_BHI_SWPO_2010_Y[,2])
linear_BHI_SWPO_2020_Y <- lm(overlay_BHI_SWPO_2020_Y[,1] ~ overlay_BHI_SWPO_2020_Y[,2])

summary(linear_BHI_SWPO_2005_Y) 
summary(linear_BHI_SWPO_2010_Y)
summary(linear_BHI_SWPO_2020_Y)

linear_BHI_SWPO_2005_H <- lm(overlay_BHI_SWPO_2005_H[,1] ~ overlay_BHI_SWPO_2005_H[,2])
linear_BHI_SWPO_2010_H <- lm(overlay_BHI_SWPO_2010_H[,1] ~ overlay_BHI_SWPO_2010_H[,2])
linear_BHI_SWPO_2020_H <- lm(overlay_BHI_SWPO_2020_H[,1] ~ overlay_BHI_SWPO_2020_H[,2])

summary(linear_BHI_SWPO_2005_H) 
summary(linear_BHI_SWPO_2010_H)
summary(linear_BHI_SWPO_2020_H)

linear_BHI_VEGE_2005_Y <- lm(overlay_BHI_VEGE_2005_Y[,1] ~ overlay_BHI_VEGE_2005_Y[,2])
linear_BHI_VEGE_2010_Y <- lm(overlay_BHI_VEGE_2010_Y[,1] ~ overlay_BHI_VEGE_2010_Y[,2])
linear_BHI_VEGE_2020_Y <- lm(overlay_BHI_VEGE_2020_Y[,1] ~ overlay_BHI_VEGE_2020_Y[,2])

summary(linear_BHI_VEGE_2005_Y) 
summary(linear_BHI_VEGE_2010_Y)
summary(linear_BHI_VEGE_2020_Y)

linear_BHI_VEGE_2005_H <- lm(overlay_BHI_VEGE_2005_H[,1] ~ overlay_BHI_VEGE_2005_H[,2])
linear_BHI_VEGE_2010_H <- lm(overlay_BHI_VEGE_2010_H[,1] ~ overlay_BHI_VEGE_2010_H[,2])
linear_BHI_VEGE_2020_H <- lm(overlay_BHI_VEGE_2020_H[,1] ~ overlay_BHI_VEGE_2020_H[,2])

summary(linear_BHI_VEGE_2005_H) 
summary(linear_BHI_VEGE_2010_H)
summary(linear_BHI_VEGE_2020_H)

linear_BHI_TROF_2005_Y <- lm(overlay_BHI_TROF_2005_Y[,1] ~ overlay_BHI_TROF_2005_Y[,2])
linear_BHI_TROF_2010_Y <- lm(overlay_BHI_TROF_2010_Y[,1] ~ overlay_BHI_TROF_2010_Y[,2])
linear_BHI_TROF_2020_Y <- lm(overlay_BHI_TROF_2020_Y[,1] ~ overlay_BHI_TROF_2020_Y[,2])

summary(linear_BHI_TROF_2005_Y) 
summary(linear_BHI_TROF_2010_Y)
summary(linear_BHI_TROF_2020_Y)

linear_BHI_TROF_2005_H <- lm(overlay_BHI_TROF_2005_H[,1] ~ overlay_BHI_TROF_2005_H[,2])
linear_BHI_TROF_2010_H <- lm(overlay_BHI_TROF_2010_H[,1] ~ overlay_BHI_TROF_2010_H[,2])
linear_BHI_TROF_2020_H <- lm(overlay_BHI_TROF_2020_H[,1] ~ overlay_BHI_TROF_2020_H[,2])

summary(linear_BHI_TROF_2005_H) 
summary(linear_BHI_TROF_2010_H)
summary(linear_BHI_TROF_2020_H)

linear <- lm(overlay[,1] ~ overlay[,2])
summary(linear) 

# Plotting
library(ggpubr)

ggscatter(data = overlay, x = "BHI_2005_m", y = "BNA_2005_Y_m", 
          add = "reg.line") + # Add regressin line
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
           label.x = 0, label.y = 1.1)