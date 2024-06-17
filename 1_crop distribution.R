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
setwd("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/1_r")

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
SWPO_2005 <- raster("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Sweet potato/SPAM_2005_global_H_TA_SWPO_A.tif") #读入栅格数据
SWPO_2010 <- raster("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Sweet potato/SPAM_2010_global_H_SWPO_A.tif") #读入栅格数据
SWPO_2020 <- raster("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Sweet potato/SPAM_2020_global_H_SWPO_A.tif") #读入栅格数据

BNA_2005_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Banana/SPAM_2005_global_Y_BANA_A.tif")
BNA_2005_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Banana/SPAM_2005_global_H_BANA_A.tif")
BNA_2010_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Banana/SPAM_2010_global_Y_BANA_A.tif")
BNA_2010_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Banana/SPAM_2010_global_H_BANA_A.tif")
BNA_2020_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Banana/SPAM_2020_global_Y_BANA_A.tif")
BNA_2020_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Banana/SPAM_2020_global_H_BANA_A.tif")

VEGE_2005_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other vegetables/SPAM_2005_global_Y_VEGE_A.tif")
VEGE_2005_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other vegetables/SPAM_2005_global_H_VEGE_A.tif")
VEGE_2010_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other vegetables/SPAM_2010_global_Y_VEGE_A.tif")
VEGE_2010_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other vegetables/SPAM_2010_global_H_VEGE_A.tif")
VEGE_2020_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other vegetables/SPAM_2020_global_Y_VEGE_A.tif")
VEGE_2020_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other vegetables/SPAM_2020_global_H_VEGE_A.tif")

TROF_2005_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other tropical fruits/SPAM_2005_global_Y_TROF_A.tif")
TROF_2005_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other tropical fruits/SPAM_2005_global_H_TROF_A.tif")
TROF_2010_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other tropical fruits/SPAM_2010_global_Y_TROF_A.tif")
TROF_2010_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other tropical fruits/SPAM_2010_global_H_TROF_A.tif")
TROF_2020_Y <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other tropical fruits/SPAM_2020_global_Y_TROF_A.tif")
TROF_2020_H <- raster("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrient profile/2_tiff/Other tropical fruits/SPAM_2020_global_H_TROF_A.tif")

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