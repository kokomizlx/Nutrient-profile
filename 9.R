library(raster)
library(ggplot2)
library(sf)

# 将栅格数据合并为一个栅格对象
Carbohydrate_2010_2 <- stack(Carbohydrate_chickpea_2010, Carbohydrate_cowpea_2010, Carbohydrate_pige_2010)
Carbohydrate_2020 <- stack(Carbohydrate_chickpea_2020, Carbohydrate_cowpea_2020, Carbohydrate_pige_2020)

plot(Carbohydrate_2010)

# 使用ggplot2绘制地图
ggplot(Carbohydrate_2010, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Layer")

print(ggplot(Carbohydrate_2010, aes(x = x, y = y, fill = layer)) +
        geom_raster() +
        theme_void() +
        theme(legend.position = "bottom") +
        labs(fill = "Layer"))

Carbohydrate_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7,
    color = "white", fill = "indianred", size = 0.1
  ) +
  geom_tile(
    data = chickpea_2010,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7,
    color = "white", fill = "chocolate", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7,
    color = "white", fill = "darkblue", size = 0.1
  )

###########################################################################################
legdtxt <- c("Below mean", "At mean", "Above mean")

Protein_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1   # Updated to linewidth
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Protein_g),  
    alpha = 0.7,
    color = "darkseagreen3", fill = "darkseagreen3", linewidth = 0.1  # Updated to linewidth
  ) +
  geom_tile(
    data = chickpea_2010,
    aes(x, y, fill = Protein_g),  
    alpha = 0.7,
    color = "lightsalmon", fill = "lightsalmon", linewidth = 0.1  # Updated to linewidth
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Protein_g),  
    alpha = 0.7,
    color = "lightskyblue2", fill = "lightskyblue2", linewidth = 0.1  # Updated to linewidth
  ) +
  scale_color_manual(name=' Regression Model ',
                     breaks=c("Cowpea", "Chickpea", "Pigeon Pea"),
                     values=c("Cowpea" = "darkseagreen3", "Chickpea" = "lightsalmon", "Pigeon" = "lightskyblue2")
  ) + 
  theme(legend.position = "bottom") 

print(Protein_2010)
###########################################################################################
# 创建一个新的数据框以便于图例
cowpea_2010$Crop <- "Cowpea"
chickpea_2010$Crop <- "Chickpea"
pige_2010$Crop <- "Pigeon Pea"

#新建数据集（用于合并）


# 合并数据集
combined_data <- rbind(
  transform(cowpea_2010, fill_color = "darkseagreen3"),
  transform(chickpea_2010, fill_color = "lightsalmon"),
  transform(pige_2010, fill_color = "lightskyblue1")
)

# 绘制图形
Carbohydrate_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = combined_data,
    aes(x, y, fill = fill_color),  # 使用 fill_color 作为填充颜色
    alpha = 0.7,
    color = "white", linewidth = 0.1
  ) +
  scale_fill_identity(name = "Crop Type", labels = c("Cowpea" = "Cowpea", "Chickpea" = "Chickpea", "Pigeon Pea" = "Pigeon Pea")) +  # 添加图例
  labs(title = "Carbohydrate Content in Different Crops", x = "Longitude", y = "Latitude") +  # 添加标题和坐标轴标签
  theme_minimal()  # 使用简约主题

# 打印图形
print(Carbohydrate_2010)

################################################################################
# 创建图形对象
Protein_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Protein_g),  
    alpha = 0.7,
    color = "darkseagreen3", fill = "darkseagreen3", linewidth = 0.1
  ) +
  geom_tile(
    data = chickpea_2010,
    aes(x, y, fill = Protein_g),  
    alpha = 0.7,
    color = "lightsalmon", fill = "lightsalmon", linewidth = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Protein_g),  
    alpha = 0.7,
    color = "lightskyblue2", fill = "lightskyblue2", linewidth = 0.1
  ) +
  scale_fill_manual(values = c("darkseagreen3", "lightsalmon", "lightskyblue2"),
                    labels = c("Cowpea", "Chickpea", "Pige"), 
                    name = "Crop Type")

# 添加标题
Protein_2010 <- Protein_2010 + labs(title = "Protein Content 2010", x = "Longitude", y = "Latitude")

# 设置保存路径和文件名
save_path <- "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results_intergrated map/"
file_name <- "protein_2010.png"

# 保存图像
ggsave(filename = paste0(save_path, file_name), plot = Protein_2010, width = 12, height = 6)

# 显示图像
print(Protein_2010)