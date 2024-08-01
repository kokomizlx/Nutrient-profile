library(ggplot2)
library(viridis)
world <- map_data("world") # 获取世界地图坐标

# Soybean
# 创建地图数据框
map_data <- data.frame(
  Water = soybean_2000$Water_g,
  lat = soybean_2000$y,
  long = soybean_2000$x
)

plot(soybean_2000)

# 绘制世界地图
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  )

# 在世界地图上叠加数据
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_raster(
    data = soybean_2000,
    aes(x, y, fill = soyb),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_viridis_c()  # Add a color scale

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile( # 坐标校正
    data = soybean_2000,
    aes(x, y, fill = soyb),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_viridis_c()  # Add a color scale

#  多层Raster叠加
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(data = soybean_2000, aes(x, y, fill = soyb), alpha = 0.7) +
  geom_tile(data = soybean_2010, aes(x, y, fill = soyb_a), alpha = 0.7) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "bottom")