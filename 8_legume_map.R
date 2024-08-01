# R配色网址：https://blog.csdn.net/bone_ace/article/details/47362619

save_path <- "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results_map/"

# 20 Soybean
# Water
library(ggplot2)
Water_Soybean_2000 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = soybean_2000,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue")  # Add a color scale with log10 transformation

Water_Soybean_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = soybean_2010,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Soybean_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = soybean_2020,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

# 添加标题
Water_Soybean_2000 <- Water_Soybean_2000 + labs(title = "Water Soybean 2000", x = "Longitude", y = "Latitude")
Water_Soybean_2010 <- Water_Soybean_2010 + labs(title = "Water Soybean 2010", x = "Longitude", y = "Latitude")
Water_Soybean_2020 <- Water_Soybean_2020 + labs(title = "Water Soybean 2020", x = "Longitude", y = "Latitude")


ggsave(filename = paste0(save_path, "Water_Soybean_2000.png"), plot = Water_Soybean_2000, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Water_Soybean_2010.png"), plot = Water_Soybean_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Water_Soybean_2020.png"), plot = Water_Soybean_2020, width = 8, height = 4.5, limitsize = FALSE)

dev.off()

# Fatty acids
Fattyacids_Soybean_2000 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = soybean_2000,
    aes(x, y, fill = fatty_acids_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "antiquewhite", high = "antiquewhite4")  # Add a color scale with log10 transformation

Fattyacids_Soybean_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = soybean_2010,
    aes(x, y, fill = fatty_acids_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "antiquewhite", high = "antiquewhite4") 

############################################################################################
# 17 Pigeon Pea
# Energy
Energy_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Energy_kJ),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "chocolate", high = "chocolate4") 

Energy_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Energy_kJ),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "chocolate", high = "chocolate4") 

Energy_Pige_2010 <- Energy_Pige_2010 + labs(title = "Energy Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Energy_Pige_2020 <- Energy_Pige_2020 + labs(title = "Energy Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Energy_Pige_2010.png"), plot = Energy_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Energy_Pige_2020.png"), plot = Energy_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

# Water
Water_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Pige_2010 <- Water_Pige_2010 + labs(title = "Water Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Water_Pige_2020 <- Water_Pige_2020 + labs(title = "Water Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Water_Pige_2010.png"), plot = Water_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Water_Pige_2020.png"), plot = Water_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

# Protein
Protein_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4") 

Protein_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4") 

Protein_Pige_2010 <- Protein_Pige_2010 + labs(title = "Protein Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Protein_Pige_2020 <- Protein_Pige_2020 + labs(title = "Protein Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Protein_Pige_2010.png"), plot = Protein_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Protein_Pige_2020.png"), plot = Protein_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

# Fat
Fat_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Fat_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "burlywood", high = "burlywood4") 

Fat_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Fat_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "burlywood", high = "burlywood4") 

Fat_Pige_2010 <- Fat_Pige_2010 + labs(title = "Fat Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Fat_Pige_2020 <- Fat_Pige_2020 + labs(title = "Fat Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Fat_Pige_2010.png"), plot = Fat_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Fat_Pige_2020.png"), plot = Fat_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

# Carbohydrate
Carbohydrate_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Carbohydrate_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "aquamarine", high = "aquamarine4") 

Carbohydrate_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Carbohydrate_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "aquamarine", high = "aquamarine4")  

Carbohydrate_Pige_2010 <- Carbohydrate_Pige_2010 + labs(title = "Carbohydrate Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Carbohydrate_Pige_2020 <- Carbohydrate_Pige_2020 + labs(title = "Carbohydrate Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Carbohydrate_Pige_2010.png"), plot = Carbohydrate_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Carbohydrate_Pige_2020.png"), plot = Carbohydrate_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

# Fibre
Fibre_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Fibre_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "darkolivegreen1", high = "darkolivegreen4") 

Fibre_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Fibre_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "darkolivegreen1", high = "darkolivegreen4")  

Fibre_Pige_2010 <- Fibre_Pige_2010 + labs(title = "Fibre Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Fibre_Pige_2020 <- Fibre_Pige_2020 + labs(title = "Fibre Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Fibre_Pige_2010.png"), plot = Fibre_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Fibre_Pige_2020.png"), plot = Fibre_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

# Vitamin
Vitamin_Pige_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2010,
    aes(x, y, fill = Vitamin_mg),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "thistle1", high = "thistle4") 

Vitamin_Pige_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Vitamin_mg),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "thistle1", high = "thistle4") 

Vitamin_Pige_2010 <- Vitamin_Pige_2010 + labs(title = "Vitamin Pigeon Pea 2010", x = "Longitude", y = "Latitude")
Vitamin_Pige_2020 <- Vitamin_Pige_2020 + labs(title = "Vitamin Pigeon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Vitamin_Pige_2010.png"), plot = Vitamin_Pige_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Vitamin_Pige_2020.png"), plot = Vitamin_Pige_2020, width = 8, height = 4.5, limitsize = FALSE)

############################################################################################
# 18 Lentil
# Water
Water_Lentil_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = lentil_2010,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Lentil_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = lentil_2020,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Lentil_2010 <- Water_Lentil_2010 + labs(title = "Water Lentil 2010", x = "Longitude", y = "Latitude")
Water_Lentil_2020 <- Water_Lentil_2020 + labs(title = "Water Lentil 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Water_Lentil_2010.png"), plot = Water_Lentil_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Water_Lentil_2020.png"), plot = Water_Lentil_2020, width = 8, height = 4.5, limitsize = FALSE)

# Protein
Protein_Lentil_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = lentil_2010,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4") 

Protein_Lentil_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = lentil_2020,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4") 

Protein_Lentil_2010 <- Protein_Lentil_2010 + labs(title = "Protein Lentil 2010", x = "Longitude", y = "Latitude")
Protein_Lentil_2020 <- Protein_Lentil_2020 + labs(title = "Protein Lentil 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Protein_Lentil_2010.png"), plot = Protein_Lentil_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Protein_Lentil_2020.png"), plot = Protein_Lentil_2020, width = 8, height = 4.5, limitsize = FALSE)

############################################################################################
# 16 Cowpea
Energy_Cowpea_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Energy_kJ),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "chocolate", high = "chocolate4") 

Energy_Cowpea_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2020,
    aes(x, y, fill = Energy_kJ),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "chocolate", high = "chocolate4") 

Energy_Cowpea_2010 <- Energy_Cowpea_2010 + labs(title = "Energy Cowpeaon Pea 2010", x = "Longitude", y = "Latitude")
Energy_Cowpea_2020 <- Energy_Cowpea_2020 + labs(title = "Energy Cowpeaon Pea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Energy_Cowpea_2010.png"), plot = Energy_Cowpea_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Energy_Cowpea_2020.png"), plot = Energy_Cowpea_2020, width = 8, height = 4.5, limitsize = FALSE)

# Water
Water_Cowpea_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Cowpea_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2020,
    aes(x, y, fill = Water_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightblue", high = "darkblue") 

Water_Cowpea_2010 <- Water_Cowpea_2010 + labs(title = "Water Cowpea 2010", x = "Longitude", y = "Latitude")
Water_Cowpea_2020 <- Water_Cowpea_2020 + labs(title = "Water Cowpea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Water_Cowpea_2010.png"), plot = Water_Cowpea_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Water_Cowpea_2020.png"), plot = Water_Cowpea_2020, width = 8, height = 4.5, limitsize = FALSE)

# Protein
Protein_Cowpea_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4") 

Protein_Cowpea_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2020,
    aes(x, y, fill = Protein_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4") 

Protein_Cowpea_2010 <- Protein_Cowpea_2010 + labs(title = "Protein Cowpea 2010", x = "Longitude", y = "Latitude")
Protein_Cowpea_2020 <- Protein_Cowpea_2020 + labs(title = "Protein Cowpea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Protein_Cowpea_2010.png"), plot = Protein_Cowpea_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Protein_Cowpea_2020.png"), plot = Protein_Cowpea_2020, width = 8, height = 4.5, limitsize = FALSE)

# Fat
Fat_Cowpea_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Fat_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "burlywood1", high = "burlywood4") 

Fat_Cowpea_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2020,
    aes(x, y, fill = Fat_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "burlywood1", high = "burlywood4") 

Fat_Cowpea_2010 <- Fat_Cowpea_2010 + labs(title = "Fat Cowpea 2010", x = "Longitude", y = "Latitude")
Fat_Cowpea_2020 <- Fat_Cowpea_2020 + labs(title = "Fat Cowpea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Fat_Cowpea_2010.png"), plot = Fat_Cowpea_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Fat_Cowpea_2020.png"), plot = Fat_Cowpea_2020, width = 8, height = 4.5, limitsize = FALSE)

# Carbohydrate
Carbohydrate_Cowpea_2010 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2010,
    aes(x, y, fill = Carbohydrate_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "aquamarine", high = "aquamarine4") 

Carbohydrate_Cowpea_2020 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2020,
    aes(x, y, fill = Carbohydrate_g),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "aquamarine", high = "aquamarine4") 

Carbohydrate_Cowpea_2010 <- Carbohydrate_Cowpea_2010 + labs(title = "Carbohydrate Cowpea 2010", x = "Longitude", y = "Latitude")
Carbohydrate_Cowpea_2020 <- Carbohydrate_Cowpea_2020 + labs(title = "Carbohydrate Cowpea 2020", x = "Longitude", y = "Latitude")

ggsave(filename = paste0(save_path, "Carbohydrate_Cowpea_2010.png"), plot = Carbohydrate_Cowpea_2010, width = 8, height = 4.5, limitsize = FALSE)
ggsave(filename = paste0(save_path, "Carbohydrate_Cowpea_2020.png"), plot = Carbohydrate_Cowpea_2020, width = 8, height = 4.5, limitsize = FALSE)

# Fibre
