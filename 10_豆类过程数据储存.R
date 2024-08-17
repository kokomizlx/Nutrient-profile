install.packages("writexl")
library(writexl)
save_path <- "D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/2024-06-26 营养统计/Results_intergrated map/"

write_xlsx(pige_2010, paste0(save_path, "pige_2010.xlsx")) # 导出数据框为Excel文件
write_xlsx(pige_2020, paste0(save_path, "pige_2020.xlsx"))

write_xlsx(bean_2000, paste0(save_path, "bean_2000.xlsx"))
write_xlsx(bean_2005, paste0(save_path, "bean_2005.xlsx"))
write_xlsx(bean_2010, paste0(save_path, "bean_2010.xlsx"))
write_xlsx(bean_2020, paste0(save_path, "bean_2020.xlsx"))

write_xlsx(chickpea_2010, paste0(save_path, "chickpea_2010.xlsx")) 
write_xlsx(chickpea_2020, paste0(save_path, "chickpea_2020.xlsx"))

write_xlsx(cowpea_2010, paste0(save_path, "cowpea_2010.xlsx")) 
write_xlsx(cowpea_2020, paste0(save_path, "cowpea_2020.xlsx"))

write_xlsx(lentil_2010, paste0(save_path, "lentil_2010.xlsx")) 
write_xlsx(lentil_2020, paste0(save_path, "lentil_2020.xlsx"))

write_xlsx(soybean_2000, paste0(save_path, "soybean_2000.xlsx"))
write_xlsx(soybean_2010, paste0(save_path, "soybean_2010.xlsx"))
write_xlsx(soybean_2020, paste0(save_path, "soybean_2020.xlsx"))
