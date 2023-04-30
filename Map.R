




# Dialect Atlas -----------------------------------------------------------

# IMPORT DATA

library(sf)
china1 <- read_sf("~/CP2022/china/CN_city_2020/CN_city_2020.shp", as_tibble = FALSE)
china2 <- read_sf("~/CP2022/china/CN_county_2020/CN_county_2020.shp", as_tibble = FALSE)

china3 <- read_sf("~/CP2022/china/ChinaProvince/ChinaProvince.shp", as_tibble = FALSE)

library(plyr)
cn1 <- merge(area_1000, china1, by.data = "AreaCode", by.china1 = "Code", All = FALSE)  #data.......
cn2 <- merge(area_1000, china2, by.data = "AreaCode", by.china2 = "Code", All = FALSE)
cn <- rbind.fill(cn1, cn2)
cn$Type = as.factor(cn$Type)

# PLOT
library(ggplot2)
ggplot() + 
  geom_sf(data = cn, aes(geometry = geometry, color = Type)) + 
  geom_sf(data = china3, fill = NA)
  #scale_color_manual(values = c("#000000", "#FF0000", "#FF0000", "#FF0000"))  # 自定义颜色
