#install.packages("readxl")
#install.packages("sf")
#install.packages("httr")
#install.packages("geojsonio")
#install.packages("dplyr")
#install.packages("elevatr")
#install.packages("raster")
#install.packages("gstat")
library(gstat)
#install.packages("sp")
#install.packages("gdistance")
#install.packages("geodata")
library(geodata)
library(sp)
library(gdistance)
library(raster)
library(elevatr)
library(dplyr)
library(geojsonio)
library(httr)
library(sf)
library(readxl)


geo_school <- st_read("C:/Users/39792/Downloads/dv372_DataVic_School_Zones_2025/Primary_Integrated_2025.geojson")
geo_school_points <- st_centroid(geo_school)

# 获取地形数据（高程）
school_locations <- st_coordinates(geo_school_points)
school_locations <- data.frame(school_locations)
colnames(school_locations) <- c("x", "y")

elevation_victoria <- get_elev_point(school_locations, prj = st_crs(geo_school_points)$proj4string, src = "aws")
bbox <- st_bbox(elevation_victoria)
res <- 0.01  # 栅格分辨率，可以根据需要调整
r <- raster(xmn = bbox["xmin"], xmx = bbox["xmax"], ymn = bbox["ymin"], ymx = bbox["ymax"], res = res, crs = st_crs(elevation_victoria)$proj4string)
idw_model <- gstat(formula = elevation ~ 1, data = elevation_victoria)
dem_raster <- interpolate(r, idw_model)

slope_raster <- terrain(dem_raster, opt = 'slope', unit = 'degrees')
plot(slope_raster, main = "Slope")

# 计算坡向（单位：度）
aspect_raster <- terrain(dem_raster, opt = "aspect")
plot(aspect_raster, main = "Aspect")


srtm <- getData("SRTM", lon = mean(school_locations$x), lat = mean(school_locations$y))

elevation_raster <- extract(srtm, school_locations)
elevation_raster <- data.frame(elevation_raster)
geo_school_points$elevation <- elevation_raster$elevation_raster

elev_raster <- rasterize(geo_school_points, srtm, field = "elevation")
elev_raster <- raster(elev_raster)


# 计算坡向
aspect_raster <- terrain(elevation_raster, opt = "aspect")
plot(aspect_raster, main = "Aspect")

# 计算地形起伏
terrain_roughness_raster <- focal(elevation_raster, w = matrix(1, 3, 3), fun = sd, na.rm = TRUE)
plot(terrain_roughness_raster, main = "Terrain Roughness")

install.packages("spatialreg")
library(spatialreg)

# 添加提取的数据到学校数据
geo_school_points$slope <- slope_values
geo_school_points$aspect <- aspect_values

geo_school_points$elevation <- runif(nrow(geo_school_points))
geo_school_points$slope <- runif(nrow(geo_school_points))
geo_school_points$aspect <- runif(nrow(geo_school_points))

coords <- st_coordinates(geo_school_points)

coords_matrix <- as.matrix(coords)
library(spdep)
dists <- spDists(coords_matrix, longlat = TRUE)
w <- 1 / dists
diag(w) <- 0  # 不包括自邻接
w <- mat2listw(w)

# 空间滞后模型
sar_model <- spautolm(elevation ~ slope + aspect, data = geo_school_points, listw = w_list, type = "lag")

sar_model <- lagsarlm(elevation ~ slope + aspect, data = geo_school_points, listw = w_list)

library(cluster)

# 提取特征数据
features <- cbind(geo_school_points$slope, geo_school_points$aspect)


# 执行 K 均值聚类
kmeans_result <- kmeans(features, centers = 3)  # 假设 3 个簇
geo_school_points$cluster <- kmeans_result$cluster

# 可视化聚类结果
plot(coords[, 1], coords[, 2], col = geo_school_points$cluster, pch = 19,
     xlab = "Longitude", ylab = "Latitude", main = "Clustering of Schools")

# Optionally, add a legend to the plot if you have a known number of clusters
legend("topright", legend = unique(geo_school_points$cluster), col = unique(geo_school_points$cluster), pch = 19)



install.packages("rnaturalearthdata")

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Step 2: Load country boundaries or other internal maps
# For example, load the map of Australia
states <- ne_states(country = "Australia", returnclass = "sf")

# Filter for Victoria only
victoria_map <- subset(states, name == "Victoria")

geo_school_points_victoria <- geo_school_points[st_intersects(geo_school_points, victoria_map, sparse = FALSE), ]

geo_school_points$longitude <- coords[, 1]
geo_school_points$latitude <- coords[, 2]

ggplot() +
  geom_sf(data = victoria_map, fill = "lightgray", color = "darkgray") +  # Plot Victoria map
  geom_point(data = geo_school_points_victoria, aes(x = longitude, y = latitude, color = as.factor(cluster)), size = 3) +  # Plot schools
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize cluster colors
  labs(title = "School Distribution with Clusters in Victoria", color = "Cluster") +
  theme_minimal()

install.packages("devtools")
devtools::install_github("ropensci/rnaturalearthhires")
geo_school_points_victoria <- geo_school_points[st_intersects(geo_school_points, victoria_map, sparse = FALSE), ]

# Extract coordinates from geometry for plotting
coords <- st_coordinates(geo_school_points_victoria)
geo_school_points_victoria$longitude <- coords[, 1]
geo_school_points_victoria$latitude <- coords[, 2]

# Step 3: Plot the map of Victoria and the clustered schools
ggplot() +
  geom_sf(data = victoria_map, fill = "lightgray", color = "darkgray") +  # Plot Victoria map
  geom_point(data = geo_school_points_victoria, aes(x = longitude, y = latitude, color = as.factor(cluster)), size = 0.8) +  # Plot schools
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize cluster colors
  labs(title = "School Distribution with Clusters in Victoria", color = "Cluster") +
  theme_minimal()



