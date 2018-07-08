# load up area shape file:
if (!require("maptools")) { install.packages("maptools"); require("maptools") }
if (!require("RQGIS")) { install.packages("RQGIS"); require("RQGIS") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }



dir = "C:/Users/stepa/OneDrive/DataScience/Rostov Transport/Shapes/GIS"
plotDir = "/Plots/"
shpDir = "C:/Users/stepa/OneDrive/DataScience/Rostov Transport/Shapes/GIS"



# attach RQGIS ----
#devtools::install_github("jannes-m/RQGIS")
library("RQGIS")

#set_env(dev = FALSE)
#open_app()



# set the environment, i.e. specify all the paths necessary to run QGIS from 
# within R
set_env("C:/QGIS28")

#check_apps(root = "C:/QGIS28")


#get_args_man()

#
find_algorithms(search_term = "([Gg]rid)")
alg = "qgis:creategrid"

params <- get_args_man(alg)
params

params$TYPE <- 3
params$EXTENT <- "39.4047313, 39.8524471, 47.1523996, 47.3687328"
params$HSPACING <- 0.01 # (params$EXTENT[[2]] - params$EXTENT[[1]])/100
params$VSPACING <- 0.01 # (params$EXTENT[[4]] - params$EXTENT[[3]])/100
params$CRS = "EPSG:32637"
params$OUTPUT <- file.path( shpDir, "/Open Rostov shp/Rostov_grid.shp")

out <- run_qgis(alg = alg,
                params = params,
                load_output = TRUE)



# Intersections --
#find_algorithms(search_term = "[Ii]ntersect")
alg = "qgis:intersection"
#open_help(alg)
#get_usage(alg)
#get_usage(alg = "qgis:intersection")

params <- get_args_man(alg)
params

params$INPUT <- file.path(shpDir, "/Open Rostov shp/Rostov_grid.shp")
params$INPUT2 <- file.path(shpDir, "/Open Rostov shp/Rostov-on-Don.shp")
params$OUTPUT <- file.path(shpDir, "/Open Rostov shp/Rostov_busstops_count_in_grid_cutted.shp")

out <- run_qgis(alg = alg,
                params = params,
                load_output = TRUE)

params$INPUT <- file.path(shpDir, "/Open Rostov shp/gis.osm_roads_free_1.shp")
params$INPUT2 <- file.path(shpDir, "/Open Rostov shp/Rostov-on-Don.shp")
params$OUTPUT <- file.path(shpDir, "/Open Rostov shp/Rostov_roads_cutted.shp")

out <- run_qgis(alg = alg,
                params = params,
                load_output = TRUE)
#
find_algorithms(search_term = "([Cc]ount)([Pp]oints)")
alg = "qgis:countpointsinpolygon"

params <- get_args_man(alg)
params

params$POLYGONS <- file.path(shpDir, "/Open Rostov shp/Rostov_busstops_count_in_grid_cutted.shp")
params$POINTS <- file.path(shpDir, "/Rostov BusStops HeatMap/RostovStops.shp")
params$OUTPUT <- file.path(shpDir, "/Open Rostov shp/Rostov_busstops_count_in_grid_cutted.shp")

out <- run_qgis(alg = alg,
                params = params,
                load_output = TRUE)









# Raster package ----
library("raster")
library("rgdal")

# download German administrative areas 
# ger is of class "SpatialPolygonsDataFrame"
rus <- getData(name = "GADM", country = "RUS", level = 0)

# first, plot the federal states of Germany
plot(rus)
# next plot the centroids created by QGIS
plot(out$geometry, pch = 21, add = TRUE, bg = "lightblue", col = "black")

qplot(ger)




# Ploting with ggmap ----

#area <- readShapePoly("ne_10m_parks_and_protected_lands/ne_10m_parks_and_protected_lands_area.shp")
area <- readShapePoly("Open Rostov shp/intersect.shp")
area <- rgdal::readOGR("Open Rostov shp/intersect.shp")
area <- rgdal::readOGR("Open Rostov shp/Rostov_grid.shp")
area <- rgdal::readOGR("Temp shp dir/Rostov_busstops_count_in_grid_cutted.shp")
areaRND <- rgdal::readOGR("Open Rostov shp/Rostov-on-Don.shp")
areaStops <- rgdal::readOGR("Rostov BusStops HeatMap/RostovStops.shp")

# admin_level_6a
# admin_level_8c
# admin_level_9
# block_stats
# Buildings Oblast
# Buildings_Core_Isochrones
# Buildings_Core
# Estado
# gis.osm_natural_a_free_1
# gis.osm_natural_free_1
# gis.osm_places_free_1
# gis.osm_pofw_a_free_1
# gis.osm_pofw_free_1
# gis.osm_traffic_free_1
# gis.osm_transport_a_free_1
# gis.osm_transport_free_1
# isochrones
# horeca



# # or file.choose:

# area <- readShapePoly(file.choose())
library(RColorBrewer)
colors <- brewer.pal(9, "YlOrRd")

#summary( area@data$NUMPOINTS )

area.points <- fortify(area)

poly_numpoint = as.integer( area@data$NUMPOINTS )

corresp = as.integer( area.points@.Data[[6]] )
for (i in c(1:length(poly_numpoint))) { 
    corresp[corresp == i - 1] = poly_numpoint[i]
}
#corresp[corresp == 1] = NA

summary( as.integer( area.points@.Data[[5]]@.Data ) )

plot(
    area,
    col = "black",
    fill = "white",
    xlim = c(min(area.points$lon), max(area.points$lon)),
    ylim = c(min(area.points$lat), max(area.points$lat)))

par( mar= c(0,0,0,0))
plot(
    area[area@data$fclass %in% c("primary", "secondary", "footway"),],
    col = colors[as.integer(area@data$fclass)[area@data$fclass %in% c("primary", "secondary", "footway")]],
    xlim = c(min(area.points$lon), max(area.points$lon)),
    ylim = c(min(area.points$lat), max(area.points$lat)))





if (!require("ggmap")) { install.packages("ggmap"); require("ggmap") }
if (!require("geosphere")) { install.packages("geosphere"); require("geosphere") }
if (!require("jpeg")) { install.packages("jpeg"); require("jpeg") }
if (!require("mapproj")) { install.packages("mapproj"); require("mapproj") }
if (!require("maps")) { install.packages("maps"); require("maps") }
if (!require("rjson")) { install.packages("rjson"); require("rjson") }
if (!require("jpeg")) { install.packages("jpeg"); require("jpeg") }
library(ggmap)


RostovCoord = geocode("Rostov-on-Don", override_limit = T)

mapImage <- get_map(location = RostovCoord,
  color = "color",
  source = "google",
  # maptype = "terrain",
  zoom = 11)

#area.points <- head( fortify(area), 1000 )

#str( as.integer( area.points$group ) )

#area.points <- area#[[1:100]]

#png(filename = file.path(dir, plotDir, "Rostov_area_hexo.png"), width = 1280, height = 1280, units = "px", pointsize = 24, bg = "white", res = 100, family = "", restoreConsole = TRUE) #, type = c("cairo-png"))

ggmap(mapImage) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = area.points, color = "gray", fill = "red", alpha = corresp / max(corresp)) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = fortify(areaRoads[areaRoads@data$fclass %in% c("primary", "secondary"),]), color = "steelblue", fill = "white", alpha = 0.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = fortify(areaStops), color = "steelblue", fill = "white", alpha = 0.85) +
    labs(x = "Longitude", y = "Latitude") + xlab("") + ylab("") +
    theme(plot.margin = unit(c(0, 0.0, -0.5, 0), "cm")) # + ggtitle("Плотность Остановок ОТ")



df = as.data.frame(area)
coords = area@polygons
df$population

area$

ggmap(mapImage) +
    geom_polygon(
        aes(x = long, y = lat),
        data = head(area, 1000), color = "cyan4", alpha = 0.5) +
        labs(x = "Longitude", y = "Latitude") + xlab("") + ylab("")

plot(area@polygons, usePolypath = TRUE)


#dev.off()






# A simple model to predict the location of the R in the R-logo using 20 presence points ----
# and 50 (random) pseudo-absence points. This type of model is often used to predict
# species distributions. See the dismo package for more of that.

# create a RasterStack or RasterBrick with with a set of predictor layers
logo <- brick(system.file("external/rlogo.grd", package = "raster"))
names(logo)

## Not run: 
# the predictor variables
par(mfrow = c(2, 2))
plotRGB(logo, main = 'logo')
plot(logo, 1, col = rgb(cbind(0:255, 0, 0), maxColorValue = 255))
plot(logo, 2, col = rgb(cbind(0, 0:255, 0), maxColorValue = 255))
plot(logo, 3, col = rgb(cbind(0, 0, 0:255), maxColorValue = 255))
par(mfrow = c(1, 1))

## End(Not run)

# known presence and absence points
p <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85,
   66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31,
   22, 34, 60, 70, 73, 63, 46, 43, 28), ncol = 2)

a <- matrix(c(22, 33, 64, 85, 92, 94, 59, 27, 30, 64, 60, 33, 31, 9,
   99, 67, 15, 5, 4, 30, 8, 37, 42, 27, 19, 69, 60, 73, 3, 5, 21,
   37, 52, 70, 74, 9, 13, 4, 17, 47), ncol = 2)

# extract values for points
xy <- rbind(cbind(1, p), cbind(0, a))
v <- data.frame(cbind(pa = xy[, 1], extract(logo, xy[, 2:3])))

#build a model, here an example with glm 
model <- glm(formula = pa ~ ., data = v)

#predict to a raster
r1 <- predict(logo, model, progress = 'text')

plot(r1)
points(p, bg = 'blue', pch = 21)
points(a, bg = 'red', pch = 21)

# use a modified function to get a RasterBrick with p and se
# from the glm model. The values returned by 'predict' are in a list,
# and this list needs to be transformed to a matrix

predfun <- function(model, data) {
    v <- predict(model, data, se.fit = TRUE)
    cbind(p = as.vector(v$fit), se = as.vector(v$se.fit))
}

# predfun returns two variables, so use index=1:2
r2 <- predict(logo, model, fun = predfun, index = 1:2)


## Not run: 
# You can use multiple cores to speed up the predict function
# by calling it via the clusterR function (you may need to install the snow package)
beginCluster()
r1c <- clusterR(logo, predict, args = list(model))
r2c <- clusterR(logo, predict, args = list(model = model, fun = predfun, index = 1:2))

## End(Not run)

# principal components of a RasterBrick
# here using sampling to simulate an object too large
# too feed all its values to prcomp
sr <- sampleRandom(logo, 100)
pca <- prcomp(sr)

# note the use of the 'index' argument
x <- predict(logo, pca, index = 1:3)
plot(x)

## Not run: 
# partial least square regression
library(pls)
model <- plsr(formula = pa ~ ., data = v)
# this returns an array:
predict(model, v[1:5,])
# write a function to turn that into a matrix
pfun <- function(x, data) {
    y <- predict(x, data)
    d <- dim(y)
    dim(y) <- c(prod(d[1:2]), d[3])
    y
}

pp <- predict(logo, model, fun = pfun, index = 1:3)

# Random Forest

library(randomForest)
rfmod <- randomForest(pa ~ ., data = v)

## note the additional argument "type='response'" that is 
## passed to predict.randomForest
r3 <- predict(logo, rfmod, type = 'response', progress = 'window')

## get a RasterBrick with class membership probabilities
vv <- v
vv$pa <- as.factor(vv$pa)
rfmod2 <- randomForest(pa ~ ., data = vv)
r4 <- predict(logo, rfmod2, type = 'prob', index = 1:2)
spplot(r4)


# cforest (other Random Forest implementation) example with factors argument

v$red <- as.factor(round(v$red / 100))
logo$red <- round(logo[[1]] / 100)

library(party)
m <- cforest(pa ~ ., control = cforest_unbiased(mtry = 3), data = v)
f <- list(levels(v$red))
names(f) <- 'red'
pc <- predict(logo, m, OOB = TRUE, factors = f)


# knn example, using calc instead of predict
library(class)
cl <- factor(c(rep(1, nrow(p)), rep(0, nrow(a))))
train <- extract(logo, rbind(p, a))
k <- calc(logo, function(x) as.integer(as.character(knn(train, x, cl))))



