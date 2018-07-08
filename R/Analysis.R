Sys.setenv(LANG = "en")
if (!require("RgoogleMaps")) { install.packages("RgoogleMaps"); require("RgoogleMaps") }
if (!require("openxlsx")) { install.packages("openxlsx"); require("openxlsx") }
if (!require("loa")) { install.packages("loa"); require("loa") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }
library(stringr)
library(gridExtra)

if (!require("extrafont")) { install.packages("extrafont"); require("extrafont") }
loadfonts()

dataDir = "C:/Users/stepa/OneDrive/DataScience/Rostov Transport/RGM/Data"
plotDir = "C:/Users/stepa/OneDrive/DataScience/Rostov Transport/RGM/Images"

df = read.xlsx(file.path(dataDir, "Bus stops.xlsx"), sheet = 1, startRow = 1)

colnames(df)[c(1:7)] = c("name", "bus", "tram", "trolley", "minibus", "lat", "lon")

df$lat = as.numeric(df$lat)
df$lon = as.numeric(df$lon)




# Number of routes ----
df = cbind( df, str_count(df$bus, ",")     + 1 )
df = cbind( df, str_count(df$tram, ",")    + 1 )
df = cbind( df, str_count(df$trolley, ",") + 1 )
df = cbind( df, str_count(df$minibus, ",") + 1 )

colnames(df)[c(10:13)] = c("n_bus", "n_tram", "n_trolley", "n_minibus")

df$n_bus[is.na(df$n_bus)] = 0
df$n_tram[is.na(df$n_tram)] = 0
df$n_trolley[is.na(df$n_trolley)] = 0
df$n_minibus[is.na(df$n_minibus)] = 0

df = cbind( df, df$n_bus + df$n_tram + df$n_trolley + df$n_minibus )
colnames(df)[14] = "n_all"



# Vehicle numbers lists ----
library(gtools)
bus_numbers = unique( unlist( strsplit( df$bus, ", ") ) ) 
bus_numbers = mixedsort( bus_numbers )[-c(length(bus_numbers))]

tram_numbers = unique(unlist(strsplit(df$tram, ", ")))
tram_numbers = mixedsort(tram_numbers)[-c(length(tram_numbers))]

trolley_numbers = unique(unlist(strsplit(df$trolley, ", ")))
trolley_numbers = mixedsort(trolley_numbers)[-c(length(trolley_numbers))]

minibus_numbers = unique(unlist(strsplit(df$minibus, ", ")))
minibus_numbers = mixedsort(minibus_numbers)[-c(length(minibus_numbers))]

bus_numbers_circled = c("5","10","11","18л","18п","22","27","28","35","35а","37","39","40","42","42а",
"45","47","49","51","54","58","60","61","68","69","69а", "78","83","89","90","90а", "94","96","99")
minibus_numbers_circled = c("12", "20", "23", "24", "25", "38", "40", "49", "93", "94", "96")
trolley_numbers_circled = c("2", "12")
tram_numbers_circled = NULL




# Routes and stpos distances data.frame ----

#if (!require("geosphere")) { install.packages("geosphere"); require("geosphere") }
#if (!require("rgeos")) { install.packages("rgeos"); require("rgeos") }

Route = function(df4, stop_n) {
    i = dim(df4)[1]
    df4 = cbind(df4, 1:dim(df4)[1])
    tmp = 1
    k = stop_n
    df_tmp = df4[k,]
    while (i >= 0) {
        if (length(k) != dim(df4)[1]) {
            set1sp <- SpatialPoints(df4[c(k), c('lon', 'lat')])
            set2sp <- SpatialPoints(df4[-c(k), c('lon', 'lat')])
            df_tmp2 = df4[-c(k),]

            kk = which(gDistance(set1sp, set2sp, byid = TRUE) == min(gDistance(set1sp, set2sp, byid = TRUE)), arr.ind = TRUE)[1, 1]

            df_tmp = rbind(df_tmp, df_tmp2[c(kk),])
            k = c(k, df_tmp2[kk, 15])
        }
        i = i - 1
    }
    return(df_tmp)
}


Route_length = function(df) {
    r_len = NULL
    for (k in c(1:(dim(df)[1] - 1))) {
        #        set1sp <- SpatialPoints(df[k, c('lon', 'lat')])
        #       set2sp <- SpatialPoints(df[k+1, c('lon', 'lat')])

        r_len = c(r_len, distm(df[c(k), c('lon', 'lat')], df[c(k + 1), c('lon', 'lat')]))
    }
    return(r_len)
}


Route_shortest = function(df4) {
    min_r = sum(Route_length(Route(df4, 1)))
    min_k = 1
    for (i in c(2:dim(df4)[1])) {
        r_l_ = sum(Route_length(Route(df4, i)))
        if (r_l_ < min_r) {
            min_r = r_l_
            min_k = i
        }
    }
    return(min_k)
}


df_routes = function(df_stops, numbers) {
    r_l = data.frame(Route_n = NULL, dist = NULL, V1 = NULL, V2 = NULL, V3 = NULL, V4 = NULL, V5 = NULL, V6 = NULL, V7 = NULL, V8 = NULL, V9 = NULL, V10 = NULL, V11 = NULL, V12 = NULL, V13 = NULL, V14 = NULL, V15 = NULL)
    for (route_n in numbers) {
        cat(paste(".", route_n, ".", sep = ""))

        df4 = df[Stops_list_for_route(df_stops, route_n),]
        df_tmp = Route(df4, Route_shortest(df4))
        r_l = rbind(r_l, cbind(rep(route_n, dim(df_tmp)[1]), c(Route_length(df_tmp), NA), df_tmp))
    }
    colnames(r_l) = c("route_n", "next_stop_dist", colnames(df))

    df_tmp = r_l

    del_list = NULL
    for (k in c(2:dim(df_tmp)[1])) {
        if ((df_tmp$route_n[k - 1] == df_tmp$route_n[k]) &&
            (df_tmp$name[k - 1] == df_tmp$name[k]) &&
            (df_tmp$next_stop_dist[k - 1] <= 150)) {
            # del string
            del_list = c(del_list, k - 1)
        }
    }
    #df_tmp[-del_list, 3]

    return(df_tmp[-del_list,])
}



#df_bus_routes = df_routes(df$bus, bus_numbers)
#write.csv2(x = df_bus_routes, file = file.path(dataDir, "Bus routes.csv"))
df_bus_routes = read.csv2(file = file.path(dataDir, "Bus routes.csv"))[, - c(1)]

#df_minibus_routes = df_routes(df$minibus, minibus_numbers)
#write.csv2(x = df_minibus_routes, file = file.path(dataDir, "Minibus routes.csv"))
df_minibus_routes = read.csv2(file = file.path(dataDir, "Minibus routes.csv"))[, - c(1)]

#df_tram_routes = df_routes(df$tram, tram_numbers)
#write.csv2(x = df_trolley_routes, file = file.path(dataDir, "Trolley routes.csv"))
df_trolley_routes = read.csv2(file = file.path(dataDir, "Trolley routes.csv"))[, - c(1)]

#df_trolley_routes = df_routes(df$trolley, trolley_numbers)
#write.csv2(x = df_tram_routes, file = file.path(dataDir, "Tram routes.csv"))
df_tram_routes = read.csv2(file = file.path(dataDir, "Tram routes.csv"))[, - c(1)]

df_bus_routes_2 = read.csv2(file = file.path(dataDir, "Bus routes 2.csv"), sep = ",")
df_bus_routes_3 = read.csv2(file = file.path(dataDir, "Bus routes 3.csv"), sep = ",")
df_bus_routes_4 = read.csv2(file = file.path(dataDir, "Bus routes 4.csv"), sep = ",")
df_bus_routes_5 = read.csv2(file = file.path(dataDir, "BusRoutes_DistrictsNew_no_blocks.csv"), sep = ",")
df_bus_routes_2 = df_bus_routes_5




# Districts Connection DF  -----
if (!require("circlize")) install.packages("circlize")

# List of all routes
route_n = as.character( unique(df_bus_routes_2$route_n) )

# List of unofficial districs passing by each route
df = data.frame(dist = NA, route_n = NA)
i = "2"
for (i in c(route_n)) {
    dists = df_bus_routes_2[df_bus_routes_2$route_n == i,]
    dists = as.character( unique(dists$name_2) )
    df = rbind( df, data.frame( dist = dists, route_n = rep( i, length(dists)) ) )
}
df = df[-c(1),]

# Table indicating (0-no, 1-yes) whether route passes a district
df5 = as.data.frame.matrix( table( df$dist, df$route_n ) )
df5 = as.data.frame.matrix(t(df5))

# Table with the number of routes connecting each pair of unofficial districts
df_dist_connect = array(dim = c(dim(df5)[2], dim(df5)[2]))
for (i in c(1:dim(df5)[2])) {
    for (j in c(1:dim(df5)[2])) {
        df_dist_connect[i,j] = table(df5[, i], df5[, j])[2,2]
    }
}

df_dist_connect[ df_dist_connect == 0 ] = NA

df_dist_connect = as.data.frame(df_dist_connect)
rownames( df_dist_connect ) = colnames(df5)
colnames( df_dist_connect ) = rownames( df_dist_connect )

for (i in c(1:dim(df_dist_connect)[1])) {
    df_dist_connect[i,i] = NA }

# Normalization of the table to values from 0 to 1
df_dist_connect = df_dist_connect/max( df_dist_connect, na.rm = T )

df_dist_connect = df_dist_connect / 2




# Mosaic ---
if (!require("vcd")) { install.packages("vcd"); require("vcd") }


#png(filename = file.path(plotDir, "Mosaic_Airline_vs_City.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

par(mar = rep(.5, 4))
mosaicplot( df_dist_connect, las = 2, col = "steelblue", main = "", cex = 0.3)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("grey", alpha.f = 0.35), srt = 45)

#dev.off()




# Circlize ----
#factors = c( colnames(df_dist_connect) )
#factors = factor(factors, levels = factors)

set.seed(10)
xlim = c(-1,1)




r_color = rand_color(length(unique(df_bus_routes_2$wikipedia)), 0.7)

# City official distric names column
df_bus_routes_2$wikipedia = as.character(df_bus_routes_2$wikipedia)
df_bus_routes_2$wikipedia = gsub("ru:", "", df_bus_routes_2$wikipedia)
df_bus_routes_2$name_3 = gsub(" \\(Ростов-на-Дону\\)", "", df_bus_routes_2$wikipedia)

# City unofficial distric names column
df_bus_routes_2$name_2 = as.character(df_bus_routes_2$name_2)

# Correspondence of official and unofficial districts DF
df_dist_corresp = data.frame(Dist = NA, Adm_dist = NA)
for (dist in c(rownames(df_dist_connect))) {
    adm_dist = unique(df_bus_routes_2$name_3[df_bus_routes_2$name_2 == dist])
    df_dist_corresp = rbind(df_dist_corresp, data.frame(Dist = rep(dist, length(adm_dist)), Adm_dist = adm_dist))
}

# Uniqnues of correspondence (Estate)
#df_dist_corresp = df_dist_corresp[c(2:11, 13:15, 17:19, 21:27, 29, 31:49, 51, 53, 55:60, 63),]

# Uniqnues of correspondence (Mikhail)
df_dist_corresp = df_dist_corresp[c(2:5, 8, 9, 11, 13, 15, 18, 20, 22, 23, 25, 28, 32, 33, 35, 36, 39, 42, 43, 44, 45, 47, 48, 49, 51, 53, 55, 58:60, 62, 63, 65, 66, 69, 71 
),]

# Coding of off.districts
library(dplyr)
df_dist_corresp <- df_dist_corresp %>%
        mutate(recode = as.numeric(factor(Adm_dist)))

# Reorder of Connection DF w.r.t. off.district names
dist_order = order(df_dist_corresp$Adm_dist)
factors = df_dist_corresp$Dist[dist_order]

df_dist_connect = df_dist_connect[dist_order, dist_order]


# Coloring of districts
label_color = function(df_dist_connect, sector.index) {
    cum_conn1 = dim(df_dist_connect)[1] - sum(is.na(df_dist_connect[sector.index,]))
    wd1 = cum_conn1 / dim(df_dist_connect)[1]
    return(adjustcolor("black",
        alpha.f = max(0.22, wd1)))
}

#df_tmp = df_bus_routes_2[, c(2, 4, 9, 10, 19, 30, 68, 69, 73)]

#font_import()
#loadfonts(device = "pdf", quiet = T)


# Circle Plot
#dev.off()
#pdf(file = paste(plotDir, "/Bus Routes Circlize.pdf", sep = ""), width = 12, height = 12, family = "Times")
png(filename = file.path(plotDir, "Bus Routes Circlize 2.png"), width = 1280, height = 1280, units = "px", pointsize = 16, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))


par(mfrow = c(1, 1))

circos.clear()
circos.par(
        start.degree = pi * 0.5,
        gap.degree = 1,
        track.margin = c(0.0005, 0.0005),
        cell.padding = c(0, 0, 0, 0))
circos.initialize(factors = factors, xlim = xlim)

title(main = "Связность районов г.Ростова-на-Дону", col = "darkgrey")#, cex = 0.1)

circos.info(plot = T)

# Districts
circos.trackPlotRegion(
                              track.index = 1,
                              ylim = c(0, 1),
                              panel.fun = function(x, y) {
                                  xlim = get.cell.meta.data("xlim")
                                  ylim = get.cell.meta.data("ylim")
                                  sector.index = get.cell.meta.data("sector.index")
                                  circos.text(median(xlim), 0.0,
                                             sector.index,
                                             #cex = 0.5,
                                             facing = "clockwise", adj = c(0.0, 0), niceFacing = F,
                                             col = label_color(df_dist_connect, sector.index)
                                             )
                              },
                              bg.border = NA,
                              bg.col = "white")


# Administrative District
circos.trackPlotRegion(
        track.index = 2,
        track.height = 0.025,
        ylim = c(0, 1),
        panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            sector.index = get.cell.meta.data("sector.index")
            #circos.rect( min(xlim), max(ylim), max(xlim), min(ylim), 
            #             sector.index, 
            #             col   = adjustcolor( (reg_group[ which( reg_group[,3]  == sector.index ), 4]) + 1, alpha.f = 0.2) )
            #     circos.text( label = reg_group[ which( as.character( reg_group[,3]) == sector.index ), 1], 
            #                   mean(xlim), mean(ylim), sector.index, cex = 0.6, facing = "inside", niceFacing = F)
        },
        bg.border = NA,
        bg.col = adjustcolor( df_dist_corresp$recode[order(df_dist_corresp$Adm_dist)], 0.3)
      )


# Connections
for (i in c(1:dim(df_dist_connect)[1])) {
    for (j in c(1:dim(df_dist_connect)[1])) {
        if ( !is.na( df_dist_connect[i, j]) ) {
            cum_conn1 = dim(df_dist_connect)[1] - sum(is.na(df_dist_connect[i,]))
            cum_conn2 = dim(df_dist_connect)[1] - sum(is.na(df_dist_connect[, j]))
            wd1 = cum_conn1 / dim(df_dist_connect)[1]# * df_dist_connect[i, j]
            wd2 = cum_conn2 / dim(df_dist_connect)[1]# * df_dist_connect[i, j]
            circos.link(
                rownames(df_dist_connect)[i], c(-wd1, wd1) , lty = 1,
                colnames(df_dist_connect)[j], c(-wd2, wd2),
                col = adjustcolor("cyan4", alpha.f = df_dist_connect[i, j]*0.5)) #col_datacor[i, j]) #, border = "white")
        }
    }
}

# Annotations
text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко \n для \n UrbanFactory", cex = 3, font = 2, col = adjustcolor("grey", alpha.f = 0.99), srt = 45)
# Legend
legend("topleft",
             legend = gsub(" район","",unique(df_dist_corresp$Adm_dist)),
             fill = adjustcolor( unique(df_dist_corresp$recode), 0.3),
             #cex = 0.5,
             title = "Район")



dev.off()




# Route plots ----

df2 = Stops_list_for_route2(df_minibus_routes, "40")
#df2 = df2[ df2$name %in% c("Универсам", "ул. Интернациональная"), ]

AllBusStops = plotmap(
                 lat = df2$lat, lon = df2$lon, API = "google2", zoom = 13,
                 col = c( "tomato",rep("steelblue", dim(df2)[1]-2), "tomato"), pch = 20, cex = 1.6)

# One bus stop
BusStop = plotmap(lat = 47.20661, lon = 39.7169, map, col = "purple", pch = "20", cex = 1.5)


str_count(string = df$bus, pattern = paste( " ", bus_numbers[1], sep = "") )



# All routes plots ----

# Buses
pdf(file = paste(plotDir, "/Bus stops.pdf", sep = ""), width = 12, height = 12, pointsize = 10)
AllBusStops = plotmap(lat = df$lat, lon = df$lon, API = "google2", zoom = 15,
                 col = "tomato", pch = 20, cex = 0.3 * df$n_bus)
dev.off()

# Minibus
pdf(file = paste(plotDir, "/Minibus stops.pdf", sep = ""), width = 12, height = 12, pointsize = 10)
AllBusStops = plotmap(lat = df$lat, lon = df$lon, API = "google2", zoom = 15,
                 col = "grey", pch = 20, cex = 0.3 * df$n_minibus)
dev.off()

# Trolleybuses
pdf(file = paste(plotDir, "/Trolley stops.pdf", sep = ""), width = 12, height = 12, pointsize = 10)
AllBusStops = plotmap(lat = df$lat, lon = df$lon, API = "google2", zoom = 15,
                 col = "steelblue", pch = 20, cex = 0.6 * df$n_trolley)
dev.off()

# Tram
pdf(file = paste(plotDir, "/Tram stops.pdf", sep = ""), width = 12, height = 12, pointsize = 10)
AllBusStops = plotmap(lat = df$lat, lon = df$lon, API = "google2", zoom = 15,
                 col = "green3", pch = 20, cex = 0.6 * df$n_tram)
dev.off()



# Bubble plot ----
map <- GetMap(center = c(df$lat[554], df$lon[554]), zoom = 15,
       size = c(640, 640), destfile = file.path(tempdir(), "meuse.png"),
        maptype = "mobile", SCALE = 1);





pdf( file = paste(plotDir, "/Bus routes heat.pdf", sep = ""), width = 7, height = 7, pointsize = 12)
#png(filename = file.path(plotDir, "Bus routes heat.png"), width = 640, height = 640, units = "px", pointsize = 16, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

df3 = df[df$n_bus > 0,]
par(cex = 0.5)
bubbleMap( df3, coords = c("lon", "lat"), map,
      zcol = 'n_bus', key.entries = 1 + 2 ^ (0:4), colPalette = colorRampPalette(c("steelblue", "tomato"))(length(1 + 2 ^ (0:4))), do.sqrt = T, alpha = 0.5, verbose = 0.5)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Stepan Sushko", cex = 5, font = 2, col = adjustcolor("steelblue", alpha.f = 0.2), srt = 45)

dev.off()


pdf(file = paste(plotDir, "/Minibus routes heat.pdf", sep = ""), width = 7, height = 7, pointsize = 12)
#png(filename = file.path(plotDir, "Minibus routes heat.png"), width = 640, height = 640, units = "px", pointsize = 16, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

df3 = df[df$n_minibus > 0,]
par(cex = 0.5)
bubbleMap( df3, coords = c("lon", "lat"), map,
      zcol = 'n_minibus', key.entries = 1 + 2 ^ (0:4), colPalette = colorRampPalette(c("steelblue", "tomato"))(length(1 + 2 ^ (0:4))), do.sqrt = T, alpha = 0.5, verbose = 0.5)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Stepan Sushko", cex = 5, font = 2, col = adjustcolor("steelblue", alpha.f = 0.2), srt = 45)

dev.off()


pdf(file = paste(plotDir, "/TrolleyBus routes heat.pdf", sep = ""), width = 7, height = 7, pointsize = 12)

#png(filename = file.path(plotDir, "TrolleyBus routes heat.png"), width = 640, height = 640, units = "px", pointsize = 16, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

df3 = df[df$n_trolley > 0,]
par(cex = 2)
bubbleMap( df3, coords = c("lon", "lat"), map,
      zcol = 'n_trolley', key.entries = c(1 + 1:4), colPalette = colorRampPalette(c("steelblue", "tomato"))(4), do.sqrt = T, alpha = 0.5, verbose = 0.5)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко\nдля\n       Ростов-Транспорт", cex = 5, font = 2, col = adjustcolor("steelblue", alpha.f = 0.2), srt = 45)

dev.off()

#png(filename = file.path(plotDir, "All routes heat.png"), width = 640, height = 640, units = "px", pointsize = 16, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

df3 = df[df$n_all > 0,]
bubbleMap(df3, coords = c("lon", "lat"), map,
      zcol = 'n_all', key.entries = 1 + 2 ^ (0:5), colPalette = colorRampPalette(c("steelblue", "tomato"))(6), do.sqrt = T, alpha = 0.5, verbose = 0.25)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко\nдля\n       Ростов-Транспорт", cex = 2, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

dev.off()

max(df$n_all )






# Metrics ----

fcn = function(xx) {
    if (!is.na(xx)) {
        unlist(strsplit(x = xx, split = ", "))
    } else return(NA)
    }

fcn2 = function(xxx, elem) {
    return(elem %in% xxx)
}

Stops_list_for_route = function(df, route) {
    list1 = lapply(df, FUN = fcn)
    list2 = unlist(lapply(list1, FUN = fcn2, elem = route))
    return(list2)
}


Stops_list_for_route2 = function(df_r, route_n) {
    return( df_r[df_r$route_n == route_n,] )
}
Stops_list_for_route2( df_tram_routes, "4" )


# Number of bus stops
tmp = NULL
for (bn in bus_numbers){
    df2 = Stops_list_for_route2(df_bus_routes[!(df_bus_routes$route_n %in% bus_numbers_circled),], bn)
    tmp = c(tmp, dim(df2)[1])}
df_sn = data.frame(route_name = bus_numbers, stops_n = tmp, vehicle = rep("bus", length(bus_numbers)))
summary( tmp )

# Number of bus stops
tmp = NULL
for (bn in minibus_numbers) {
    df2 = Stops_list_for_route2(df_minibus_routes[!(df_minibus_routes$route_n %in% minibus_numbers_circled),], bn)
    tmp = c(tmp, dim(df2)[1])}
df_sn = rbind(df_sn, data.frame(route_name = minibus_numbers, stops_n = tmp, vehicle = rep("minibus", length(minibus_numbers))))
summary(tmp)

# Number of bus stops
tmp = NULL
for (bn in trolley_numbers) {
    df2 = Stops_list_for_route2(df_trolley_routes[!(df_trolley_routes$route_n %in% trolley_numbers_circled),], bn)
    tmp = c(tmp, dim(df2)[1])}
df_sn = rbind(df_sn, data.frame(route_name = trolley_numbers, stops_n = tmp, vehicle = rep("trolley", length(trolley_numbers))))
summary(tmp)

# Number of bus stops
tmp = NULL
for (bn in tram_numbers) {
    df2 = Stops_list_for_route2(df_tram_routes, bn)
    tmp = c(tmp, dim(df2)[1])}
df_sn = rbind(df_sn, data.frame(route_name = tram_numbers, stops_n = tmp, vehicle = rep("tram", length(tram_numbers))))
summary(tmp)

df_sn = df_sn[ df_sn$stops_n != 0, ] 




# Plots 
ggplot(data = df_sn, aes(vehicle, stops_n)) +
    geom_boxplot( ) + xlab("") + ylab("")


df_sn2 = as.data.frame(t(cbind(
    summary(df_sn[df_sn$vehicle == "bus", 2]),
    summary(df_sn[df_sn$vehicle == "minibus", 2]),
    summary(df_sn[df_sn$vehicle == "trolley", 2]),
    summary(df_sn[df_sn$vehicle == "tram", 2]))))

df_sn2 = cbind(df_sn2, c("автобус", "маршрутка", "тролейбус", "трамвай"))
colnames(df_sn2)[7] = "vehicle"

df_routes_n = data.frame(
    vehicle = c("автобус", "маршрутка", "тролейбус", "трамвай"),
    routes_n = c(length(bus_numbers), length(minibus_numbers), length(trolley_numbers), length(tram_numbers)))
p00 = ggplot(data = df_routes_n, aes(vehicle, routes_n)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Число маршрутов\n в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm")) + ylim(c(0,70))

df_routes_n = data.frame(
    vehicle = c("автобус", "маршрутка", "тролейбус", "трамвай"),
    routes_n = c(length(bus_numbers_circled), length(minibus_numbers_circled), length(trolley_numbers_circled), length(tram_numbers_circled)))
p0 = ggplot(data = df_routes_n, aes(vehicle, routes_n)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Число кольцевых маршрутов\n в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm")) + ylim(c(0, 70))



p1 = ggplot(data = df_sn2, aes(vehicle, Min.)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Минимальное число остановок\n на маршруте в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm"))

p2 = ggplot(data = df_sn2, aes(vehicle, Max.)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Максимальное число остановок\n на маршруте в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm"))

p3 = ggplot(data = df_sn2, aes(vehicle, Mean)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Среднее число остановок\n на маршруте в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm"))

df_tmp = data.frame(len = NULL, route_n = NULL)
bn = "1"
for (bn in c(bus_numbers))
{
    df_tmp = rbind( df_tmp, c(bn, sum( df_bus_routes[ as.character(df_bus_routes$route_n) == bn, 2]) ))
}

p4 = ggplot(data = df_sn2, aes(vehicle, Min.)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Минимальное число остановок\n на маршруте в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm"))

p5 = ggplot(data = df_sn2, aes(vehicle, Max.)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Максимальное число остановок\n на маршруте в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm"))

p6 = ggplot(data = df_sn2, aes(vehicle, Mean)) +
    geom_bar(stat = "identity", fill = "steelblue") + ggtitle("Среднее число остановок\n на маршруте в Ростове") + xlab("") + ylab("") + theme(plot.margin = unit(c(0.0, 0.0, -0.2, 0.0), "cm"))


png(filename = file.path(plotDir, "Routes stats 1.png"), width = 300, height = 400, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows"))

grid.arrange( p00, p0, ncol = 1)

dev.off()

png(filename = file.path(plotDir, "Routes stats 2.png"), width = 300, height = 600, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows"))

grid.arrange(p1, p2, p3, ncol = 1)

dev.off()



df_sn3 = as.data.frame(t(cbind(
    summary(df_bus_routes$next_stop_dist),
    summary(df_minibus_routes$next_stop_dist),
    summary(df_trolley_routes$next_stop_dist),
    summary(df_tram_routes$next_stop_dist)
)))
df_sn3 = cbind(df_sn3, c("автобус", "маршрутка", "тролейбус", "трамвай"))
colnames(df_sn3)[8] = "vehicle"



warnings()




# Trash ----

suppressWarnings(require(loa, quietly = TRUE))
suppressWarnings(require(latticeExtra, quietly = TRUE))

data(incidents)

cols <- c("yellow", "darkred")
GoogleMap(~lat * lon | DayOfWeek, #plot conditioned         
          data = incidents, map = NULL,
          groups = incidents$violent,
          col = cols, cex = 0.1, alpha = 0.3,
          panel = panel.loaPlot2,
          key.groups = list(main = "Violent",
                          cex = 1, col = cols))

library(latticeExtra)
useOuterStrips(
    GoogleMap(~lat * lon, #plot conditioned         
          data = df, map = AllBusStops,
          panel = panel.kernelDensity, #surface plot control                  
    col.regions = c("lightyellow", "darkred"), #
    alpha.regions = 0.5, #                       
    col = 1, #surface calculation    
    n = 200, at = c(1, 5, 10, 15, 20),
    scales = list(draw = FALSE), xlab = "", ylab = ""))



data(lat.lon.meuse, package = "loa", envir = environment())




max( df$n_all )
