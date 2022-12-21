#paleo climate with modern pollen anomlies

rm(list =ls())
library(raster)
library(sf)
library(tidyverse)

path <- "C:/Users/lschild/Documents/pollen/data/cru"
anoms <- read.csv("C:/Users/lschild/Documents/pollen/bistab/biome/local_anom.csv")

clim_vars <- c("MAT",
               "Jul",
               "prec")

for(var in clim_vars){
  #extract modern/reference climate from cru
  cru <- raster(paste0(path,"/modern_",
                       var,".tif"))
  coords <- Temp %>%
    filter(Dataset_ID %in% anoms$Dataset_ID)%>%
    group_by(Dataset_ID) %>%
    summarize(Lat = mean(Latitude),
              Lon = mean(Longitude))
  res <- extract(cru,
                    coords[,c("Lon","Lat")],
                    buffer = 100000,
                    fun = mean)
  
  col <- ifelse(var == "Jul",
                "TJul_WAPLS",
                ifelse(var == "MAT",
                       "MAAT_WAPLS",
                       "Prec_WAPLS"))
  modern <- cbind(coords, clim = res) %>%
    filter(!(is.na(clim)))%>%
    merge(anoms)%>%
    mutate(abs = get(col)+clim)
  
  save <- modern %>%
    dplyr::select(Dataset_ID, slice, Lat, Lon, abs)
  names(save)[length(names(save))] <- var
  
  write.csv(save,
            paste0("C:/Users/lschild/Documents/pollen/bistab/biome/pollen_anom_",
                   var,
                   ".csv"),
            row.names = FALSE)
  
  rasterize
  Points <- sf::st_as_sf(coords = c("Lon","Lat"),
                              x = modern,
                              crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
  empty <- raster(ncol = 36,
                  nrow = 18)
  make_raster <- function(slices){
    sub <- Points %>%
      filter(slice == slices)

    sub_raster <- rasterize(sub,
                            empty,
                            field = "abs",
                            fun = mean)
    sub_raster <- crop(sub_raster,
                       extent(c(-180,180,20,90)))
    return(sub_raster)
  }

  rasters <- lapply(as.numeric(as.character(unique(Points$slice))),
                    make_raster)

  r <- do.call(brick, rasters)
  names(r) <- as.character(unique(Points$slice))

  world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  #plot maps for slices
  for(index in 1:length(names(r))){
    raster <- r[[index]]
    name <- paste(sub("X","",names(raster)),"BP")

    png(paste0("C:/Users/lschild/Documents/pollen/bistab/plots/anom_temp/",
               var,"_",
               name,
               ".png"),
        width = 1200,
        height = 600)
    plot(world$geometry,
         ylim = c(30,90),
         main = name,
         sub = var)
    plot(raster,
         add =TRUE,
         alpha = 0.5,
         legend = FALSE,
         main = name,
         interpolate = TRUE)
    contour(raster,
            add = TRUE,
            col = par("fg"),
            nlevels = 10,
            lwd = 2,
            labcex = 1,
            interpolate = TRUE)
    dev.off()
  }
}
