# Aquest mòdul genera l'arxiu de recorregut que utilitza el processador com a recorregut teòric
# pren el fitxer "recorregut_cursa.gpx" i genera un "recorregut_cursa.csv"

# Sempre començem amb una netaja de l'estat
rm(list=ls())


library(gpx, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(sp, warn.conflicts = FALSE)
library(RcppTOML, warn.conflicts = FALSE)
library(readODS, warn.conflicts = FALSE)


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# PARAMETRITZACIÓ  ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# llegim les configuracions
configuracio_cursa <- RcppTOML::parseToml(file.path("configuracio.tom"))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# LECTURA INICIAL DEL RECORREGUT ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Càrrega del track a processar -------------------------
gpx_raw <- gpx::read_gpx(file=file.path(configuracio_cursa$fitxers_usuari$FITXER_BASE_RECORREGUT))

# [[1]] >>> ... el track
track_referencia_raw <- gpx_raw$tracks[[1]] %>% 
  as_tibble() %>%
  select(location_x = Longitude, location_y = Latitude, location_z = Elevation)

# EL gpx ja està ordenat de forma natural
track_referencia_master <- track_referencia_raw %>% mutate(ordre_punt = 1:n())

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# SIMPLIFICACIÓ DEL RECORREGUT ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Per millorar el remostreig de després, unim les rectes que
# estiguin dividides

# (codi adaptata de proposta de chatGPT)

eliminar_puntos_linea_recta <- function(coords_x, coords_y, tolerancia) {
  # coord_x <- track_referencia_master$location_x
  # coord_y <- track_referencia_master$location_y
  # tolerancia <- 0.0001
  coords <- matrix(cbind(coords_x, coords_y), ncol = 2)
  new_coords <- coords[1,] # Inicializar con el primer punto
  
  for (i in 2:(nrow(coords)-1)) {
    p1 <- coords[i-1,]
    p2 <- coords[i,]
    p3 <- coords[i+1,]
    d1 <- spDistsN1(matrix(p1, ncol = 2), p2, longlat = TRUE) # Distancia entre p1 y p2
    d2 <- spDistsN1(matrix(p2, ncol = 2), p3, longlat = TRUE) # Distancia entre p2 y p3
    d3 <- spDistsN1(matrix(p1, ncol = 2), p3, longlat = TRUE) # Distancia entre p1 y p3
    if (d1 + d2 <= (1 + tolerancia) * d3) {
      # Si la distancia entre p1 y p3 es menor o igual que la suma de las distancias entre p1 y p2 y entre p2 y p3
      # entonces el punto p2 forma parte de una línea recta entre p1 y p3, por lo que se omite
      next
    } else {
      new_coords <- rbind(new_coords, p2) # Agregar el punto p2
    }
  }
  new_coords <- rbind(new_coords, coords[nrow(coords),]) # Agregar el último punto
  return(new_coords)
}


# La tolerància ha de ser petita per que només
# volem esborrar els punts que no donen forma al recorregut
punts_simplificats <- eliminar_puntos_linea_recta(track_referencia_master$location_x, track_referencia_master$location_y, 0.000001) %>%
  as.data.frame(row.names = FALSE) %>%
  as_tibble() %>%
  rename(location_x = V1, location_y = V2)


track_referencia_master_simplificat <- track_referencia_master %>%
  inner_join(punts_simplificats, by = join_by(location_x, location_y))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# AUGMENT DE LA RESOLUCIÓ (INTERPOLACIÓ) ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

track_referencia_master_interpolat <- tibble(ordre_punt = numeric(), location_x=numeric(), location_y=numeric(), location_z=numeric())

for (i in 1:(nrow(track_referencia_master_simplificat)-1)) {
  # Codi basat en proposta de ChatGPT
  # i <- 2
  p1 <- track_referencia_master_simplificat[i,]
  p2 <- track_referencia_master_simplificat[i+1,]
  
  distancia <- sp::spDistsN1(pts = as.matrix(p1[,c("location_x","location_y")]), pt = as.matrix(p2[,c("location_x","location_y")]),  longlat = TRUE) * 1000
  
  track_referencia_master_interpolat <- track_referencia_master_interpolat %>% 
    bind_rows(data.frame(ordre_punt = p1$ordre_punt,
                         location_x = p1$location_x,
                         location_y = p1$location_y,
                         location_z = p1$location_z))
  
  if (distancia >= configuracio_cursa$configuracio_sistema$RESOLUCIO_INTERPOLACIO) {
    # Només subdividim si el segment es menor de 5 metres
    m <- ceiling(distancia / configuracio_cursa$configuracio_sistema$RESOLUCIO_INTERPOLACIO)
    for (j in 1:(m-1)) {
      # j <- 1
      t <- j / m
      track_referencia_master_interpolat <- track_referencia_master_interpolat %>% 
        bind_rows(data.frame(ordre_punt = p1$ordre_punt,
                             location_x = p1$location_x*(1-t) + p2$location_x*t,
                             location_y = p1$location_y*(1-t) + p2$location_y*t,
                             location_z = p1$location_z*(1-t) + p2$location_z*t))
    }
  }
}


track_referencia_master_interpolat$ordre_punt <- 1:nrow(track_referencia_master_interpolat)


# De nou, fem una simplificació del recorregut ara eliminant els vértex on el seu pròxim punt
# està a distància igual o menor a un llindar
track_referencia_master_interpolat_simplificat <- list()

for (i in 1:(nrow(track_referencia_master_interpolat)-1)) {
  # Codi basat en proposta de ChatGPT
  # i <- 2
  p1 <- track_referencia_master_interpolat[i,]
  p2 <- track_referencia_master_interpolat[i+1,]
  
  distancia_seguent <- sp::spDistsN1(pts = as.matrix(p1[,c("location_x","location_y")]), pt = as.matrix(p2[,c("location_x","location_y")]),  longlat = TRUE) * 1000
  
  if (distancia_seguent >= configuracio_cursa$configuracio_sistema$DISTANCIA_MINIMA_VERTEXS) {
    track_referencia_master_interpolat_simplificat[[length(track_referencia_master_interpolat_simplificat)+1]] <- track_referencia_master_interpolat[i,]
  }
  
  
}
  
  
track_referencia_master_interpolat_simplificat <- bind_rows(track_referencia_master_interpolat_simplificat)

# a track_referencia_master_interpolat_simplificat tenim
# el recorregut fi interpolat segons configuracio_cursa$configuracio_sistema$RESOLUCIO_INTERPOLACIO


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# DESAT DE DADES PEL PROCESSADOR ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# write.csv(track_referencia_master_interpolat_simplificat, file = configuracio_cursa$configuracio_sistema$FITXER_RECORREGUT, row.names = FALSE)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# GENERACIÓ DEL PLÀNOL ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

if (FALSE) {
  # No funciona fora d'RStudio (error pandoc)
  library(leaflet)
  library(mapview)
  
  # CRS per defecte a leaflet:
  # EPSG:3857 >>> WGS 84 / Pseudo-Mercator 
  pois <- readODS::read_ods(configuracio_cursa$fitxers_usuari$FITXER_POIS)
  
  mapa <- 1
  
  leaflet() %>%
    addTiles(options = providerTileOptions(minZoom = 8, maxZoom = 50)) %>%
    # addPolylines(track_referencia_raw$location_x, track_referencia_raw$location_y) %>%
    addCircleMarkers(track_referencia_master_interpolat_simplificat$location_x, 
                     track_referencia_master_interpolat_simplificat$location_y, 
                     # label = track_referencia_master_interpolat_simplificat$ordre_punt, 
                     # labelOptions = labelOptions(noHide = T, textOnly = FALSE, textsize = "12px"),
                     radius = 0.1, color = "red", opacity = 1) %>%
    addCircleMarkers(track_referencia_raw$location_x, 
                     track_referencia_raw$location_y, 
                     radius = 3, color = "blue", opacity = 0.5)
    
    # addPolylines(track_referencia_master_interpolat_simplificat$location_x, track_referencia_master_interpolat_simplificat$location_y, color = "red", weight = 5, fillOpacity = 1  ) %>%
    # addAwesomeMarkers(pois$location_x, pois$location_y, icon = makeAwesomeIcon(icon = 'heart', markerColor = 'orange'))
  
  # gravem el mapa com a arxiu per l'usuari
  mapshot(mapa, file = "mapa_del_recorregut.png", vwidth = 2000, vheight = 2000)
}