
rm(list=ls())


library(tidyverse)
library(RcppTOML)
library(purrr)


configuracio_toml <- RcppTOML::parseToml(file.path("..","configuracio.tom"))

gps_mostra_estatica <- gpx::read_gpx(file=file.path("gps_stream.gpx")) 

classificacio <- readxl::read_xlsx(file.path("Y:\\repo_tfg\\simulador_r\\carrera-noche-de-san-anton-jaen\\dades_base\\CLASIFICACION-SAN-ANTON-VIRTUAL-2021.xlsx"), sheet = "Sheet1") %>% 
  as_tibble() %>% 
  mutate(META = as.numeric(gsub("^([[:digit:]]+)'([[:digit:]]+).*","\\1.\\2", META)))



track_referencia <- gps_mostra_estatica$tracks$`Carrera noche de San Anton Jaen` %>% 
  as_tibble() 


num_temps_interes <- configuracio_toml$simulacio$nombre_participants


# Obtenció de les classificacions a simular ----------------------------------------
# Quines classificacions volem simular?

# de cada categoria, agafem, aleatòriament, un número de participants
num_categories <- nrow(classificacio %>% distinct(CATG))

# Respectem el pes de les categories per sumar finalment el nombre requerit de participants
nombre_participants_simular_per_categoria <- classificacio %>% 
  count(CATG, name = "nombre_participants") %>% 
  mutate(pc_participants = nombre_participants / sum(nombre_participants),
         nombre_participants_simulacio = ceiling(num_temps_interes*pc_participants))

# Obtenim les classificacions que voldrem simular
classificacions_a_simular <- classificacio %>%
  left_join(nombre_participants_simular_per_categoria %>% select(CATG, nombre_participants_simulacio), by = join_by(CATG)) %>%
  group_by(CATG) %>%
  sample_n(nombre_participants_simulacio, replace = TRUE) %>%
  select(-nombre_participants_simulacio) %>%
  ungroup()


# Adaptació GPX de referència ----------------------------------------
# Quin factor de temps a aplicar al GPX per cada participant?


# ... Només volem un senyal per segon

stopifnot(nrow(track_referencia %>% count(Time) %>% filter(n>1)) == 0)
  

# ... Temps de referència
temps_a_mostra <- as.period(max(track_referencia$Time) - min(track_referencia$Time))

# ... identificadors de membre
classificacions_a_simular$member_id <- c(1:nrow(classificacions_a_simular))

# ... Trobem el factor a aplicar per cada classificacio
classificacions_a_simular$PERIOD <- hms(classificacions_a_simular$TIEMPO)
classificacions_a_simular$FACTOR_REFERENCIA <- classificacions_a_simular$PERIOD / temps_a_mostra


# ... apliquem la corba de no finalització

# per cada classificació
# ... un 70 percent acaba el recooregut (tipus 1)
# ... un 5 percent cau al percentil .50 del seu temps (tipus 2)
# ... un 10 percent cau al percentil .80 del seu temps (tipus 3)
# ... un 5 percent cau al percentil .95 del seu temps (tipus 4)
# el procés és... continua 10 minuts emetent la darrera posició i finalitza la sèrie
classificacions_a_simular <- classificacions_a_simular %>%
  group_by(CATG) %>%
  mutate(acaba_carrera = sample(c(1,2,3,4), prob=c(0.7,0.05,0.1,0.05), size = n(), replace = TRUE)) %>%
  ungroup()

# no cal que el paràmetre "prob" sumi zero:
# https://stackoverflow.com/questions/59918865/what-happens-when-prob-argument-in-sample-sums-to-less-greater-than-1
# Amb aquesta selecció la proporció real queda:
# 78, 5.5, 11, 5.5

# fem una funció per aplicar l'ajust a cada track d'interès
transforma_track <- function(track, factor, tipus_acaba_carrera) {
  # track <- track_referencia
  # factor <- 0.9700621
  # tipus_acaba_carrera <- 1
  
  # El principi es ajustar el temps acumulat segon a segon
  
  noms_originals <- names(track)
  
  # Aplicació del coeficient de correcció del temps
  nou_track <- track %>%
    ungroup() %>%
    arrange(Time) %>%
    mutate(acum_s_track = Time - min(Time),
           acum_s_sim = floor(acum_s_track * factor),
           Time = min(Time) + acum_s_sim) %>%  # Substituim el Time Original
    select(all_of(noms_originals))
    
  # aquest procés pot generar més d'una posició per segon
  # ens quedem amb qualsevol d'elles dintre de cada segon
  nou_track <- nou_track %>%
    group_by(Time) %>%
    sample_n(1) %>%
    ungroup()
  
  # baixem la resolució temporal del track
  nou_track <- nou_track %>%
    mutate(temps_amb_anterior = Time - lag(Time)) %>%
    replace_na(list(temps_amb_anterior = dseconds(0))) %>%
    filter(temps_amb_anterior >= configuracio_toml$simulacio$minim_temps_entre_senyals)
    
  
    
  
  
  # aplicació del criteri de finalització
  quantils <- c(1,0.5,0.8,0.95)
  
  punt_fallada <- quantile(nou_track$Time, probs = quantils[tipus_acaba_carrera])
  punt_fi_track <- punt_fallada + dminutes(10)

  # On ha fallat el membre?
  darrer_punt_abans_de_fallada <- nou_track[nou_track$Time >= punt_fallada,][1,]
  
  # d'aquí en endavant, totes les posicions son el punt de fallada
  nou_track[nou_track$Time >= punt_fallada,]$Elevation <- darrer_punt_abans_de_fallada$Elevation
  nou_track[nou_track$Time >= punt_fallada,]$Latitude <- darrer_punt_abans_de_fallada$Latitude
  nou_track[nou_track$Time >= punt_fallada,]$Longitude <- darrer_punt_abans_de_fallada$Longitude
  
  # i passats 10 minuts, tallem el track
  nou_track <- nou_track[nou_track$Time <= punt_fi_track,]

  nou_track
}


# Fem una combinatòria i apliquem un map al track original
tracks_simulacio <- crossing(track_referencia, classificacions_a_simular) %>% 
  nest(track_referencia = names(track_referencia)) %>%
  mutate(track_adaptat = pmap(list(track_referencia, FACTOR_REFERENCIA, acaba_carrera), transforma_track)) %>%
  select(-track_referencia)


# Preparem exportació i situem l'inici de la carrera a l'inici del semestre del TFG
tracks_a_exportar <- tracks_simulacio %>%
  unnest(track_adaptat) %>%
  select(member_id, longitude = Longitude, latitude = Latitude, elevation = Elevation, time = Time) %>%
  mutate(time = lubridate::ymd_hms(paste("2023-03-01",format(time, "%H:%M:%S")), tz = "Europe/Madrid"))

stop("Grabació manual")
# Grabació als fitxers d'interès
write.csv2(tracks_a_exportar, file = "gps_members.csv", row.names = FALSE)
write.csv2(tracks_simulacio %>% select(-track_adaptat), file = "tracks_simulacio.csv", row.names = FALSE)







