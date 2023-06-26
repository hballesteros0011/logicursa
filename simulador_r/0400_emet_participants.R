
# Mòdul de simulació de posicions GPS
rm(list=ls())

library(logger)
library(gpx)
library(jsonlite)
library(httr)
library(RPostgres)
library(RcppTOML)
library(tidyverse)
library(readODS)


source(file="0100_inicialitza_simulador.R")

# Configuració de l'script -----------------------

# Inicialitzem la bbdd per començar de nou?
INICIALITZA_BBDD <- TRUE # No està implementat el FALSE!!!



# Esborrem el contingut de les taules ------------------------------
if (INICIALITZA_BBDD) {
  log_debug("Esborrem el contingut de les taules de la BBDD implicades en la simulació")

  # Fem una conexió directa per gestionar la configuració de la cursa
  connexio_bbdd <- dbConnect(drv=Postgres(), 
                             user="logicursa_processador", 
                             password="uoc-tfg1",
                             host="localhost", 
                             port=5432, 
                             dbname="logicursa",
                             bigint="numeric") # per evitar que emeti int64
  
  dbExecute(connexio_bbdd, "DELETE FROM logicursa.in_stream_locations;") # esborrem primer aquesta pels foreign keys

}





# Carrega del GPS de base --------------------------------------------
log_debug("Carrega del GPS dels members")

localitzacions_members <- read.csv2(file.path(directori_origen_dades,"gps_members.csv")) %>% as_tibble()

tracks_a_simular <- localitzacions_members %>%
  mutate(cursa_id = configuracio$identificador_cursa) %>%
  mutate(time = ymd_hms(time, tz = "Europe/Madrid"))

if (FALSE) {
  localitzacions_members %>% distinct(member_id)
  
  
}

# Participants -------------------------------------------------
log_debug("Establiment de participants a simular")



# llegim les configuracions de la cursa actual
directori_processador <- file.path("..","processador_r")
configuracio_processador <- RcppTOML::parseToml(file.path(directori_processador,"configuracio.tom"))
configuracio_cursa <- RcppTOML::parseToml(file.path(directori_processador,configuracio_processador$processador$DIRECTORI_BASE_CURSA, configuracio_processador$processador$FITXER_CONFIGURACIO_CURSA))


participants_cursa_raw <- read_ods(file.path(directori_processador, configuracio_processador$processador$DIRECTORI_BASE_CURSA,configuracio_cursa$fitxers_usuari$FITXER_BASE_PARTICIPANTS))  %>%
  mutate(cursa_id = configuracio_cursa$dades_cursa$identificador_cursa)

# Número de participants a simular
n_participants <- nrow(participants_cursa_raw)

# Enviament de coordenades -----------------------------------------------
log_debug("Enviament de coordenades")


# definim la funció que envia una localització al servidor
envia_localitzacio <- function(localitzacio) {
  log_debug("envia_localitzacio")
  
  # PER UNIT TEST
  # saveRDS(localitzacio, "c:\\temp\\localitzacio.rds")
  # stop()
  # localitzacio <- readRDS("c:\\temp\\localitzacio.rds")
  
  if (FALSE) {
    # unit test
    localitzacio <- list(member_id = 1,
                         cursa_id = configuracio$identificador_cursa,
                         time = ymd_hms("2023-03-01 20:34:45", tz = "Europe/Madrid"),
                         longitude = -3.804239,
                         latitude = 37.77829,
                         elevation = 544.615)
    
    dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"))
    
  }
  
  
#   -- Taula de recepció de les posicions per part dels participants
#   CREATE TABLE logicursa.in_stream_locations (
#     id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
#     cursa_id CHARACTER(10), -- Identificador de la cursa
#     participant_num INTEGER, -- Número de participant ("Dorsal")
#     location_x REAL, --coordenades
#     location_y REAL, --coordenades
#     location_z REAL, --coordenades
#     in_stream_dt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, -- instant d'ingesta a la BBDD (auto)
# 	  json_data text -- text en format json amb informació addicional i opcional
#   );

  dades_a_enviar <- list(cursa_id = localitzacio$cursa_id,
                         in_stream_dt = localitzacio$time,
                         participant_num = localitzacio$member_id,
                         location_x = localitzacio$longitude,
                         location_y = localitzacio$latitude,
                         location_z = localitzacio$elevation)
  
  
  headers_post <- add_headers(.headers = c("Authorization" = paste0("Bearer ",TOKEN_BBDD_PRODUCER),
                                           "Content-Type" = "application/json"))
  
  # Enviament a postREST
  # Fem un POST enlloc d'un PUT per fer servir els camps per defecte definits a la BBDD
  resposta <- tryCatch({
    
    POST(url = "http://localhost:3000/in_stream_locations",
         encode = "json",
         headers_post,
         body = dades_a_enviar)
    
  }, error = function(e) {
    NULL
  })
  
  
  if (is.null(resposta)){
    log_error("No hem pogut enviar la localitzacio")
  } else {
    # text_log <- paste0("json_localitzacio = ",toString(cadena_json_localitzacio_actual),
    #                    ",url = ",resposta$url,
    #                    ",status_code = ",resposta$status_code,
    #                    ",date = ",resposta$date,
    #                    ",times = ",paste0(resposta$times, collapse = ","))
    
    text_log <- paste0("resposta$status_code = ",resposta$status_code,
                       ",resposta$date = ",resposta$date)
    
    # log_debug(skip_formatter(text_log))

  }
  
  # i també fem una visualització
  
  resposta
  
}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# BUCLE PRINCIPAL DE SIMULACIÓ -----------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# instants d'inici i final de la cursa
inici_cursa <- min(tracks_a_simular$time)
final_cursa <- max(tracks_a_simular$time)


nombre_segons_simular <- as.numeric(final_cursa - inici_cursa, units = "secs")

# curl http://localhost:3000/in_stream_locations -X POST -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" -d '{"member_id": "1","reading_dt": "10:15:30","location_x": "14.4164","location_y": "2.212426","location_z": "10.968"}'
# -d '{"member_id": "1","reading_dt": "10:15:30","location_x": "14.4164","location_y": "2.212426","location_z": "10.968"}'

for (sec_actual in c(1:nombre_segons_simular)) {
  # sec_actual <- 1
  
  instant_actual <- inici_cursa + dseconds(sec_actual) - 1
  
  members_en_linia <- length(unique(tracks_a_simular[tracks_a_simular$time >= instant_actual,]$member_id))
  
  
  
  # Senyals de participants en el segon actual
  senyals_segon_actual <- tracks_a_simular[tracks_a_simular$time == instant_actual,]
  
  if (FALSE) {
    senyals_segon_actual %>%
      count(member_id, time, name = "nombre_de_senyals") %>%
      filter(nombre_de_senyals > 1)
  }
  
  #fem un enviament per cada senyal d'aquest segon
  # això ho fem tant ràpid com poguem
  # simplifiquem no utilitzant paral·lelitzacions (makeCluster et al)
  
  if (nrow(senyals_segon_actual) > 1)
    for (i_senyal_actual in 1:nrow(senyals_segon_actual)) {

      senyal_actual_a_enviar <- list(member_id = senyals_segon_actual[i_senyal_actual,]$member_id,
                                     cursa_id = senyals_segon_actual[i_senyal_actual,]$cursa_id,
                                     time = senyals_segon_actual[i_senyal_actual,]$time,
                                     longitude = senyals_segon_actual[i_senyal_actual,]$longitude,
                                     latitude = senyals_segon_actual[i_senyal_actual,]$latitude,
                                     elevation = senyals_segon_actual[i_senyal_actual,]$elevation)
      
      resposta <- envia_localitzacio(senyal_actual_a_enviar)
      
      if (is.null(resposta)) {
        # No s'ha pogut enviar el missatge, pausem uns segons per
        # que l'usuari pugui fer quelcom
        Sys.sleep(5)
        
      }

      log_debug(paste("Enviat instant:",instant_actual, ". Nombre d'actualitzacions:", nrow(senyals_segon_actual), ". Membres actius:",members_en_linia," Respota HTTP:", resposta$status_code))      
    } else {
      log_debug(paste("Enviat instant:",instant_actual, ". Nombre d'actualitzacions:", nrow(senyals_segon_actual), ". Membres actius:",members_en_linia," Sense enviament."))      
    }
  
  
  # Esperem pel següent enviament
  Sys.sleep(1/configuracio$factor_velocitat)
  
}
  
  # Esborrem les in_stream_locations per començar de nou
  # TODO: Hem de fer que el processador es reinicii, ja que ara acumula i acumula
  # dbExecute(connexio_bbdd, "DELETE FROM logicursa.in_stream_locations;")


# Tancament de la BBDD ------------------------------------------
log_debug("Tancament de la BBDD")

dbDisconnect(connexio_bbdd)


if (FALSE) {
  #sandbox
  install.packages("leaflet") 
  
  library(leaflet)
  m <- leaflet() %>%
    addTiles() %>%
    addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R")
  
  m  
  
  
}