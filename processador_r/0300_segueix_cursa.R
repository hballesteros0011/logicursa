
# Mòdul de simulació procés de dades de la cursa
rm(list=ls())

library(logger, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(RPostgres, warn.conflicts = FALSE)
library(RcppTOML, warn.conflicts = FALSE)
library(hms, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(rstack)
library(R6)
#library(HiddenMarkov)
library(sp)
library(sf)
#library(leaflet)
#library(jsonlite, warn.conflicts = FALSE)

fes_pausa <- function() {
  cat("Prem una tecla per continuar")
  library = readLines(con = "stdin", 1)
}


# Configuració de l'script -----------------------
# log_layout(layout_glue_colors)
log_threshold(DEBUG)
# log_threshold(INFO)
log_appender(appender_tee("processador.log.txt"))

options(pillar.print_max = 50)
options(pillar.print_min = 50)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# INICIALITZACIÓ DEL MÒDUL ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_info("INICIALITZACIÓ DEL MÒDUL")

FENT_DEBUG <- TRUE   # AGAFEM COORDENADES DE FITXER GPX?
DEBUG_ESCALA_TEMPS <- 10   # Factor de multiplicació de temps en mode de debug


# llegim les configuracions
configuracio_processador <- RcppTOML::parseToml(file.path("configuracio.tom"))
configuracio_cursa <- RcppTOML::parseToml(file.path(configuracio_processador$processador$DIRECTORI_BASE_CURSA, configuracio_processador$processador$FITXER_CONFIGURACIO_CURSA))

# copiem a variables les constants més emprades

cursa_id <- configuracio_cursa$dades_cursa$identificador_cursa
interval_ingesta_bbdd = configuracio_processador$processador$INTERVAL_INGESTA_BBDD


normalitza_tipus_query <- function(taula_de_dbquery) {
  # Funció per aplicar normalitzacions a les taules obtingudes de la
  # base de dades
  # taula_de_dbquery <- tbl_in_stream_locations_raw
  
  #Netejem els espais en blanc dels camp de text
  columnes_text <- names(taula_de_dbquery)[sapply(taula_de_dbquery, is.character)]
  if (length(columnes_text)>1)
    taula_de_dbquery[, columnes_text] <- lapply(taula_de_dbquery[, columnes_text], trimws)
  
  taula_de_dbquery
  
}


bipbip <- function() {
  
  # en l'entorn de producció no es poden fer bip bips....
  if ( Sys.info()["sysname"] == "Windows" ) beepr::beep()
  
}


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# CONNEXIÓ A LA BBDD DE LOGICURSA --------------------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_info("CONNEXIÓ A LA BBDD DE LOGICURSA")

connexio_bbdd <- dbConnect(drv=Postgres(), 
                           user="logicursa_processador", 
                           password="uoc-tfg1",
                           # host="localhost", 
                           host = "192.168.1.200",
                           port=5432, 
                           dbname="logicursa",
                           bigint="numeric") # per evitar que emeti int64



# dbListTables(connexio_bbdd)
# dbDisconnect(connexio_bbdd)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# PREPARACIÓ DE DEPURACIÓ ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

if (FENT_DEBUG) {
  # Si està activat el FENT_DEBUG, fem una càrrega de localitzacions
  log_debug("PREPARACIÓ DE DEPURACIÓ")

  
  
  # Afegim coordenades de debug a ingestar (gpx)
  if (FALSE) { # simulació d'un sol participant
    library(gpx, warn.conflicts = FALSE)
    # fem un participant de prova que serà l'ultim de la llista
    resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.participants WHERE cursa_id = '", cursa_id, "' AND nom_cognoms  = 'Participant de mostra';"))  
    q_num_participant_prova <- dbSendQuery(connexio_bbdd, paste0("SELECT MAX(num)+1 AS num FROM logicursa.participants WHERE cursa_id = '", cursa_id, "';"))
    num_participant_prova <- dbFetch(q_num_participant_prova)$num
    dbClearResult(q_num_participant_prova)
    
    
    gpx_raw <- gpx::read_gpx(file = file.path(configuracio_processador$processador$DIRECTORI_BASE_CURSA, configuracio_cursa$fitxers_usuari$FITXER_PARTICIPANT_MOSTRA))
    
    # Preparem les posicions a carregar a l'instream com participant num_participant_prova
    gps_participant_mostra <- gpx_raw$tracks[[1]] %>% 
      as_tibble() %>%
      select(location_x = Longitude, location_y = Latitude, location_z = Elevation, in_stream_dt = Time) %>%
      mutate(cursa_id = cursa_id, participant_num = num_participant_prova )
    
    
    
    resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.in_stream_locations WHERE cursa_id = '", cursa_id, "' AND participant_num = ", num_participant_prova, ";"))
    dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"), gps_participant_mostra, append = TRUE, row.names = FALSE)
    
    participant_mostra <- tibble(cursa_id = cursa_id,
                                 num = num_participant_prova,
                                 nom_cognoms = "Participant de mostra",
                                 categoria = "MOSTRA")
    
    dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "participants"), participant_mostra, append = TRUE, row.names = FALSE)
    
    # dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "participants"))
    
    
  }
  

  if (TRUE) { # simulació del conjunt de posicions de participants
    
    # Participants
    taula_participants_carregada <- dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "participants"))
    localitzacions_participants <- read.csv2(file.path("..","simulador_r","carrera-noche-de-san-anton-jaen","dades_base","gps_members.csv")) %>% as_tibble()
    num_participants_simulats <- length(unique(localitzacions_participants$member_id))
    
    
    # Localitzacions
    # Només utilitzem les localitzacions de participants que tenim
    # a la taula de participants

    locs_a_carregar <- localitzacions_participants[localitzacions_participants$member_id %in% taula_participants_carregada$num,] %>%
      mutate(cursa_id = cursa_id, participant_num = member_id ) %>%
      select(cursa_id, participant_num, location_x = longitude, location_y = latitude, location_z = elevation, in_stream_dt = time)

    # min(locs_a_carregar$in_stream_dt)
    # max(locs_a_carregar$in_stream_dt)
    
    # per debug <<<< Només treballem amb un
    # locs_a_carregar <- locs_a_carregar %>% filter(participant_num %in% c(2,12))
    
    
    resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.in_stream_locations WHERE cursa_id = '", cursa_id, "';"))
    dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"), locs_a_carregar, append = TRUE, row.names = FALSE)
    
    # dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"))
    # unique(dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"))$participant_num)
      
  }
  
  
  # en cas de debug, necessitem una mesura del temps recorregut
  # la següent variable emmagatzem el temps en segons transcorreguts
  # des de l'inici del processament
  temps_debug <- as.numeric(NA)
  
}



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# DEFINICIO MODEL DE DADES ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_info("DEFINICIO MODEL DE DADES")


t_estat_participant <- R6Class("t_estat_participant", public=list(num=as.numeric(NA), 
                                                                nom_cognoms=as.character(NA), 
                                                                categoria=as.character(NA), 
                                                                posicions_ingestades=as.data.frame(NA),
                                                                posicions_a_cursa=as.data.frame(NA),
                                                                a_meta=as.logical(FALSE), # El corredor ha arribat a l'àmbit de meta?
                                                                a_meta_dt=as.character("00:00:00"), # Temps de carrera (HH:MM:SS)
                                                                info_derivada=list(),
                                                                darrer_refresc=as.character("00:00:00") # Instant de darrer refresc
                                                                ))  # Informació calculada per participant



t_punt_interes <- R6Class("t_punt_interes", public=list(num = as.numeric(NA), # número del punt d'interès
                                                        tipus = as.character(NA), # Tipus de punt d'interès (Només implementat "panoramica")
                                                        descripcio = as.character(NA),
                                                        index_graf = as.numeric(NA),
                                                        proxims_participants = as_tibble(NA),  # A quin punt del graf de la cursa està associat?
                                                        darrer_refresc=as.character("00:00:00")) # Instant de darrer refresc
)



t_estat_cursa <- R6Class("t_estat_cursa", public=list(cursa_id = as.character(NA),
                                                    recorregut = as.data.frame(NA),
                                                    ordre_punt_pre_meta = as.numeric(NA), # A partir de quin punt és meta
                                                    estat_punts_interes = list(), # Serà lista de t_punt_interès
                                                    estat_participants = list(), # Serà lista de t_participant
                                                    pila_posicions_pendents = stack,  # pila de dataframes amb les posicions ingestades i no processades
                                                    posicions_processades_num = as.numeric(NA),
                                                    darrera_posicio_ingestada = as.numeric(NA),
                                                    darrera_posicio_ingestada_dt = as.POSIXct(NA),
                                                    classificacio = as.data.frame(NA),
                                                    instant_inici = as.POSIXct(NA),
                                                    duracio_maxima_cursa = as.duration(NA),
                                                    instant_limit = as.POSIXct(NA),
                                                    temps_transcorregut = difftime(NA,NA, tz = "Europe/Madrid"),  # El repliquem aquí per que s'utilitza en molts càlculs
                                                    cursa_finalitzada = as.logical(NA)) ) # per extreure de la BBDD només les dades pendents




# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# DEFINICIO DE VARIABLES D'ESTAT  ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_info("DEFINICIO DE VARIABLES D'ESTAT")


# Magatzem de curses
estat_curses <- list()



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# INICIALITZACIÓ DE CURSA A SEGUIR  ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquesta versió només es processa una cursa per processador
# (la configurada a la BBDD)
log_info("Inicialització de la cursa a seguir")

# TODO: La inicialitzación hauria de fer-se mitjantçant un constructor
estat_curses[[cursa_id]] <- t_estat_cursa$new()
estat_curses[[cursa_id]]$pila_posicions_pendents <- stack$new()
estat_curses[[cursa_id]]$posicions_processades_num <- 0
estat_curses[[cursa_id]]$cursa_id <- cursa_id
estat_curses[[cursa_id]]$darrera_posicio_ingestada <- -1
estat_curses[[cursa_id]]$darrera_posicio_ingestada_dt <- ymd_hms("1997-12-31 00:00:00") # El -1 per dates

estat_curses[[cursa_id]]$duracio_maxima_cursa <- dhours(3)

# TODO: L'inici s'hauria d'establir a la primera arribada d'una localització
instant_inici <- force_tz(min(dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"))$in_stream_dt), tz = "Europe/Madrid")

estat_curses[[cursa_id]]$instant_inici <- instant_inici
estat_curses[[cursa_id]]$instant_limit <- estat_curses[[cursa_id]]$instant_inici + estat_curses[[cursa_id]]$duracio_maxima_cursa

estat_curses[[cursa_id]]$cursa_finalitzada <- FALSE

estat_curses[[cursa_id]]$recorregut <- dbGetQuery(connexio_bbdd, paste0("SELECT * FROM logicursa.recorreguts WHERE cursa_id='", cursa_id, "';"))

# establim el punt que representa l'àmbit de meta
temp_distancia_a_pre_meta <- max(estat_curses[[cursa_id]]$recorregut$distancia_desde_inici) - configuracio_cursa$configuracio_sistema$DISTANCIA_PRE_META
estat_curses[[cursa_id]]$ordre_punt_pre_meta <- min(estat_curses[[cursa_id]]$recorregut[estat_curses[[cursa_id]]$recorregut$distancia_desde_inici >= temp_distancia_a_pre_meta,]$ordre_punt)
rm(temp_distancia_a_pre_meta)


# inicialització dels participants d'aquesta cursa
participants_cursa_actual <- dbGetQuery(connexio_bbdd, paste0("SELECT * FROM logicursa.participants WHERE cursa_id='", cursa_id, "';"))
estructura_taula_ingestes <- dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "in_stream_locations"))[0,]

for (i in 1:nrow(participants_cursa_actual)) {
  participant_actual <- participants_cursa_actual[i,]
  estat_curses[[cursa_id]]$estat_participants[[participant_actual$num]] <- t_estat_participant$new()
  estat_curses[[cursa_id]]$estat_participants[[participant_actual$num]]$num <- participant_actual$num
  estat_curses[[cursa_id]]$estat_participants[[participant_actual$num]]$categoria <- participant_actual$categoria
  estat_curses[[cursa_id]]$estat_participants[[participant_actual$num]]$nom_cognoms <- participant_actual$nom_cognoms
  estat_curses[[cursa_id]]$estat_participants[[participant_actual$num]]$posicions_ingestades <- estructura_taula_ingestes
  
}

# inicialització dels punts d'interès

punts_interes_cursa_actual <- normalitza_tipus_query(dbGetQuery(connexio_bbdd, paste0("SELECT * FROM logicursa.punts_interes WHERE cursa_id='", cursa_id, "';")))

for (i in 1:nrow(punts_interes_cursa_actual)) {
  # i <- 1
  punt_interes_actual <- punts_interes_cursa_actual[i,]
  estat_curses[[cursa_id]]$estat_punts_interes[[punt_interes_actual$num]] <- t_punt_interes$new()
  estat_curses[[cursa_id]]$estat_punts_interes[[punt_interes_actual$num]]$index_graf <- punt_interes_actual$ordre_punt_recorregut[1]
  estat_curses[[cursa_id]]$estat_punts_interes[[punt_interes_actual$num]]$descripcio <- punt_interes_actual$descripcio
  estat_curses[[cursa_id]]$estat_punts_interes[[punt_interes_actual$num]]$num <- punt_interes_actual$num
  estat_curses[[cursa_id]]$estat_punts_interes[[punt_interes_actual$num]]$tipus <- punt_interes_actual$tipus
}


if (FALSE) {
  # debug
  
  estat_curses[[cursa_id]] 
  
}



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# ESBORRAT DE L'ESTAT ACTUAL -------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_info("ESBORRAT DE L'ESTAT ACTUAL")


# En aquesta secció esborrem els resultats del seguiment
# si he reiniciat, es calculen de nou al propi procés de recuperació

dbExecute(connexio_bbdd, "DELETE FROM logicursa.posicions_consumer_out;")


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# DEFINICIÓ DE FUNCIONS AUXILIARS -------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_info("DEFINICIÓ DE FUNCIONS AUXILIARS")


num_curses_actives <- function() {
  # Retorna el nombre de curses actives  
  num_curses <- 0
  
  for (i in 1:length(estat_curses)) {
    # i <-1
    if (!estat_curses[[i]]$cursa_finalitzada)
      num_curses <- num_curses + 1
    
  }
  
  num_curses
}

retorna_punts_propers_participant <- function(posicion_participant_a_avaluar, estat_cursa) {
  log_trace("retorna_punts_propers_participant: bucle:  ",num_pasada_bucle, " participant: ", posicion_participant_a_avaluar$participant_num[1])

  # De cara al càlcul de transicions observades
  # retornem les posicions probables al graf de la cursa durant el recorregut del participant
  
  # if (num_pasada_bucle == 161 && posicion_participant_a_avaluar$participant_num[1] == 12) {
  #   saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  #   saveRDS(posicion_participant_a_avaluar, "c:\\temp\\posicion_participant_a_avaluar.rds")
  #   save.image("c:\\temp\\recupera.RData")
  #   log_debug("CENTINELLA")
  #   
  #   stop()
  # }
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
  # posicion_participant_a_avaluar <- readRDS("c:\\temp\\posicion_participant_a_avaluar.rds")
  

  sp_participant <- SpatialPoints(as.matrix(posicion_participant_a_avaluar %>% select(location_x, location_y)), CRS(configuracio_cursa$dades_cursa$CRS))
  sp_participant <- st_as_sf(sp_participant)

  
  sp_cursa <- SpatialPoints(as.matrix(estat_cursa$recorregut %>% select(location_x, location_y)), CRS(configuracio_cursa$dades_cursa$CRS))
  sp_cursa <- st_as_sf(sp_cursa)
  
  distancies_a_cursa <- units::drop_units(st_distance(sp_participant, sp_cursa))
  
  METRES_CERCA_PUNTS <- 20
  indexos <- which(distancies_a_cursa < METRES_CERCA_PUNTS, arr.ind = TRUE)
  distancies <- distancies_a_cursa[cbind(indexos[, "row"], indexos[, "col"])]
  punts_propers_a_participant <- data.frame(index_participant = indexos[, "row"], index_graf_cursa = indexos[, "col"], distancia = distancies) %>% as_tibble()
  
  NUM_CANDIDATS_RETINDRE <- 20
  estats_candidants_per_posicio <- punts_propers_a_participant %>%
    arrange(index_participant, distancia) %>%
    group_by(index_participant) %>%
    slice_head(n=NUM_CANDIDATS_RETINDRE) %>%
    ungroup()
  
  # per depuració, afegim l'id de la ingesta
  estats_candidants_per_posicio <- estats_candidants_per_posicio %>%
    left_join(posicion_participant_a_avaluar %>% 
                rownames_to_column("index_participant") %>% 
                select(index_participant, id_ingesta = id) %>% 
                mutate(index_participant = as.numeric(index_participant)), 
              by = join_by(index_participant)) %>%
    select(id_ingesta, index_participant, index_graf_cursa, distancia)
  
  
  log_trace("retorna_punts_propers_participant - FI: bucle:  ",num_pasada_bucle, " participant: ", posicion_participant_a_avaluar$participant_num[1])
  return(estats_candidants_per_posicio)
}

# *************************************************************************************************************************************
actualitza_posicions_a_graf_de_cursa_simplificat <- function(estat_participant, estat_cursa) {
  log_trace("actualitza_posicions_a_graf_de_cursa_simplificat bucle:",num_pasada_bucle, " participant: ", estat_participant$num)
  
  
  # PER UNIT TEST
  # saveRDS(estat_participant, "c:\\temp\\estat_participant.rds")
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle == 161 && estat_participant$num == 15) {
  #   log_debug("CENTINELLA")
  #   stop() }
  # estat_participant <- readRDS("c:\\temp\\estat_participant.rds")
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
  # publica_informacio_cursa(estat_cursa)
  # stop()
  
  if (FALSE) {
    # unit test
   estat_participant$posicions_a_cursa
    
  }
  
  
  # Com a mínim necessitem X punts per avaluar la posició (poden quedar menys per no tenir alguns propers)
  PUNTS_AVALUAR <- 15
  
  # TODO: només hauriem de comptar amb les posicions que tinguin una distància
  # Avaluem només les **darreres posicions** ingestades del participant
  posicion_participant_a_avaluar <- estat_participant$posicions_ingestades %>%
    top_n(PUNTS_AVALUAR, wt = in_stream_dt)
  
  rownames(posicion_participant_a_avaluar) <- 1:nrow(posicion_participant_a_avaluar)
  
  log_trace(paste0("Avaluant ingesta de: ", min(posicion_participant_a_avaluar$id), " a: ", max(posicion_participant_a_avaluar$id)))
  
  
  if(nrow(posicion_participant_a_avaluar) >= PUNTS_AVALUAR) {
    # Tenim un mínim de punts a avaluar, podem

    # save.image("c:\\temp\\debug.Rimage")
    proximitats_recorregut_participant <- retorna_punts_propers_participant(posicion_participant_a_avaluar, estat_cursa)

    if (FALSE) { 
      
      punts_propers_dibuixar <- proximitats_recorregut_participant %>%
        distinct(index_graf_cursa) %>%
        left_join(estat_cursa$recorregut %>% select(ordre_punt, location_x, location_y), by = c("index_graf_cursa" = "ordre_punt"))
      
      leaflet() %>%
        addTiles(options = providerTileOptions(minZoom = 8, maxZoom = 50)) %>%
        addCircleMarkers(estat_cursa$recorregut$location_x, 
                         estat_cursa$recorregut$location_y,
                         radius = 0.1, color = "blue", opacity = 0.5) %>% # Punts de la cursa
        addCircleMarkers(punts_propers_dibuixar$location_x, 
                         punts_propers_dibuixar$location_y,
                         fill = FALSE, radius = 5, color = "green", opacity = 1) %>% # Punts de la cursa propers al participant
        addCircleMarkers(posicion_participant_a_avaluar$location_x, 
                         posicion_participant_a_avaluar$location_y,
                         radius = 0.2, color = "red", opacity = 1) %>% # Punts gps a avaluar
        setView(mean(posicion_participant_a_avaluar$location_x), mean(posicion_participant_a_avaluar$location_y), 18)
      # addPolylines(track_referencia_raw$location_x, track_referencia_raw$location_y) %>%
      # addCircleMarkers(track_referencia_raw$location_x, track_referencia_raw$location_y, radius = 5, opacity = 1, color = "blue", fill = TRUE, fillColor = "blue", fillOpacity = 1) %>%
      # addPolylines(track_referencia_master_interpolat_simplificat$location_x, track_referencia_master_interpolat_simplificat$location_y, color = "red", weight = 5, fillOpacity = 1  ) %>%
      # addAwesomeMarkers(pois$location_x, pois$location_y, icon = makeAwesomeIcon(icon = 'heart', markerColor = 'orange'))
    } # mapa de debug
    
    
    
    # Mirem si estem al principi de la cursa
    # si es així, només acceptem proximitats menors a la meitat de la cursa
    
    # Això ens permet establir una primer sequencia correcta a partir de la cual
    # extendre la resta de punts ingestats
    if (nrow(estat_participant$posicions_a_cursa) == 1) {
      # encara no tenim sequencia inicial
      meitat_cursa <- estat_cursa$recorregut$ordre_punt[floor(nrow(estat_cursa$recorregut)/2)]
      
      candidats_sobre_graf <- proximitats_recorregut_participant %>%
        filter(index_graf_cursa < meitat_cursa) %>% # fora els potencials llunyans
        filter(index_graf_cursa > lag(index_graf_cursa)) %>%
        group_by(id_ingesta) %>%
        summarise(index_cursa = max(index_graf_cursa), .groups = "drop") %>%
        left_join(posicion_participant_a_avaluar %>% select(id_ingesta = id, in_stream_dt),by = join_by(id_ingesta))
      
      # Primer bloc de posicions
      estat_participant$posicions_a_cursa <- candidats_sobre_graf
        
        
    } else {
      
      # darrera posició sobre el graf que s'ha obtingut
      darrer_index_cursa <- max(estat_participant$posicions_a_cursa$index_cursa)
      
      # quins poden ser els pròxims punts sobre el graf?
      candidats_sobre_graf <- proximitats_recorregut_participant %>%
        filter(index_graf_cursa >= darrer_index_cursa) %>% # No anem enrrere respecte el darrer detectat
        filter(abs(index_graf_cursa - darrer_index_cursa) < 60) %>% # No saltem més de 60 pasos # TODO: Parametritzar
        filter(index_graf_cursa > lag(index_graf_cursa)) # Cadascún dels candidats ha de ser posterior a l'anterior
      
      if(nrow(candidats_sobre_graf)>0) {
        # Després del filtratge, encara tenim més d'una posició candidata
        candidats_sobre_graf <- candidats_sobre_graf %>%
          group_by(id_ingesta) %>%
          summarise(index_cursa = max(index_graf_cursa), .groups = "drop") %>% # ens quedem amb la darrera candidata
          left_join(posicion_participant_a_avaluar %>% select(id_ingesta = id, in_stream_dt),by = join_by(id_ingesta))

        # afegim la nova candidata a la col·lecció de posicions sobre el graf que porta ja el participant
        estat_participant$posicions_a_cursa <- bind_rows(candidats_sobre_graf, estat_participant$posicions_a_cursa) %>%
          group_by(id_ingesta, in_stream_dt) %>%
          summarise(index_cursa = max(index_cursa), .groups = "drop")
      }
        

    }
      
    
        
    if (FALSE) { 
      
      punts_propers_dibuixar <- proximitats_recorregut_participant %>%
        distinct(index_graf_cursa) %>%
        left_join(estat_cursa$recorregut %>% select(ordre_punt, location_x, location_y), by = c("index_graf_cursa" = "ordre_punt"))
      
      leaflet() %>%
        addTiles(options = providerTileOptions(minZoom = 8, maxZoom = 50)) %>%
        addCircleMarkers(estat_cursa$recorregut$location_x, 
                         estat_cursa$recorregut$location_y,
                         radius = 0.1, color = "blue", opacity = 0.5) %>% # Punts de la cursa
        addCircleMarkers(punts_propers_dibuixar$location_x, 
                         punts_propers_dibuixar$location_y,
                         fill = FALSE, radius = 5, color = "green", opacity = 1) %>% # Punts de la cursa propers al participant
        addCircleMarkers(posicion_participant_a_avaluar$location_x, 
                         posicion_participant_a_avaluar$location_y,
                         radius = 0.2, color = "red", opacity = 1) %>% # Punts gps a avaluar
        setView(mean(posicion_participant_a_avaluar$location_x), mean(posicion_participant_a_avaluar$location_y), 18)
    } # mapa de debug
    
    
  }
  
  log_trace("actualitza_posicions_a_graf_de_cursa_simplificat - FI bucle:",num_pasada_bucle)
  
}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCIÓ D'INGESTA DE NOVES LOCALITZACIONS ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

carrega_noves_ingestes <- function(estat_cursa, limit_registres = NA) {
  log_trace("carrega_noves_ingestes bucle: ",num_pasada_bucle)
  # Carrega noves ingestes per la cursa pasada per paràmetre
  # limit_registres s'utilitza en casos de debug
  
  # PER UNIT TEST
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle >= 10) stop() else print(paste0("bucle: ",num_pasada_bucle))
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")

  
  # if (!is.na(limit_registres)) {
  #   txt_limit <- paste0(" LIMIT ", limit_registres, ";")
  # } else {
  #   txt_limit <- ";"
  # }
    
  if (FALSE){
    noves_ingestes <- dbGetQuery(connexio_bbdd, paste0("SELECT * FROM logicursa.in_stream_locations WHERE cursa_id='", estat_cursa$cursa_id, "';"))
  }
  
  noves_ingestes <- dbGetQuery(connexio_bbdd, paste0("SELECT * FROM logicursa.in_stream_locations WHERE cursa_id='", estat_cursa$cursa_id, "'",
                                   # "' AND id > ", estat_cursa$darrera_posicio_ingestada,
                                   " AND in_stream_dt > '",format(estat_cursa$darrera_posicio_ingestada_dt, format = "%Y-%m-%d %H:%M:%S"), "'",
                                   " AND in_stream_dt <= '",format(temps_actual, format = "%Y-%m-%d %H:%M:%S"), "'"))

  tbl_noves_ingestes <- normalitza_tipus_query(noves_ingestes)
  
  if (nrow(tbl_noves_ingestes)) {
    # Només afegim si tenim noves ingestes
    # Afegim les noves ingestes trobades a l'estat de la cursa
    # TODO: Hauria de ser una cua, no una pila
    #       L'impacte es petit per la ràpidesa del càlcul,
    #       Però conceptualment no és correcte
    estat_cursa$pila_posicions_pendents$push(tbl_noves_ingestes)
    # estat_cursa$darrera_posicio_ingestada <- max(tbl_noves_ingestes$id)
    estat_cursa$darrera_posicio_ingestada_dt <- max(tbl_noves_ingestes$in_stream_dt)
    
    ingestes_de_participants <- paste0(sort(unique(noves_ingestes$participant_num)), collapse = ", ")
    
    if (FENT_DEBUG) log_debug(paste0("Ingestades ", nrow(tbl_noves_ingestes), " coordenades per la cursa ", estat_cursa$cursa_id, ". Nou darrer Id ingestat: ", max(tbl_noves_ingestes$id), " Participants: ",ingestes_de_participants))
  }
  
  log_trace("carrega_noves_ingestes - FI bucle:",num_pasada_bucle)
}


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCIÓ D'ACTUALITZACIÓ DE PARTICIPANT ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquest apartat processem les ingestes pendents per 1 participant

actualitza_localitzacio_participant <- function(estat_participant, noves_posicions, estat_cursa) {
  log_trace("actualitza_localitzacio_participant bucle:",num_pasada_bucle, " participant: ", estat_participant$num, " noves coordenades: ", nrow(noves_posicions))
  
  # Donat un participant i noves posicions, actualitzem l'estat d'aquest
  
  # PER UNIT TEST
  # saveRDS(estat_participant, "c:\\temp\\estat_participant.rds")
  # saveRDS(noves_posicions, "c:\\temp\\noves_posicions.rds")
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle >= 12) stop() else print(paste0("bucle: ",num_pasada_bucle))
  # estat_participant <- readRDS("c:\\temp\\estat_participant.rds")
  # noves_posicions <- readRDS("c:\\temp\\noves_posicions.rds")
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
  
  
  
  amb_noves_posicions <- nrow(noves_posicions) > 0
  

  if (amb_noves_posicions > 0 & !estat_participant$a_meta) {
    # Tenim noves posicions, actualitzem, si toca, la posició sobre el graf de la cursa
    
    # Actualitzem les coordenades ja ingestades
    estat_participant$posicions_ingestades <- rbind(estat_participant$posicions_ingestades, noves_posicions)
    
    # si tenim noves posicions, hem d'actualitzar la posició del participant dintre del graf de la cursa
    actualitza_posicions_a_graf_de_cursa_simplificat(estat_participant, estat_cursa)

    # Avaluem si ha arribat a meta
    
    if (is.null(estat_participant$posicions_a_cursa$index_cursa)) {
      # No ha ingestat ni una coordenada
      estat_participant$a_meta <- FALSE
    } else {
      estat_participant$a_meta <- max(estat_participant$posicions_a_cursa$index_cursa) >= estat_cursa$ordre_punt_pre_meta
    }
    
    
    
    if (estat_participant$a_meta) {
      
      # fem snap a la darrera posició del recorregut i marquem temps d'arribada
      # saveRDS(estat_participant, "c:\\temp\\estat_participant.rds")
      # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
      # stop()
      # beepr::beep()
      # estat_participant <- readRDS("c:\\temp\\estat_participant.rds")
      # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
      # ... asignem el darrer punt del recorregut
      
      # Error in `bind_rows()`:
      # ! Can't combine `..1$a_meta_dt` <datetime<local>> and `..23$a_meta_dt` <duration<hours>>.
      estat_participant$posicions_a_cursa[nrow(estat_participant$posicions_a_cursa),]$index_cursa <- max(estat_cursa$recorregut$ordre_punt)
      estat_participant$a_meta_dt <- substr(as.character(as_hms(estat_participant$posicions_a_cursa[nrow(estat_participant$posicions_a_cursa),]$in_stream_dt - force_tz(estat_cursa$instant_inici,tz="UTC"))),1,8)
      
      
    } else {

      # Quina ha sigut la darrera coordenada del participant
      if (!is.na(estat_cursa$temps_transcorregut) & !is.na(estat_participant$posicions_a_cursa[nrow(estat_participant$posicions_a_cursa),])) {
        estat_participant$darrer_refresc <- substr(as.character(as_hms(estat_participant$posicions_a_cursa[nrow(estat_participant$posicions_a_cursa),]$in_stream_dt - force_tz(estat_cursa$instant_inici,tz="UTC"))),1,8)
      }

    }
    
  }
  
  
  
  if (FENT_DEBUG) {
    # print(estat_participant)
    # estat_participant$posicions_a_cursa
    # 
    # distancia_a_pre_meta <- max(estat_cursa$recorregut$distancia_desde_inici) - configuracio_cursa$configuracio_sistema$DISTANCIA_PRE_META
    # ordre_punt_pre_meta <- min(estat_cursa$recorregut[estat_cursa$recorregut$distancia_desde_inici >=x,]$ordre_punt)
    # 
    # View(estat_cursa$recorregut)
  }

  if (FALSE) {
    estat_cursa_pre <- estat_cursa
    estat_participant_pre <- estat_participant
    
    estat_cursa$estat_participants[[estat_participant$num]]$posicions_ingestades
    estat_cursa_pre$estat_participants[[estat_participant$num]]$posicions_ingestades
    
    
  }
  
  log_trace("actualitza_localitzacio_participant -- FI bucle:",num_pasada_bucle, " participant: ", estat_participant$num, " noves coordenades: ", nrow(noves_posicions))

}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# ACTUALITZACIÓ D'INFO DERIVADA DEL PARTICIPANT ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquesta funció fem càlculs lleuguers sobre l'estat del participant

actualitza_informacio_participant <- function(estat_participant, estat_cursa) {
  log_trace("actualitza_informacio_participant bucle:",num_pasada_bucle, " participant: ", estat_participant$num)
  # Donat un participant i noves posicions, actualitzem l'estat d'aquest
  
  # PER UNIT TEST
  # saveRDS(estat_participant, "c:\\temp\\estat_participant.rds")
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle > 10) stop() else print(paste0("bucle: ",num_pasada_bucle))
  # estat_participant <- readRDS("c:\\temp\\estat_participant.rds")
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")

  longitud_cursa <- max(estat_cursa$recorregut$distancia_desde_inici)
  
  if (nrow(estat_participant$posicions_a_cursa) > 1) {
    
    darrera_posicio_sobre_graf <- estat_participant$posicions_a_cursa[which.max(estat_participant$posicions_a_cursa$in_stream_dt),]
  
    estat_participant$info_derivada[["distancia_realitzada"]] <- estat_cursa$recorregut[darrera_posicio_sobre_graf$index_cursa,]$distancia_desde_inici
    estat_participant$info_derivada[["distancia_pendent"]] <- longitud_cursa -  estat_cursa$recorregut[darrera_posicio_sobre_graf$index_cursa,]$distancia_desde_inici
    
    # Velocitats mitjanes:
    # _total >>> des de l'inici de la cursa
    estat_participant$info_derivada[["velocitat_mitjana_kmh_total"]] <- (estat_participant$info_derivada[["distancia_realitzada"]]/1000) / as.numeric(estat_cursa$temps_transcorregut, units = "hours")
    
    # _5 >>> darrers 5 minuts de la cursa
    # estat_participant$info_derivada[["velocitat_mitjana_kmh_total"]] <- (estat_participant$info_derivada[["distancia_realitzada"]]/1000) / as.numeric(estat_cursa$temps_transcorregut, units = "hours")
  } else {
    estat_participant$info_derivada[["distancia_realitzada"]] <- 0
    estat_participant$info_derivada[["distancia_pendent"]] <- longitud_cursa
    estat_participant$info_derivada[["velocitat_mitjana_kmh_total"]] <- 0
  }
  
  if (DEBUG) {
    
    log_trace(paste0("Participant actualitzat: ", estat_participant$num,
                     " Distància: ", estat_participant$info_derivada[["distancia_realitzada"]], 
                     " D. pendent: ", estat_participant$info_derivada[["distancia_pendent"]], 
                     " Velocitat: ", estat_participant$info_derivada[["velocitat_mitjana_kmh_total"]]))
  }
    
    
    
  
  # print(estat_participant$info_derivada)
  log_trace("actualitza_informacio_participant -- FI bucle:",num_pasada_bucle, " participant: ", estat_participant$num)
  
}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCIÓ D'ACTUALITZACIÓ DE LA CLASSIFICACIO ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquesta funció fem l'actualització de la classificació de la cursa

actualitza_classificacio <- function(estat_cursa) {
  log_trace("actualitza_classificacio bucle:",num_pasada_bucle)
  # Donat un estat de cursa, actualitzem la classificació de la mateixa
  
  
  
  # PER UNIT TEST
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle > 161) stop() else print(paste0("bucle: ",num_pasada_bucle))
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")

  
  # Recollim la darrera posició de cada participant
  clasificacio_actual <- list()
  for (i in 1:length(estat_cursa$estat_participants)) {
    # i <- 23
    participant_actual <- estat_cursa$estat_participants[[i]]
    
    # a quin índex està aquest participant?
    if (is.na(participant_actual$posicions_a_cursa[1,1])) {
      # aquest participant encara no té cap posició en el graf de la cursa
      index_cursa_consumer <- 1
      index_cursa_primera_vegada_consumer <- as.POSIXct(strptime("2099-12-31 00:00:00", "%Y-%m-%d %H:%M:%S"))
    } else {
      index_cursa_consumer = max(participant_actual$posicions_a_cursa$index_cursa)
      index_cursa_primera_vegada_consumer <- max(participant_actual$posicions_a_cursa$in_stream_dt)
    }
    
    clasificacio_actual[[i+1]] <- tibble(index_cursa = index_cursa_consumer,
                                         index_cursa_primera_vegada = index_cursa_primera_vegada_consumer,
                                         nom = paste0("#", participant_actual$num, " - ", participant_actual$nom_cognoms),
                                         a_meta = participant_actual$a_meta,
                                         a_meta_dt = participant_actual$a_meta_dt,
                                         distancia_realitzada = participant_actual$info_derivada$distancia_realitzada,
                                         distancia_pendent = participant_actual$info_derivada$distancia_pendent,
                                         velocitat_mitjana_kmh_total = participant_actual$info_derivada$velocitat_mitjana_kmh_total,
                                         darrer_refresc = participant_actual$darrer_refresc)
    
  }
  clasificacio_actual <- bind_rows(clasificacio_actual) %>% arrange(desc(index_cursa),index_cursa_primera_vegada) %>% mutate(posicio_actual = 1:n())
  
  
  log_trace("actualitza_classificacio - FI bucle:",num_pasada_bucle)
  
  estat_cursa$classificacio <- clasificacio_actual
  
  
}



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCIÓ D'ACTUALITZACIÓ DE PARTICIPANT ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquesta funció fem càlculs lleuguers sobre l'estat d'un punt d'interès

actualitza_1_punt_interes <- function(index_punt_interes, estat_cursa) {
  log_trace("actualitza_punt_interes bucle:",num_pasada_bucle)
  # Donat un participant i noves posicions, actualitzem l'estat d'aquest
  
  # PER UNIT TEST
  # saveRDS(index_punt_interes, "c:\\temp\\index_punt_interes.rds")
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle >= 44 && estat_cursa$estat_punts_interes[[index_punt_interes]]$tipus == "panoramica") stop() else print(paste0("bucle: ",num_pasada_bucle))
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
  # index_punt_interes <- readRDS("c:\\temp\\index_punt_interes.rds")

  if (FALSE) {
    
    estat_cursa$estat_punts_interes[[index_punt_interes]]
    
    
  }
  
  
  if (estat_cursa$estat_punts_interes[[index_punt_interes]]$tipus == "panoramica" | estat_cursa$estat_punts_interes[[index_punt_interes]]$tipus == "arribada") {
    # només hi ha implementat el tipus panoramica o l'arribada

    # en quin node del graf de cursa hi és aquest punt d'interès?
    id_graf_pano_actual <- estat_cursa$estat_punts_interes[[index_punt_interes]]$index_graf
    dist_inici_pano_actual <- estat_cursa$recorregut[estat_cursa$recorregut$ordre_punt == id_graf_pano_actual, c("distancia_desde_inici")]

    proxims_participants <- estat_cursa$classificacio %>%
      filter(index_cursa < id_graf_pano_actual) # ens quedem amb els que encara no han passat
    
    if (nrow(proxims_participants)>0) {
      # si hi ha participants pendents de pasar, podem calcular coses
      proxims_participants <- proxims_participants %>% 
      mutate(distancia_a_punt_interes = dist_inici_pano_actual - distancia_realitzada) %>% # A quina distància està el corredor?
      mutate(distancia_desde_proxim = max(distancia_realitzada) - distancia_realitzada) %>%
      filter(distancia_a_punt_interes <= 500) # Només presentem si el pròxim està mínim a 500 metres del punt d'interès
      # filter(distancia_desde_proxim <= 500) # Agafem com a màxim aquells que disten menys de 500 metres del primer de la llista a passar
    } else {
      # ehomogeneitzem camps
      proxims_participants$distancia_a_punt_interes <- as.numeric(NA)
      proxims_participants$distancia_a_punt_interes <- as.numeric(NA)
    }

    estat_cursa$estat_punts_interes[[index_punt_interes]]$proxims_participants <- proxims_participants
    
    # Actualitzem l'hora de refesc
    #estat_cursa$estat_punts_interes[[index_punt_interes]]$darrer_refresc <- substr(as.character(as_hms(estat_cursa$darrera_posicio_ingestada_dt - force_tz(estat_cursa$instant_inici,tz="UTC"))),1,8)

  } else {
    # hem de tenir un dataframe, encara que sigui buit
    # això és per que es controla el número de files per presentar o no el punt d'interès
    estat_cursa$estat_punts_interes[[index_punt_interes]]$proxims_participants <- data.frame(sense_regs = as.numeric())
    
    
  }
  
  log_trace("actualitza_punt_interes - FI bucle:",num_pasada_bucle)
}


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCIÓ D'ACTUALITZACIÓ DE PUNTS D'INTERÈS ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquest apartat actualitzem l'estat dels punts d'interès

actualitza_punts_interes <- function(estat_cursa) {
  log_trace("actualitza_punts_interes bucle:",num_pasada_bucle)
  # I amb les posicions actualitzades, refem l'estat de punts d'interes
  
  # PER UNIT TEST
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle >= 24) stop() else log_debug(paste0("***************************************************** bucle: ",num_pasada_bucle))
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")

  
  for (i in 1:length(estat_cursa$estat_punts_interes)) {
    # i <- 1
    actualitza_1_punt_interes(i, estat_cursa)

  }
  
  if (FALSE) {
   #debug
    i <- 3
    estat_cursa$estat_punts_interes[[i]]$proxims_participants
    dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "estat_punts_interes_consumer_out"))
    
  }
  
  log_trace("actualitza_punts_interes - FI bucle:",num_pasada_bucle)
}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCIÓ D'ACTUALITZACIÓ DE CURSA ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquest apartat processem les ingestes pendents per 1 cursa


actualitza_estat_cursa <- function(estat_cursa) {
  log_trace("actualitza_estat_cursa bucle:",num_pasada_bucle)
  # actualitzem l'estat de la cursa segons les posicions pendents

  # PER UNIT TEST
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle >= 10) stop()
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")

  # Ha transcurregut el límit d'aquesta cursa?
  estat_cursa$cursa_finalitzada <-  temps_actual > estat_cursa$instant_limit
  if( estat_cursa$cursa_finalitzada ) log_info(paste0("Cursa ", estat_cursa$cursa_id, " finalitzada per arribar a límit de temps."))
    

  # Avaluem les ingestes pendents de processar
  
  while( estat_cursa$pila_posicions_pendents$size() > 0) {
    # Processem tota la pila de posicions pendents
    noves_posicions <- estat_cursa$pila_posicions_pendents$pop()
    
    # Recorrem els participants que apareixen a les dades i els actualitzem cadascún d'ells
    for (i in unique(noves_posicions$participant_num)) {
      # i <- 12
      actualitza_localitzacio_participant(estat_cursa$estat_participants[[i]], noves_posicions[noves_posicions$participant_num == i,], estat_cursa)
      actualitza_informacio_participant(estat_cursa$estat_participants[[i]], estat_cursa)
    }
    
    estat_cursa$posicions_processades_num <- estat_cursa$posicions_processades_num + nrow(noves_posicions)

  }
  
  # I actualitzem la classificació de la cursa amb les dades auxiliars
  estat_cursa$classificacio <- actualitza_classificacio(estat_cursa)
  
  # i actualitzem l'estat dels punts d'interès  
  actualitza_punts_interes(estat_cursa)

  estat_cursa$temps_transcorregut <- temps_actual - estat_cursa$instant_inici
  log_trace("actualitza_estat_cursa FI bucle:",num_pasada_bucle)

}


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# PUBLICACIÓ D'INFORMACIÓ DE LA CURSA ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# En aquesta funció publiquem la informació de la cursa al sistema

publica_informacio_cursa <- function(estat_cursa) {
  log_trace(paste0("publica_informacio_cursa bucle:",num_pasada_bucle))
  # Omple les taules de la BBDD que alimentará als clients
  
  # PER UNIT TEST
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (num_pasada_bucle == 10) stop()
  # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
  
  
  
  # Esborrem les taules de sortida de dades
  # ja que les refresquem cada vegada
  dbExecute(connexio_bbdd, "DELETE FROM logicursa.posicions_consumer_out;")
  dbExecute(connexio_bbdd, "DELETE FROM logicursa.estat_punts_interes_consumer_out;")
  dbExecute(connexio_bbdd, "DELETE FROM logicursa.clasificacio_consumer_out;")
  # dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "posicions_consumer_out"))
  
  
  
  # Recopilació de posicions a publicar -------------
  posicions_a_publicar <- list()
  
  for( i in 1:length(estat_cursa$estat_participants) ) {
    # per cada participant
    # i <- 2
    # estat_cursa$estat_participants[[i]]

    # if (!is.na(estat_cursa$estat_participants[[i]]$posicions_a_cursa)) {
    if (nrow(estat_cursa$estat_participants[[i]]$posicions_ingestades) > 0) {
      # Aquest participant ha tingut mínim una ingesta GPS

      darrera_posicio_ingestada <- estat_cursa$estat_participants[[i]]$posicions_ingestades[which.max(estat_cursa$estat_participants[[i]]$posicions_ingestades$in_stream_dt),]
      # View(estat_cursa$estat_participants[[i]]$posicions_ingestades)
      
      # print(estat_cursa$estat_participants[[i]])
      
      # al planol presentem la coordenada reportada GPS
      # tot i que els càlculs el fem amb la calculada sobre el graf
      posicions_a_publicar[[length(posicions_a_publicar)+1]] <- tibble(cursa_id = estat_cursa$cursa_id,
                                                                       participant_num = estat_cursa$estat_participants[[i]]$num,
                                                                       participant_identificacio = paste0("#",estat_cursa$estat_participants[[i]]$num," - ", estat_cursa$estat_participants[[i]]$nom_cognoms),
                                                                       tipus_posicio = 0, # tipus de punt estándar
                                                                       a_meta = estat_cursa$estat_participants[[i]]$a_meta,
                                                                       location_x = darrera_posicio_ingestada$location_x,
                                                                       location_y = darrera_posicio_ingestada$location_y)
                                                                       # location_z = darrera_posicio_ingestada$location_z)
      # as.data.frame(posicions_a_publicar[[length(posicions_a_publicar)]])
      
      if (FENT_DEBUG) {
        # Dibuixem també la posició al graf, per fer comprovacions

        # if (nrow(estat_cursa$estat_participants[[i]]$posicions_a_cursa) > 1) { # TODO: La primera pot ser un NA
        #   
        #   # Mínim ha d'haver-hi una possició deduïda
        #   
        #   darrera_posicio_graf <- estat_cursa$estat_participants[[i]]$posicions_a_cursa[which.max(estat_cursa$estat_participants[[i]]$posicions_a_cursa$in_stream_dt),]
        #   darrera_posicio_graf <- estat_cursa$recorregut[darrera_posicio_graf$index_cursa,]
        #   
        #   posicions_a_publicar[[length(posicions_a_publicar)+1]] <- tibble(cursa_id = estat_cursa$cursa_id,
        #                                                                    participant_num = estat_cursa$estat_participants[[i]]$num,
        #                                                                    participant_identificacio = paste0("#",estat_cursa$estat_participants[[i]]$num," - ", estat_cursa$estat_participants[[i]]$nom_cognoms),
        #                                                                    tipus_posicio = 1, # tipus de punt de comprovació sobre graf
        #                                                                    location_x = darrera_posicio_graf$location_x,
        #                                                                    location_y = darrera_posicio_graf$location_y)
        # }                                                               # location_z = darrera_posicio_graf$location_z)
      }
      
    }
  }
  posicions_a_publicar <- bind_rows(posicions_a_publicar)
  
  # Afegim les posicions a publicar ------------
  dbWriteTable(connexio_bbdd,
               Id(schema = "logicursa", table = "posicions_consumer_out"),
               posicions_a_publicar,
               append = TRUE)

  gravat <- nrow( dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "posicions_consumer_out")) )
  
  log_trace(paste0("Registres gravats: ",gravat))
  
  
  # Publicació de la classificació --------------
  
  taula_consumer_clasificacio <- estat_cursa$classificacio %>%
    left_join(estat_cursa$recorregut %>% filter(cursa_id == cursa_id) %>% select(index_cursa = ordre_punt, location_x, location_y),by = join_by(index_cursa)) %>%
    mutate(cursa_id = estat_cursa$cursa_id) %>%
    mutate(distancia_realitzada = floor(distancia_realitzada),
           distancia_pendent = floor(distancia_pendent),
           velocitat_mitjana_kmh_total = floor(velocitat_mitjana_kmh_total)) %>%
    select(cursa_id, posicio_actual, participant_identificacio = nom, a_meta, a_meta_dt, distancia_realitzada, distancia_pendent, 
           velocitat_mitjana_kmh_total, location_x, location_y, darrer_refresc)
  
  # # stop()
  # saveRDS(estat_cursa, "c:\\temp\\estat_cursa.rds")
  # if (any(taula_consumer_clasificacio$a_meta)) stop()
  
  # if (any(taula_consumer_clasificacio$a_meta)) {
  #   beepr::beep()
  #   # estat_cursa <- readRDS("c:\\temp\\estat_cursa.rds")
  #   str(dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "clasificacio_consumer_out")))
  # }

  dbWriteTable(connexio_bbdd,
               Id(schema = "logicursa", table = "clasificacio_consumer_out"),
               taula_consumer_clasificacio,
               append = TRUE) # hem fet un delete previ
  
  
  # Publicació de l'estat dels punts d'interès --------------
    # I amb les posicions actualitzades, refem l'estat de punts d'interes
  taula_consumer_punts_interes <- list()
  for (i in length(estat_cursa$estat_punts_interes):1) {
    # i <- 2
    if (estat_cursa$estat_punts_interes[[i]]$tipus == "panoramica" & nrow(estat_cursa$estat_punts_interes[[i]]$proxims_participants)>0) {
      
      # només tenim implementat la panoramica
      # només presentem si hi ha un o més participants a la "vista"
      
      taula_consumer_punts_interes[[length(taula_consumer_punts_interes)+1]] <- estat_cursa$estat_punts_interes[[i]]$proxims_participants %>%
        mutate(cursa_id = estat_cursa$cursa_id,
               punt_interes_num = estat_cursa$estat_punts_interes[[i]]$num,
               punt_interes_descripcio = paste0("#", punt_interes_num, " - ", estat_cursa$estat_punts_interes[[i]]$descripcio),
               distancia_a_punt_interes = floor(distancia_a_punt_interes)) %>%
        select(cursa_id, punt_interes_num, punt_interes_descripcio, participant_identificacio = nom, distancia_a_punt_interes, darrer_refresc)

      
    }
  }
  
  taula_consumer_punts_interes <- bind_rows(taula_consumer_punts_interes)
  
  dbWriteTable(connexio_bbdd,
               Id(schema = "logicursa", table = "estat_punts_interes_consumer_out"),
               taula_consumer_punts_interes,
               append = TRUE) # hem fet un delete previ
  
  log_trace(paste0("publica_informacio_cursa FI bucle:",num_pasada_bucle))
}



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# PROCÉS PRINCIPAL DEL PROCESSADOR ------------------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_debug("PROCÉS PRINCIPAL DEL PROCESSADOR")


# Aquest és el bucle principal del processador
# s'executa mentre hi hagi una cursa activa

curses_en_actiu <- num_curses_actives()
num_pasada_bucle <- 1
if (FENT_DEBUG) {
  # Si estem fent debug, el temps d'inici es el primer instant del participant 1
  temps_actual <- instant_inici
  delta_temps_debug <- 0
} else {
  temps_actual <- now()
}

while (curses_en_actiu > 0) {
  log_debug(paste0("Executant bucle principal, bucle: ", num_pasada_bucle,". Temps simulat: ", temps_actual))
  
  # En cada cop de bucle incorporem les coordenades pendents
  for (i in 1:length(estat_curses)) {
    # i <- 1
    # Si estem fent debug, limitem el ritme d'ingesta
    # if (FENT_DEBUG) temp_limit <- 50 else temp_limit <- NA
    carrega_noves_ingestes(estat_curses[[i]]) #,temp_limit)
  }
  
  
  # Després de carregades les coordenades pendents, les processem
  for (i in 1:length(estat_curses)) {
    # i <- 1
    actualitza_estat_cursa(estat_curses[[i]])
  }


  # Després d'actualitzades les curses, les presentem per consum
  for (i in 1:length(estat_curses)) {
    # i <- 1
    publica_informacio_cursa(estat_curses[[i]])
  }


  # Fem la pausa d'ingestes global

  if (FENT_DEBUG) { # Si estem fent debug, apliquem multiplicador
    Sys.sleep(configuracio_processador$processador$INTERVAL_INGESTA_BBDD / DEBUG_ESCALA_TEMPS)
    
    # Actualitzem el temps transcorregut
    delta_temps_debug <-  configuracio_processador$processador$INTERVAL_INGESTA_BBDD * DEBUG_ESCALA_TEMPS
    temps_actual <- temps_actual + dseconds(delta_temps_debug)
    
    # log_debug(paste0("Distancia participant mostra: ", estat_curses[[1]]$recorregut[estat_curses[[1]]$estat_participants[[num_participant_prova]]$ordre_punt_graf_actual,]$distancia_desde_inici))
    # log_debug(paste0("Coordenades ingestades participant 1: ", nrow(estat_curses[[1]]$estat_participants[[num_participant_prova]]$posicions_ingestades)))
    
    # estat_curses[[1]]$estat_participants[[num_participant_prova]]$posicions_ingestades
    
  }
  else {
    Sys.sleep(configuracio_processador$processador$INTERVAL_INGESTA_BBDD)
    temps_actual <- now()
  }
  num_pasada_bucle <- num_pasada_bucle + 1
  curses_en_actiu <- num_curses_actives()
}


# Tancament de la BBDD
log_info("Tancament de la BBDD")

dbDisconnect(connexio_bbdd)