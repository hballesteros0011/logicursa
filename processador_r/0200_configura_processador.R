# Aquest mòdul s'encarrega de les tasques de
# configuració de la cursa
# Alta del recorregut
# Alta dels participants
# Alta d'altres qüestions (punts destacats, avituallaments, etc.)

# Sempre començem amb una lleugera netaja de l'estat
rm(list=ls())

library(logger, warn.conflicts = FALSE)
# library(gpx, warn.conflicts = FALSE)
library(RPostgres, warn.conflicts = FALSE)
library(RcppTOML, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(readODS)
library(sp)
library(sf)

# source(file="0100_inicialitza_simulador.R")


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# INICIALITZACIÓ DEL MÒDUL ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
log_threshold(TRACE)

# llegim les configuracions
configuracio_processador <- RcppTOML::parseToml(file.path("configuracio.tom"))
configuracio_cursa <- RcppTOML::parseToml(file.path(configuracio_processador$processador$DIRECTORI_BASE_CURSA, configuracio_processador$processador$FITXER_CONFIGURACIO_CURSA))

# copiem a variables les constants més emprades

cursa_id <- configuracio_cursa$dades_cursa$identificador_cursa

# Funcions auxiliars
genera_confirmacio <- function(x, nom_fitxer_desti) {
  # Gravem la taula X a nom_fitxer_desti junt amb un missatge
  
  # fitxer_comprovacio <- file.path("confirmacio_carrega","participants_carregats.csv")
  if (file.exists(nom_fitxer_desti)) file.remove(nom_fitxer_desti)
  
  log_info(paste0("Gravats ", nrow(x), " registres a ", nom_fitxer_desti))
  write.csv2(x, nom_fitxer_desti)
  
}


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# GRABACIÓ DE LA CONFIGURACIÓ A LA BBBDD ---------------------
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Connexió a la BBDD de logicursa -----------------------
log_debug("Connexió a la BBDD de logicursa")

# Fem varies connexions per poder emprar send/fetch
connexio_bbdd <- dbConnect(drv=Postgres(), 
                           user="logicursa_processador", 
                           password="uoc-tfg1",
                           #host="localhost", 
                           host="192.168.1.200",
                           port=5432, 
                           dbname="logicursa",
                           bigint="numeric") # per evitar que emeti int64

# Esborrem dades prèvies a la BBDD ----------------
# amb això fem idempotent la configuració de la cursa
log_debug("Esborrem la cursa prèvia")

# atenció a l'ordre d'eliminació per respectar les claus forànes
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.posicions_consumer_out WHERE cursa_id = '", cursa_id, "';"))
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.in_stream_locations WHERE cursa_id = '", cursa_id, "';"))
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.participants WHERE cursa_id = '", cursa_id, "';"))
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.punts_interes WHERE cursa_id = '", cursa_id, "';"))
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.recorreguts WHERE cursa_id = '", cursa_id, "';"))
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.curses WHERE cursa_id = '", cursa_id, "';"))
resultat_sql <- dbExecute(connexio_bbdd, paste0("DELETE FROM logicursa.pot_de_comandes WHERE cursa_id = '", cursa_id, "';"))

# Emisió de les dades de la cursa ----------------

# ... Configuració de la cursa -----------------


# la estructura a satisfer és:
# 
# CREATE TABLE logicursa.curses (
#   id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
#   cursa_id CHARACTER(10), -- Identificador de la cursa
#   json_data text -- text en format json amb informació addicional
#   UNIQUE(cursa_id)
# );
log_info("Carregant la descripció de la cursa")

curses_raw <- tibble(cursa_id = cursa_id, json_data = NA)

dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "curses"), curses_raw, append = TRUE, row.names = FALSE)

genera_confirmacio(curses_raw, file.path("confirmacio_carrega","curses_carregades.csv"))

# dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "curses"))


# ... recorregut  -----------------

# la estructura a satisfer és:
# 
# CREATE TABLE logicursa.recorreguts (
#   id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
#   cursa_id CHARACTER(10), -- Identificador de la cursa
#   ordre_punt INTEGER,
#   location_x REAL,
#   location_y REAL,
#   location_z REAL,
#   json_data TEXT, -- text en format json amb la informació addicional a aquest punt
#   UNIQUE(cursa_id, ordre_punt)
# );
log_info("Carregant del recorregut de la cursa")

# afegim les coordenades de la cursa a la base de dades

recorregut_cursa_raw <- read_csv(file.path(configuracio_processador$processador$DIRECTORI_BASE_CURSA,configuracio_cursa$configuracio_sistema$FITXER_RECORREGUT), show_col_types = FALSE) %>%
  mutate(cursa_id = cursa_id)

sp_punts_cursa <- SpatialPoints(as.matrix(recorregut_cursa_raw %>% select(location_x, location_y)), CRS(configuracio_cursa$dades_cursa$CRS))
sp_punts_cursa <- st_as_sf(sp_punts_cursa)

distancies_entre_punts <- st_distance(sp_punts_cursa[2:nrow(sp_punts_cursa),], sp_punts_cursa[1:(nrow(sp_punts_cursa)-1),], by_element = TRUE)

recorregut_cursa_raw$distancia_desde_inici <- c(0,cumsum(distancies_entre_punts))

dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "recorreguts"), recorregut_cursa_raw, append = TRUE, row.names = FALSE)

genera_confirmacio(recorregut_cursa_raw, file.path("confirmacio_carrega","recorregut_cursa_carregada.csv"))


# ... participants -----------------
# la estructura a satisfer és:
# 
# CREATE TABLE logicursa.participants (
#   id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
#   cursa_id CHARACTER(10), -- Identificador de la cursa
#   participant_num smallint NOT NULL,
#   nom_cognoms TEXT,
#   categoria CHARACTER(10), -- Femenina? Masculina? Èlit? Amateur?
#     UNIQUE(cursa_id, participant_num) -- En una mateixa cursa no es permet més d'una categoría per participant
# );
log_info("Carregant els participants")

camps_existents <- dbListFields(connexio_bbdd, Id(schema = "logicursa", table = "participants"))

participants_cursa_raw <- read_ods(file.path(configuracio_processador$processador$DIRECTORI_BASE_CURSA,configuracio_cursa$fitxers_usuari$FITXER_BASE_PARTICIPANTS))  %>%
  mutate(cursa_id = cursa_id) %>%
  select(any_of(camps_existents)) # Seleccionem camps per si s'han afegit més

dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "participants"), participants_cursa_raw, append = TRUE, row.names = FALSE)

genera_confirmacio(participants_cursa_raw, file.path("confirmacio_carrega","participants_carregats.csv"))

# ... punts d'interès -----------------
# la estructura a satisfer és:
# 
# CREATE TABLE logicursa.punts_interes (
#   id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
#   cursa_id CHARACTER(10), -- Identificador de la cursa
#   num smallint NOT NULL,
#   location_x REAL,
#   location_y REAL,
#   tipus CHARACTER(128),
#   json_data text, -- text en format json amb la informació addicional a aquest punt
#   UNIQUE(cursa_id, num) -- En una mateixa cursa no es permet més d'una categoría per participant
# );
log_info("Carregant els punts d'interès")
camps_existents <- dbListFields(connexio_bbdd, Id(schema = "logicursa", table = "punts_interes"))

punts_interes_raw <- read_ods(file.path(configuracio_processador$processador$DIRECTORI_BASE_CURSA,configuracio_cursa$fitxers_usuari$FITXER_POIS)) %>%
  mutate(cursa_id = cursa_id) %>%
  select(any_of(camps_existents)) # Seleccionem camps per si s'han afegit més

# Associem el punt d'interès al punt que toqui de la cursa
# es suposa una gran precisió de la coordenada del punt d'interès, per
# tant es considera segur agar el punt de la cursa més proper


sp_punts_interes <- SpatialPoints(as.matrix(punts_interes_raw %>% select(location_x, location_y)), CRS(configuracio_cursa$dades_cursa$CRS))
sp_punts_interes <- st_as_sf(sp_punts_interes)

punts_interes_raw$ordre_punt_recorregut <-  recorregut_cursa_raw[st_nearest_feature(sp_punts_interes, sp_punts_cursa),]$ordre_punt

dbWriteTable(connexio_bbdd, Id(schema = "logicursa", table = "punts_interes"), punts_interes_raw, append = TRUE, row.names = FALSE)

genera_confirmacio(punts_interes_raw, file.path("confirmacio_carrega","punts_interes_carregats.csv"))


# dbReadTable(connexio_bbdd, Id(schema = "logicursa", table = "punts_interes"))
# ... altres paràmetres de la cursa ----------------


# tanquem la conexió i apasiau
dbDisconnect(connexio_bbdd)

