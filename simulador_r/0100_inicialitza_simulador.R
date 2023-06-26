# En aquest mòdul fem les inicialitzacions necessàries i comunes
# a la resta de mòduls



# Variables de la cursa simulada -------------------------------------------

directori_base_parametres <- file.path("carrera-noche-de-san-anton-jaen")
directori_origen_dades <- file.path(directori_base_parametres, "dades_base")
directori_desti_dades <- file.path(directori_base_parametres, "precalculs")

# llegim la configuració
configuracio_toml <- RcppTOML::parseToml(file.path(directori_base_parametres,"configuracio.tom"))

configuracio <- list(
  nombre_participants = configuracio_toml$simulacio$nombre_participants,
  identificador_cursa = configuracio_toml$simulacio$identificador_cursa,
  factor_velocitat = configuracio_toml$simulacio$factor_velocitat)



# Configuració del logging ----------------------------------

log_layout(layout_glue_colors)
log_threshold(TRACE)

# desactivat el tee per que enviar els controls de color a l'arxiu
# com es fa per que colori la consola i no l'arxiu?
# log_appender(appender_tee("Y:\\repo_tfg\\dades_mostra\\log_execucions.txt", append = TRUE))

# Atenció!! Permet enviar logs a Telegram!!!


# TOKEN DE PRODUCER general --------------------------------
# És el que s'utilitzarà per les tasques de gestió

# TODO: Després cada participant hauria de tenir el seu propi token?

# Token de seguretat pel client amb rol PRODUCER (gestionat per PostgREST)
TOKEN_BBDD_PRODUCER <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoibG9naWN1cnNhX3Byb2R1Y2VyIn0.Kqk-kbha-bH1P4jKfPCN8J1p3jpzZKSDdGxRbTaLiYI"

# Variables globals del simulador ----------------------------



# Funcions auxiliars ------------------------------------------

fes_pausa <- function() {
  cat("Prem una tecla per continuar")
  library = readLines(con = "stdin", 1)
}


conecta_a_bbdd <- function(usuari, clau_de_pas) {
  # usuari <- "logicursa_processador"
  # clau_de_pas <- "uoc-tfg1"
  connexio_bbdd <- dbConnect(drv=Postgres(), 
                             user=usuari, 
                             password=clau_de_pas,
                             host="localhost", 
                             port=5432, 
                             dbname="logicursa")
  connexio_bbdd
   
}
