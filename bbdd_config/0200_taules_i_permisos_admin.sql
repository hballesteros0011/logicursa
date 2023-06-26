DROP SCHEMA IF EXISTS logicursa CASCADE;

-- Schema on deixarem tot l'entorn de logicursa
CREATE SCHEMA logicursa;

-- Taula de comunicació amb el processador
CREATE TABLE logicursa.pot_de_comandes (
	id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa afectada
	desinatari smallint NOT NULL, -- "0" Missatge pel processador de cursa, "1" Missatge pel processador de preparació
	texte_comanda CHARACTER(128) -- Literal de la comanda a interpretar, verb+paràmetres
);

-- Taula on desar informació sobre cada cursa
CREATE TABLE logicursa.curses (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
	json_data TEXT, -- text en format json amb informació addicional
	UNIQUE(cursa_id) -- No es poden duplicar els identificadors
);

-- Taula on desar els recorreguts teòrics de les curses
CREATE TABLE logicursa.recorreguts (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
    ordre_punt INTEGER, -- Ordre d'aquest punt dintre del recorregut de la cursa, atenció: sempre 1..n ordenats!!!
	location_x REAL, -- coordenades
    location_y REAL, -- coordenades
    location_z REAL, -- coordenades
	distancia_desde_inici REAL, -- Distància d'aquest punt respecte l'inici de la cursa
	UNIQUE(cursa_id, ordre_punt), -- Una cursa no pot tenir ordres duplicats
	UNIQUE(cursa_id, location_x, location_y) -- Una cursa no pot tenir coordenades duplicades, obviem Z per simplicitat
);

-- Taula on desar els participants de la cursa
CREATE TABLE logicursa.participants (
	id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
	categoria CHARACTER(10), -- Femenina? Masculina? Èlit? Amateur?
    num INTEGER NOT NULL, -- Número de participant ("Dorsal")
	nom_cognoms TEXT, -- Noms i cognoms del participant
	UNIQUE(cursa_id, num) -- En una mateixa cursa es permet més d'una inscripció per participant
);

-- Taula on desar els punts d'interès de la cursa
CREATE TABLE logicursa.punts_interes (
	id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
	num INTEGER NOT NULL, -- Número de punt d'interès
	descripcio TEXT, -- text lliure de descripcio del punt d'interès
	location_x REAL, -- coordenades
    location_y REAL, -- coordenades
	tipus CHARACTER(128), -- Tipus de punt (implementat "panoramica")
	ordre_punt_recorregut INTEGER, -- A quin punt de la cursa s'associa? Omplert pel processador
	json_data text, -- text en format json amb la informació addicional a aquest punt
	UNIQUE(cursa_id, num) -- En una mateixa cursa no es permet més d'una categoría per participant
);

-- Taula de recepció de les posicions per part dels participants
CREATE TABLE logicursa.in_stream_locations (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	in_stream_dt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, -- instant d'ingesta a la BBDD (auto)
	cursa_id CHARACTER(10), -- Identificador de la cursa
	participant_num INTEGER, -- Número de participant ("Dorsal")
	location_x REAL, --coordenades
    location_y REAL, --coordenades
    location_z REAL --coordenades
);
	--json_data text -- text en format json amb informació addicional i opcional


-- Taula de consum de posicions de participants de les apps de seguiment
CREATE TABLE logicursa.posicions_consumer_out (
	id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
	participant_num INTEGER, -- Número de participant ("Dorsal")
	participant_identificacio TEXT, -- Noms i cognoms del participant, precedits pel número de dorsal
	tipus_posicio INTEGER DEFAULT 0,  -- 0 si es una posició del  GPS, 1 si és sobre el graf de cursa.
	a_meta BOOLEAN, -- ja ha arribat a meta?
    location_x REAL, --coordenades
    location_y REAL --coordenades
);

-- Taula de consum de clasificacio de les apps de seguiment
CREATE TABLE logicursa.clasificacio_consumer_out (
	id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
	posicio_actual INTEGER,
	location_x REAL, --coordenades on està actualment (per fer zoom a l'app)
    location_y REAL, --coordenades on està actualment (per fer zoom a l'app)
	a_meta BOOLEAN, -- ja ha arribat a meta?
	a_meta_dt CHARACTER(8), -- instant d'arribada a meta en HH:MM:SS
	participant_identificacio TEXT, -- Noms i cognoms del participant, precedits pel número de dorsal
	distancia_realitzada INTEGER, -- Distància ja recorreguda, enter per simplicitat
	distancia_pendent INTEGER,-- Distància fins a l'arribada, enter per simplicitat
	velocitat_mitjana_kmh_total INTEGER, -- Velocitat mitjana des de l'inici, enter per simplicitat
	darrer_refresc CHARACTER(8) -- instant de darrer refresc en HH:MM:SS
);

-- Taula de consum de l'estat dels punts d'interès de les apps de seguiment (un reg. per participant)
CREATE TABLE logicursa.estat_punts_interes_consumer_out (
	id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	cursa_id CHARACTER(10), -- Identificador de la cursa
	punt_interes_num INTEGER, -- Identificació del punt d'interès
	punt_interes_descripcio TEXT, -- Identificació del punt d'interès
	participant_identificacio TEXT, -- Noms i cognoms del participant, precedits pel número de dorsal
	distancia_a_punt_interes INTEGER, -- Distància del participant a aquest punt d'interès
	darrer_refresc CHARACTER(8) -- instant de darrer refresc en HH:MM:SS
);


-- -- LES ACTIVEM MÉS ENDAVANT
--ALTER TABLE ONLY logicursa.members ADD CONSTRAINT members_pkey PRIMARY KEY (member_id);
--ALTER TABLE ONLY logicursa.recorreguts ADD CONSTRAINT fk_cursa FOREIGN KEY (cursa_id) REFERENCES logicursa.curses(cursa_id);
--ALTER TABLE ONLY logicursa.in_stream_locations ADD CONSTRAINT fk_member FOREIGN KEY (member_id) REFERENCES logicursa.members(member_id);

GRANT USAGE ON SCHEMA logicursa TO logicursa_consumer; -- Espectadors
GRANT USAGE ON SCHEMA logicursa TO logicursa_producer; -- Participants
GRANT USAGE ON SCHEMA logicursa TO logicursa_processador; -- Processador de dades

-- Permisos de gestió de taules pel processador
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA logicursa TO logicursa_processador;

-- Permisos pels consumidors
GRANT SELECT ON TABLE logicursa.posicions_consumer_out TO logicursa_consumer;
GRANT SELECT ON TABLE logicursa.estat_punts_interes_consumer_out TO logicursa_consumer;
GRANT SELECT ON TABLE logicursa.punts_interes TO logicursa_consumer;
GRANT SELECT ON TABLE logicursa.recorreguts TO logicursa_consumer;
GRANT SELECT ON TABLE logicursa.clasificacio_consumer_out TO logicursa_consumer;

GRANT INSERT ON TABLE logicursa.in_stream_locations TO logicursa_producer;

