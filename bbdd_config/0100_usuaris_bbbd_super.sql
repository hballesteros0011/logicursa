DROP DATABASE IF EXISTS logicursa;

-- Usuari de gestió de la BBDD
DROP USER IF EXISTS logicursa_admin;
CREATE USER logicursa_admin WITH PASSWORD 'uoc-tfg1';

-- Usuari que representa un corredor generant informació
DROP USER IF EXISTS logicursa_producer;
CREATE USER logicursa_producer WITH PASSWORD 'uoc-tfg1' NOLOGIN;

-- Usuari per descarregar dades de la BBDD per part del processador
DROP USER IF EXISTS logicursa_processador;
CREATE USER logicursa_processador WITH PASSWORD 'uoc-tfg1' LOGIN;

-- Usuari que representa els seguidors de la cursa
DROP USER IF EXISTS logicursa_consumer;
CREATE USER logicursa_consumer WITH PASSWORD 'uoc-tfg1' NOLOGIN;

-- Configuació d'usuaris per PostgREST
-- Usuari amb el que es conecta PostgREST
DROP USER IF EXISTS authenticator;
CREATE ROLE authenticator NOINHERIT LOGIN PASSWORD 'uoc-tfg1';

-- En què es pot convertir 'authenticator'?
GRANT logicursa_consumer TO authenticator;
GRANT logicursa_producer TO authenticator;

-- Creació de la base de dades
CREATE DATABASE logicursa
    WITH
    OWNER = logicursa_admin
    ENCODING = 'UTF8'
--    LC_COLLATE = 'Spanish_Spain.1252'
--    LC_CTYPE = 'Spanish_Spain.1252'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1 -- TODO: A REVISAR
    IS_TEMPLATE = False;