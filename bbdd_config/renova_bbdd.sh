#!/bin/bash


LC_MESSAGES=English
export PGPASSWORD=uoc-tfg1

psql -U postgres -d postgres -p 5432 -f 0100_usuaris_bbbd_super.sql
psql -U logicursa_admin -d logicursa -p 5432 -f 0200_taules_i_permisos_admin.sql
