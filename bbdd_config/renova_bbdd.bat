@echo off

echo "Paso 1/2"
call "00 psql super.bat" "0100_usuaris_bbbd_super.sql"

echo "Paso 2/2"
call "01 psql logicursa.bat" "0200_taules_i_permisos_admin.sql"

