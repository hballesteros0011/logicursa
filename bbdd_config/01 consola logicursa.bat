@echo off
REM Copyright (c) 2012-2020, EnterpriseDB Corporation.  All rights reserved

SET LC_MESSAGES=English 


SET PGPASSWORD=uoc-tfg1

"C:\Program Files\PostgreSQL\15\bin\psql.exe" -h localhost -U logicursa_admin -d logicursa -p 5432

pause


