@echo off
REM Copyright (c) 2012-2020, EnterpriseDB Corporation.  All rights reserved

SET LC_MESSAGES=English 

SET PGPASSWORD=uoc-tfg1

"C:\Program Files\PostgreSQL\15\bin\psql.exe" -h localhost -U postgres -d postgres -p 5432 -f %1

pause


