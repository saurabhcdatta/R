########################################
# 0. Get latest Insured Share Growth from Server
# Updated: November 18, 2026
########################################

library(DBI)
library(odbc)

options(scipen = 999)

setwd("S:/Committees and Task Force/NCUSIF Investment/SIF Model 2026/OMB_Nov_2026/ISG_Model_New/Data")

# Connect to the database
con <- dbConnect(odbc(), dsn = "ReportDatabase", timeout = 10)

# Insured Deposits Quarterly (CU_TYPE 1 and 2 only)
# Join, filter and sum all run on the server
qry <- "
SELECT f.cycle_date,
  COUNT(*) AS n,
  SUM(CAST(f.ACCT_069A AS FLOAT))
    AS ins_sh
FROM FS220B f
JOIN (
  SELECT DISTINCT JOIN_NUMBER, CU_TYPE
  FROM CU030
  WHERE CU_TYPE IN ('1','2')
) t ON f.join_number = t.JOIN_NUMBER
GROUP BY f.cycle_date
ORDER BY f.cycle_date"

is_table_fs220b <- dbGetQuery(con, qry)

# Print all quarters and check the latest n
print(is_table_fs220b)

# Optional check: any CU listed in CU030 as both
# type 1 and type 2? (0 = no double counting)
chk_type <- dbGetQuery(con, "
SELECT JOIN_NUMBER
FROM CU030
WHERE CU_TYPE IN ('1','2')
GROUP BY JOIN_NUMBER
HAVING COUNT(DISTINCT CU_TYPE) > 1")

print(nrow(chk_type))

write.csv(is_table_fs220b, file = 'Insured_Deposits_2026Q3.csv')

shell.exec('Insured_Deposits_2026Q3.csv')

# Disconnect from the database
dbDisconnect(con)
