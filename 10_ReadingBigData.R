## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------------------
library(RPostgreSQL)
library(DBI)
library(data.table)
library(JWileymisc)
library(keyring)

options(width = 70,
        stringsAsFactors = FALSE,
        datatable.print.nrows = 20, ## if over 20 rows
        datatable.print.topn = 3, ## print first and last 3
        digits = 2) ## reduce digits printed by R


## ----------------------------------------------------------------------------------------------------------------------------------
drv <- dbDriver("PostgreSQL")

conSuper <- dbConnect( drv,
                   dbname = "postgres",
                   host = "localhost",
                   port = 5432,
                   user = "postgres",
                   password = "advancedr"
                   )

dbGetInfo(conSuper)


## ----------------------------------------------------------------------------------------------------------------------------------
dbDisconnect(conSuper)
dbUnloadDriver(drv)

rm(conSuper)
rm(drv)



## ----------------------------------------------------------------------------------------------------------------------------------
aces_daily <- as.data.table(aces_daily)
ADDRESS <- aces_daily[, .(UserID)]
ADDRESS <- unique(ADDRESS)

set.seed(42020)
ADDRESS$PostalCode <- round(rnorm(nrow(ADDRESS), mean = 4000, sd = 300), 0)
colnames(ADDRESS) <- tolower(colnames(ADDRESS))


DEMOGRAPHICS <- aces_daily[, .(UserID, Age, BornAUS, SES_1, EDU)]
DEMOGRAPHICS <- unique(DEMOGRAPHICS)
DEMOGRAPHICS[, SurrogateID := seq_len(.N)]
DEMOGRAPHICS <- DEMOGRAPHICS[, .(UserID, SurrogateID, Age, BornAUS, SES_1, EDU)]
colnames(DEMOGRAPHICS) <- tolower(colnames(DEMOGRAPHICS))


THRICEDAILYMETRICS <- aces_daily[, .(UserID, SurveyInteger, SurveyStartTimec11,
                              STRESS, PosAff, NegAff)]
colnames(THRICEDAILYMETRICS) <- tolower(colnames(THRICEDAILYMETRICS))

DAILYMETRICS <- aces_daily[, .(UserID, SurveyDay, 
                              SOLs, WASONs, SUPPORT,
                              COPEPrb, COPEPrc, COPEExp, COPEDis)]
colnames(DAILYMETRICS) <- tolower(colnames(DAILYMETRICS))
rm(aces_daily)


## ----------------------------------------------------------------------------------------------------------------------------------
drv <- dbDriver("PostgreSQL")
conSuper <- dbConnect(drv, dbname = "postgres", host = "localhost",port = 5432,
                   user = "postgres", password = "advancedr" )



## ----------------------------------------------------------------------------------------------------------------------------------
dbWriteTable(conn = conSuper, name = "address", value = ADDRESS)
dbWriteTable(conn = conSuper, name = "demographics", value = DEMOGRAPHICS)
dbWriteTable(conn = conSuper, name = "thricedailymetrics", value = THRICEDAILYMETRICS)
dbWriteTable(conn = conSuper, name = "dailymetrics", value = DAILYMETRICS)
rm(ADDRESS, DEMOGRAPHICS, THRICEDAILYMETRICS, DAILYMETRICS)


## ----------------------------------------------------------------------------------------------------------------------------------
dbListTables(conSuper)


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conSuper, "CREATE USER gradstudentr WITH PASSWORD 's3cur3'")
dbGetQuery(conSuper, "CREATE USER gradstudenti WITH PASSWORD 'saf3'")
dbGetQuery(conSuper, "CREATE USER gradstudentu WITH PASSWORD 'sur3'")


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conSuper, "GRANT SELECT ON dailymetrics TO gradstudentr;")
dbGetQuery(conSuper, "GRANT INSERT ON address TO gradstudenti;")
dbGetQuery(conSuper, "GRANT SELECT, UPDATE ON address TO gradstudentu;")


## ----------------------------------------------------------------------------------------------------------------------------------
conRead <- dbConnect(drv, dbname = "postgres", host = "localhost",port = 5432,
                   user = "gradstudentr", password = "s3cur3" )


conInsert <- dbConnect(drv, dbname = "postgres", host = "localhost",port = 5432,
                   user = "gradstudenti", password = "saf3" )

conUpdate <- dbConnect(drv, dbname = "postgres", host = "localhost",port = 5432,
                   user = "gradstudentu", password = "sur3" )



## ----------------------------------------------------------------------------------------------------------------------------------
dbListTables(conRead)
dbListTables(conInsert)
dbListTables(conUpdate)


## ----------------------------------------------------------------------------------------------------------------------------------
dbRemoveTable(conRead, "address")
dbRemoveTable(conInsert, "address")
dbRemoveTable(conUpdate, "address")


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conRead, "SELECT * FROM demographics;") #does not work


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbGetQuery(conRead, "SELECT * FROM dailymetrics;")
head(query)


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conInsert, "SELECT * FROM address;")


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbGetQuery(conUpdate, "SELECT * FROM address;")
tail(query)


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conInsert, "INSERT INTO address VALUES (200, 200, 9999)")
query <- dbGetQuery(conSuper, "SELECT * FROM address")
tail(query)


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conUpdate, "SELECT * FROM address WHERE userid = 1")
dbGetQuery(conUpdate, "UPDATE address SET postalcode = 1111 WHERE userid = 1")
dbGetQuery(conUpdate, "SELECT * FROM address WHERE userid = 1")


## ----------------------------------------------------------------------------------------------------------------------------------
dbDisconnect(conRead)
dbDisconnect(conInsert)
dbDisconnect(conUpdate)
rm(conRead, conInsert, conUpdate)


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conSuper, "REVOKE SELECT ON dailymetrics FROM gradstudentr;")
dbGetQuery(conSuper, "REVOKE INSERT ON address FROM gradstudenti;")
dbGetQuery(conSuper, "REVOKE SELECT, UPDATE ON address FROM gradstudentu;")


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conSuper, "DROP USER gradstudentr")
dbGetQuery(conSuper, "DROP USER gradstudenti")
dbGetQuery(conSuper, "DROP USER gradstudentu")


## ----------------------------------------------------------------------------------------------------------------------------------
dbConnect(drv, dbname = "postgres", host = "localhost",port = 5432,
                   user = "gradstudentr", password = "s3cur3" )


## ----------------------------------------------------------------------------------------------------------------------------------
dbDisconnect(conSuper)
rm(conSuper)


## ----------------------------------------------------------------------------------------------------------------------------------
key_set_with_value(service = "advancedrBookSQLch10",
                   username = "postgres",
                   password = "advancedr")


## ----------------------------------------------------------------------------------------------------------------------------------
key_list()


## ----------------------------------------------------------------------------------------------------------------------------------
usrPassword <- key_get(service = "advancedrBookSQLch10",
                       username = "postgres")


## ----------------------------------------------------------------------------------------------------------------------------------
conSuper <- dbConnect(drv, dbname = "postgres", host = "localhost",port = 5432,
                   user = "postgres",
                   password = usrPassword )
rm(usrPassword)


## ----------------------------------------------------------------------------------------------------------------------------------
key_delete("advancedrBookSQLch10", "postgres")
key_list()


## ----------------------------------------------------------------------------------------------------------------------------------
dbListTables(conSuper)
dbListFields(conSuper, "thricedailymetrics")


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbReadTable(conSuper, "thricedailymetrics")
head(query)


## ----------------------------------------------------------------------------------------------------------------------------------
dbWriteTable(conn = conSuper, name = "iris", value = iris, overwrite = FALSE, append = FALSE)
dbWriteTable(conn = conSuper, name = "iris", value = iris, overwrite = FALSE, append = FALSE)


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbGetQuery(conSuper,
                    statement = "SELECT *
                                 FROM address")
query <- as.data.table(query)
head(query)


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbGetQuery(conSuper,
                    "SELECT postalcode
                     FROM address")
query <- as.data.table(query)
head(query)


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbGetQuery(conSuper,
                    "SELECT postalcode
                     FROM address
                     WHERE userid = 1")
query <- as.data.table(query)
head(query)


## ----------------------------------------------------------------------------------------------------------------------------------
query <- dbGetQuery(conSuper,
                    "SELECT userid, stress
                    FROM thricedailymetrics
                    WHERE surveyinteger >= 2")
query <- as.data.table(query)
head(query)


## ----------------------------------------------------------------------------------------------------------------------------------
BiggerData <- rnorm(200000000, 1)
BiggerData <- as.data.table(BiggerData)
dbWriteTable(conSuper, "biggerdata", BiggerData)
rm(BiggerData)
dbListTables(conSuper)


## ----------------------------------------------------------------------------------------------------------------------------------
dbRemoveTable(conSuper, "biggerdata")
dbListTables(conSuper)


## ----------------------------------------------------------------------------------------------------------------------------------
dbGetQuery(conSuper, "VACUUM FULL")


## ----------------------------------------------------------------------------------------------------------------------------------
dbRemoveTable(conSuper, "address")
dbRemoveTable(conSuper, "demographics")
dbRemoveTable(conSuper, "thricedailymetrics")
dbRemoveTable(conSuper, "dailymetrics")
dbRemoveTable(conSuper, "iris")
dbGetQuery(conSuper, "VACUUM FULL")


## ----------------------------------------------------------------------------------------------------------------------------------
dbDisconnect(conSuper)
dbUnloadDriver(drv)
rm(conSuper, drv, query)

