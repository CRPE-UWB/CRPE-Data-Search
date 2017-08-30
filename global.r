# List of functions we use
library(shiny)
library(shinyjs)
library(V8)
library(RMySQL)
library(DBI)
library(dplyr)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"



# Gets a Formmated String of Timestamp
getFormattedTime <- function() {
  format(Sys.time(), "%Y-%m-%d")
}

getTables <- function(){
  on.exit(dbDisconnect(con))
  con <- dbConnect(
    drv = dbDriver("MySQL"),    
    dbname = "Education",
    host = 'educationdata.csj8biafq77k.us-west-2.rds.amazonaws.com',
    port = 3306,
    user = "crpe",
    password = "!crpecrpe1")
  tbls <- dbListTables(con)
  return(tbls)
}

getDist <- function(st) {
  
  on.exit(dbDisconnect(con))
  con <- dbConnect(
    drv = dbDriver("MySQL"),
    dbname = "Education",
    host = 'educationdata.csj8biafq77k.us-west-2.rds.amazonaws.com',
    port = 3306,
    user = "crpe",
    password = "!crpecrpe1")
  n <- length(st)
  rs <- list()
  if (n > 0) {
    dists <- FIPST[st]
    for (i in 1:n) {
      q <- dbGetQuery(con, paste("select distinct Leaid, Leanm, Fipst from Schools where Fipst = ", dists[i], ";"))
      rs[[i]] <-  q
    }
    ds <- do.call(rbind, rs) 
    return(c("ALL", ds$Leanm))
  }
  else {
    return("Please select at least one state")
  }
}

getRates <- function(x) {
  con <- dbConnect(
    drv = dbDriver("MySQL"), 
    dbname = "Education",
    host = "educationdata.csj8biafq77k.us-west-2.rds.amazonaws.com",
    port = 3306,
    user = "crpe",
    password = "!crpecrpe1")  
  on.exit(dbDisconnect(con))
  d<-list()
  d$school<-dbGetQuery(con, paste("Select Ncessch as NCES_ID, Schnam as SCHOOL, 
                                  Report_Year as YEAR, All_Cohort as ENROLLED, All_Rate as GRAD_RATE 
                                  from GradRates where Ncessch =",x,"order by GradRates.Report_Year desc 
                                  limit 3;"))
  d$dist<-dbGetQuery(con, paste("Select DISTRICT, YEAR, TOT_FUNDING from (Select Ncessch, 
                                Finance.Leanm as DISTRICT, Finance.REPORT_YEAR as YEAR, TOTALREV as 
                                TOT_FUNDING from Finance join Schools on Finance.Leaid = Schools.Leaid 
                                where Ncessch =",x,") as Temp order by YEAR desc limit 3;"))

  return(d)
}

