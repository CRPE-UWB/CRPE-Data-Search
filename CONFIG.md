DATABASE README

This database is a MySQL database hosted on AWS
The DB contains public education data sourced from various education data reporting agencies (NCES, OCR, etc)

To connect to the database for an interactive R session, copy and paste the following code into a .r file, and do your work within the function titled dbWork. The function "on.exit()" is only called when the code exists the function. If we do not do all of the work within the function, the databse connection will not be dropped for the entire time that we have the file open. Since the db only allows 16 simultaneous connections, that is very bad, and could result in the DB becoming inaccessible to others as well as the main web application.


library(RMySQL)
library(DBI)
library(dplyr) #if using filter/select/merge functions
dbWork <- function() {
	on.exit(dbDisconnect(con))
	con <- dbConnect(
      drv = dbDriver("MySQL"), 
      dbname = "Education",
      host = "educationdata.csj8biafq77k.us-west-2.rds.amazonaws.com",
      port = 3306,
      user = "crpe",
      password = "!crpecrpe1") 

      #insert code here
      # example:
      #t <- tbl(con, "Finance")
      #t <- t %>% filter(Leaid==1000005)
      #write.csv(t, file, row.names=FALSE)
      #etc, etc.

}

Use RazorSQL client to access and update the DB

Click in the left column, find databases, and click on "Education" to open up the table explorer for our DB

Right click on a table and select "import data" to add a new years data to that table

Ensure that the new data columns' ordering is correct and aligned with the existing data in the database

Make sure to highlight all numeric columns in excel and "format cells" to the appropriate data type. I.e. go to all integer columns, highlight and right click -> format cells -> number(in the left column) -> decimal places = 0. And for all columns that have a double/float type, highlight and right click -> format cells -> number(in the left column) -> decimal places = 5 (or however many is fitting for the use case)

SAVE after formatting the excel file, and upload by following the steps in the import data wizard.

To run simple queries within razorSQL, type the following within the query window:
(The green arrow pointing to the right in the panel just above the query window will execute the command where the cursor is, the downward pointing arrow will execute every command typed in the window even if separated by semicolons)
Use Education; #run once after connecting to DB initially to target the correct data schema
select * from Schools; #returns all records from Schools table
select Ncessch, Schanm, Chartr from Schools; #returns just the ncesid, school name, and whether or not its a charter (from the schools table)
select * from Schools join GradRates on Schools.Ncessch = GradRatesNcessch; #returns every grad rates record with the extra school information appended to each row of data
select * from GradRates where Ncessch = 100005; #get gradrates for albertville alabama schools
selct count(*) from GradRates where year = 2016; #good way to check if the number of records uploaded from a new year of data matches what is in the .csv file





