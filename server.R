##################################################################################################
#             OWNER: C.R.P.E.
##################################################################################################
#             Developed by Simon Classon and Kevin Cha
##################################################################################################
#             V1.01
##################################################################################################
#             August 22, 2017
##################################################################################################
library(shiny) # Need to Run Shiny App
library(shinyjs) # Need to Make Fields Mandatory/Optional and Do Other Cool Stuff
library(DBI) # Need to Query with Database
library(DT) # Need to Download Filtered DataTable
library(RMySQL)
library(V8)
library(dplyr)
library(leaflet)
library(RJSONIO)
library(htmltools)
source('scripts/list.r')
source('scripts/global.r')  

## Backend of the Shiny App -----------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # Renders the data table based on the inputs from the side panel
  dataTable <- function() {
    
    #require these, or skip over this code
    req(input$dataset, input$state, input$submitBttn)
    
    #setting up easier names to use
    dat <- input$dataset
    sta <- input$state
    dis <- input$district
    
    #establish db connection
    con <- dbConnect(
      drv = dbDriver("MySQL"), 
      dbname = "Education",
      host = "educationdata.csj8biafq77k.us-west-2.rds.amazonaws.com",
      port = 3306,
      user = "crpe",
      password = "!crpecrpe1")  
    
    #Disconnects from the Database Once User Done using App 
    on.exit(dbDisconnect(con))

    #conditional branching to decide which dataset join to construct
    if(dat%in%c("GradRates", "OCR")) {
      if ("ALL" %in% dis) {
        n <- length(sta)
        tbs <- list()
        for (i in 1:n) {
          #'lock' these inputs while building the query string
          isolate({input$dataset})
          isolate({input$district})
          isolate({input$state})
          #build the query
          query <- paste("Select * from Schools join ", dat, " on Schools.Ncessch = ", dat, 
                         ".Ncessch where Schools.Fipst = ", FIPST[sta[i]], ";", sep = "")
          #t equals the result set retrieved from con with the query
          t <- dbGetQuery(con, query) 
          tbs[[i]] <- t
        }
        #if multiple resultsets retrieved over multiple states/districts, call rbind on them
        return(do.call(rbind, tbs))
      }
      else {
        n <- length(dis)
        tbs <- list()
        for (i in 1:n) {
          isolate({input$dataset})
          isolate({input$district})
          isolate({input$state})
          query <- paste("Select * from Schools join ", dat, " on Schools.Ncessch = ", dat, 
                         ".Ncessch where Schools.Leanm = '", dis[i], "';", sep = "")
          t <- dbGetQuery(con, query) 
          tbs[[i]] <- t
        }
        return(do.call(rbind, tbs))      
      }
    }
    
    else if (dat == "StatePerformance"){
      n <- length(sta)
      tbs <- list()
      for (i in 1:n) {
        isolate({input$dataset})
        isolate({input$state})
        query <- paste("Select * from ", sta[i], "Performance;", sep="")
        t <- dbGetQuery(con, query) 
        tbs[[i]] <- t
        return(do.call(rbind, tbs))
      }
    }
  
    else {
      if ("ALL" %in% dis) {
        n <- length(sta)
        tbs <- list()
        for (i in 1:n) {
          isolate({input$dataset})
          isolate({input$district})
          isolate({input$state})
          query <- paste("Select * from ", dat, " where Fipst = ", FIPST[sta[i]], ";")
          t <- dbGetQuery(con, query) 
          tbs[[i]] <- t       
        }
        return(do.call(rbind, tbs))
      }
      else {
        numDis <- length(dis)
        tbs <- list()
        for (i in 1:numDis) {
          isolate({input$dataset})
          isolate({input$district})
          isolate({input$state})
          query <- paste("Select * from ", dat, " where Leanm = '", dis[i], "';", sep="")
          t <- dbGetQuery(con, query) 
          tbs[[i]] <- t        
        }
        return(do.call(rbind, tbs)) 
      }
    }
  }
  
  table<-reactiveValues()
  table$tableExtension<-c("Responsive", "Buttons")
  table$init<-JS(paste('function(setting, json) { alert("access extra columns by', 
                       'clicking the green plus sign on the left of each row of data"); }'))
  observe({
    if(!input$responsive) {
      table$tableExtension<-c("Buttons", "Responsive")
      table$init<-JS(paste('function(setting, json) { alert("access extra columns by', 
                           'clicking the green plus sign on the left of each row of data"); }'))
      table$init<-JS(paste('function(setting, json) { alert("access extra columns by', 
                                 'clicking the green plus sign on the left of each row of data"); }'))
    }
    else{
      table$tableExtension<-c("Buttons")
      table$init<-NULL
    }
  })
  
  output$myTable <- DT::renderDataTable({
    DT::datatable(dataTable(),
                  extensions = table$tableExtension,
                  class = "stripe compact",
                  options = list(deferRender = TRUE,
                                 dom = 'Brtip', #each letter corresponds to a code for options i.e. t=table, B=buttons
                                 class = 'display',
                                 buttons = I('colvis'),
                                 initComplete = table$init,
                                 pageLength = 20,
                                 rowReorder = TRUE,
                                 columnDefs = list(list(
                                   targets = c(1:20),
                                   render = JS(
                                     "function(data, type, row, meta) {",
                                     "return type == 'display' && data.length > 12 ?",
                                     "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                     "}")
                                 ))
                  ),
                  filter = list(position = "top", clear = FALSE, plain = TRUE),
                  rownames = FALSE
    )})
  
  #stuff
  output$caption <- renderText(paste(input$dataset))
  output$caption1 <- renderText(paste(input$state))
  output$caption2 <- renderText(paste(input$district))

  # store a value to change it to true once datatable loads
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  
  # simulate data load for loading screen
  observe({

    if(input$submitBttn){
      
      df <- data.frame(
                       val = rnorm(200, 0, 1))

      ## Simulate the data load
      Sys.sleep(5)
      # set my condition to TRUE
      rv$setupComplete <- TRUE
    }

  ## the conditional panel reads this output
  output$setupComplete <- reactive({
    return(rv$setupComplete)
  })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  })
  
  #update the selectable options for subsequent drop-down menus after 
  #dataset has been selected
  observe({
    if (substr(input$dataset, 1, 5) == "State") {
      updateSelectInput(session, "district", choices = "ALL", selected ="ALL")
    }
    else {
      updateSelectInput(session, "district", choices = c("ALL", getDist(input$state)), selected ="ALL")
    }
  })
  #api key for use in Google Place data AIzaSyBD0L32-9x86RLaMX0uYFy5GT80bFbSni4
  
  output$aMap <- renderLeaflet({
    l <- leaflet() %>% addTiles(group = "Base") %>% addPopups(lat=44.5802, lng=-103.4617, popup="Perform a datasearch on the Schools table to see them mapped. Select just a single district to see public resource data as well.") %>% setView(zoom=4,lat=44.5802, lng=-103.4617)
  })

  output$myMap <- renderLeaflet({
  #establish connection variable
    con <- dbConnect(
      drv = dbDriver("MySQL"), 
      dbname = "Education",
      host = "educationdata.csj8biafq77k.us-west-2.rds.amazonaws.com",
      port = 3306,
      user = "crpe",
      password = "!crpecrpe1")  
    on.exit(dbDisconnect(con))
    #Get the data needed for the map
    d<-dataTable()[dataTable()$Chartr!=1, c("Ncessch", "Schnam", "Buildinglat", "Buildinglon", 
                                             "Level", "Chartr")]
    c<-dataTable()[dataTable()$Chartr==1, c("Ncessch", "Schnam", "Buildinglat", "Buildinglon", 
                                            "Level", "Chartr")]

    #build the SpatialPointsDataFrame for use in the map (requires matrix of just coords(xy), 
    #plus the matrix(mapData) of associated data)
    dxy <-data.frame(lat=d$Buildinglat, lon=d$Buildinglon)
    cxy <-data.frame(lat=c$Buildinglat, lon=c$Buildinglon)
    mapData<- sp::SpatialPointsDataFrame(coords=dxy, data= d)
    charterData <-sp::SpatialPointsDataFrame(coords=cxy, data= c)

    #gets the appropriate color for the marker based on School Level (elementary, jr high, high, n/a)
    getColor <- function(mdata) {
      sapply(mdata$Level, function(Level) {
        if(Level >= 4) {
          "red"
        } else if(Level >= 3) {
          "orange"
        } else if(Level >= 2) {
          "purple"
        }
        else if(Level>=1) {
          "green"
        }
        else {
          "black"
        }})
    }
    
    #assign an icon to each row of data based on School-Level data
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(mapData)
    )
    
    #make a leaflet from mapData, add tiles (flat street data) to base group, add markers for schools
    l <- leaflet() %>% addTiles(group = "Base") %>% 
      addAwesomeMarkers(lng=mapData$Buildinglon,
                 lat=mapData$Buildinglat,
                 label=mapData$Schnam,
                 layerId=mapData$Ncessch,
                 group="NOT_CHARTER", 
                 icon=icons,
                 clusterOptions = markerClusterOptions()
      ) %>% 
      addAwesomeMarkers(lng=charterData$Buildinglon,
                        lat=charterData$Buildinglat,
                        label=charterData$Schnam,
                        layerId=charterData$Ncessch,
                        group="CHARTER", 
                        icon=icons,
                        clusterOptions = markerClusterOptions()
      )
      
    #if only looking at one district, add public resource data (libraries, police, rec centers etc.)
    if (length(input$district)==1&&input$district[1]!="ALL") {
      park<-addUtils("park")
      po<-addUtils("police")
      lib<-addUtils("library")
      fire<-addUtils("fire_station")
      rec<-addUtils("rec_center")
      l <- l %>% addCircles(lng=park$lg, lat=park$lt, label=park$nm, group = "Park", color="green") %>% 
        addCircles(lng=lib$lg, lat=lib$lt, label=lib$nm, group = "Library", color="gold") %>% 
        addCircles(lng=po$lg, lat=po$lt, label=po$nm, group = "Police", color="blue") %>% 
        addCircles(lng=fire$lg, lat=fire$lt, label=fire$nm, group = "Fire Station", color="red") %>% 
        addCircles(lng=rec$lg, lat=rec$lt, label=rec$nm, group="Recreation Center", color="purple") %>% 
        addLayersControl(
          baseGroups = "Base",
          overlayGroups = c("Park", "Library", "Police", "Fire Station", "Recreation Center", 
                            "CHARTER", "NOT_CHARTER"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% hideGroup(c("Park", "Library", "Police", "Fire Station", "Recreation Center", 
                          "CHARTER", "NOT_CHARTER"))
    }
    else {
      l <- l %>% addLayersControl(
        baseGroups = "Base",
        overlayGroups = c("CHARTER", "NOT_CHARTER"),
        options = layersControlOptions(collapsed = FALSE) 
      ) %>% hideGroup(c("CHARTER", "NOT_CHARTER"))
    }
    l %>% addScaleBar(position = c("bottomleft")) %>% addLegend(position='bottomright', colors=c("green", "purple", "orange", "red", "black"), labels=c("elementary", "middle", "high", "other", "not reported"))
  })
  
  
  
  #if park data requested, make it and then pass the data back to renderLeaflet
  addUtils <- function(util) {
    #need for input/output of json notation
    require(RJSONIO)
    #make a point (lat,lng) for use in api request
    centre <- paste(mean(dataTable()[input$myTable_rows_all,"Buildinglat"]), 
                    mean(dataTable()[input$myTable_rows_all,"Buildinglon"]), sep=",")
    data <- list()
    #get data for all types of utlities
      #build the url for our api requests
      url <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",
                   centre,"&radius=8000",
                   "&keyword=",util,
                   "&key=AIzaSyBD0L32-9x86RLaMX0uYFy5GT80bFbSni4", sep = "")
      #x equals the resultset we get from our API call
      x <- fromJSON(url,
                    simplify = FALSE)
      #store the lat/lons and names for use in renderleaflet layer options
      if (x$status!="OK" || length(x$results)<1) {return()}
      n <- length(x$results)
      m <- 1
      for (i in 1:n) {
        if (!is.null(x$results[[i]]$geometry$location$lat)) {
        data$lt[[m]] <- x$results[[i]]$geometry$location$lat
        data$lg[[m]] <- x$results[[i]]$geometry$location$lng
        data$nm[[m]] <- x$results[[i]]$name
        m <- m+1
        }
      }
      
      if (!is.null(x$next_page_token)) {
        url <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?pagetoken=",
                     x$next_page_token,"&key=AIzaSyBD0L32-9x86RLaMX0uYFy5GT80bFbSni4", sep = "")
        y <- fromJSON(url,
                      simplify = FALSE)
        if (y$status=="OK" ) {
        
        for (k in 1:length(y$results)) {
          if (!is.null(y$results[[k]]$geometry$location$lat)) {
          data$lt[[m]] <- y$results[[k]]$geometry$location$lat
          data$lg[[m]] <- y$results[[k]]$geometry$location$lng
          data$nm[[m]] <- y$results[[k]]$name
          m <- m+1
          }
        }
        }
      }
      return(data)
  }

  #store clicked marker data to display appropriate stats next to map
  data <- reactiveValues(clickedMarker=NULL, clickedMarkerData=NULL)
  
  # observe the marker click info and print to map page when it is changed.
  observeEvent(input$myMap_marker_click,{
    data$clickedMarker <- input$myMap_marker_click
    m <- data$clickedMarker
    data$clickedMarkerData <- getRates(m$id)
  })
  observeEvent(input$myMap_click,{
    data$clickedMarker <- NULL
    data$clickedMarkerData<-NULL
  })
  #the two tables that output when a marker is clicked
  #table 1 for school enrol/gradrates
  output$testData1 <- renderTable({
    data$clickedMarkerData$school
  }, bordered = TRUE, align = 'l', width = 'auto')
  #table2 for dist info/funding
  output$testData2 <- renderTable({
    data$clickedMarkerData$dist
  }, bordered = TRUE, align = 'r', width = 'auto')
  
  #remove the inpu ui's when submit is clicked
  observeEvent(input$submitBttn, {
    removeUI(
      selector = "div:has(> #inputs)"
    )
  })
  
  #Resets Page
  observeEvent(
    input$resetBttn, {
      js$reset()
    }
  )
  
  
  # Download Button
  output$downloadBttn <- downloadHandler(
    filename = function() { paste('data-', getFormattedTime(), '.csv', sep='') },
    content = function(file) {
      write.csv(dataTable()[input$myTable_rows_all, ], 
                file, 
                row.names = FALSE)
    }
  )
  
  
})

