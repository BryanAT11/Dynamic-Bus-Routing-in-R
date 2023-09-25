#source("setAWSPassword.R")
source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","fresh","lubridate","gsubfn", "shinyjs")
loadPkgs(pkgnames)

#changed endgamemodal, output for no. of passengers in each bus stop


#http://rstudio.github.io/shinydashboard/get_started.html
mytheme <- create_theme(
  adminlte_sidebar(
    dark_bg = "#FFFFE0",
    dark_hover_bg = "#FBDB65",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#006EB3",
    box_bg = "#FBF6D9", 
    info_box_bg = "#D8DEE9"
  )
)

busStops <- as.data.frame(list(stopid=1:7,name=c("s1","Yellow","Green","Purple","Grey","Brown","Hub"),
                               x_pct=c(100,18,33.3,53,76,74,1),
                               y_pct=c(100,40,17,43,10,80,80)))
numBuses <- 4
busOffsets <- as.data.frame(list(x_pct=c(-3,3,3,-3),y_pct=c(-3,-3,3,3)))

BUSID1 = 10
BUSID2 = 11
BUSID3 = 12
BUSID4 = 13
ARROWID = 2 #delete this
YELLOWSTOPID = 3 #delete this

getInitialSpriteArray <- function(){
  #Prof PJ Note: Sprite id should match the row number of the data frame
  spriteArray <- NULL
  
  sprite <- as.data.frame(list(id=1, img="building.png",
                               label="Building", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 20, y_scalepct= 20,
                               x_pct=busStops$x_pct[1], y_pct=busStops$y_pct[1], 
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray, sprite)
  
  sprite <- as.data.frame(list(id=2, img="blue_arrow1.png",
                               label="Arrow", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 50, y_scalepct= 50,
                               x_pct=110, y_pct=110, x=0, y=0 ))# initially off the map
  spriteArray <- rbind(spriteArray,sprite)
  
  sprite <- as.data.frame(list(id=3, img="",
                               label="", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 11, y_scalepct= 11,
                               x_pct=busStops$x_pct[2], y_pct=busStops$y_pct[2], 
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  
  sprite <- as.data.frame(list(id=4, img="",
                               label="", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 11, y_scalepct= 11,
                               x_pct=busStops$x_pct[3], y_pct=busStops$y_pct[3], 
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=5, img="",
                               label="", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 11, y_scalepct= 11,
                               x_pct=busStops$x_pct[4], y_pct=busStops$y_pct[4], 
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=6, img="",
                               label="", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 11, y_scalepct= 11,
                               x_pct=busStops$x_pct[5], y_pct=busStops$y_pct[5],  x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=7, img="",
                               label="", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 11, y_scalepct= 11,
                               x_pct=busStops$x_pct[6], y_pct=busStops$y_pct[6],  x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=8, img="",
                               label="", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 20, y_scalepct= 20,
                               x_pct=busStops$x_pct[7], y_pct=busStops$y_pct[7], 
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=9, img="arrowtopurplebusstop.png",
                               label="Arrow", showBorder=FALSE, isdraggable=FALSE,
                               x_scalepct = 30, y_scalepct= 30,
                               x_pct=110, y_pct=110, x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=10, img="",
                               label="Bus", showBorder=FALSE, isdraggable=TRUE,
                               x_scalepct = 15, y_scalepct= 15,
                               x_pct=110, y_pct=110, # Initially, each bus is off the map
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=11, img="",
                               label="Bus", showBorder=FALSE, isdraggable=TRUE,
                               x_scalepct = 17, y_scalepct= 15,
                               x_pct=110, y_pct=110, # Initially, each bus is off the map
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  sprite <- as.data.frame(list(id=12, img="",
                               label="Bus", showBorder=FALSE, isdraggable=TRUE,
                               x_scalepct = 17, y_scalepct= 15,
                               x_pct=110, y_pct=110, # Initially, each bus is off the map
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite) 
  
  sprite <- as.data.frame(list(id=13, img="",
                               label="Bus", showBorder=FALSE, isdraggable=TRUE,
                               x_scalepct = 17, y_scalepct= 15,
                               x_pct=110, y_pct=110, # Initially, each bus is off the map
                               x=0, y=0 ))
  spriteArray <- rbind(spriteArray,sprite)
  spriteArray
}

isLegalMove <- function(sprites,spriteid,newlocation){
  # Prof PJ: Don't use gridRow and gridCol anymore
  
  # # Check to make sure the location corresponds to a valid grid location
  # isLegal <- newlocation$row>=1 &newlocation$row<=GRIDSIZE & newlocation$col>=1 & newlocation$col<=GRIDSIZE # Will be either TRUE or FALSE
  # # We can prevent moving a piece on top of another piece if the other piece is draggable
  isLegal <- TRUE
  # Return the value of isLegal
  isLegal
}

getClosestBusStop <- function(dropLocation){
  # Copy dropLocation
  newlocation <- dropLocation
  # Get the coordinates in percentage terms
  x_pct <- dropLocation$x_pct
  y_pct <- dropLocation$y_pct
  # print(paste0("x_pct: ",x_pct," y_pct: ",y_pct))
  #compute distance from each bus stop (busStops is a global variable: never changes)
  dist <- sqrt((busStops$x_pct-x_pct)^2+(busStops$y_pct-y_pct)^2)
  # print("printing distance vector and which.min")
  # print(dist)
  # pick the bus stop that is closest to the drop location
  beststop <- which.min(dist)
  print(beststop)
  # replace newlocation with bus stop location
  newlocation$x_pct <- busStops$x_pct[beststop]
  newlocation$y_pct <- busStops$y_pct[beststop]
  # Prof PJ Note: store the bus stop index as well
  newlocation$busstop <- beststop
  # return the closest location
  newlocation
}

logoutModal<- function(failed=FALSE){
  modalDialog(
    h4("Are you sure you would like to logout?"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("logout", "Logout")
    )
  )
}

#Define current bus ID
current_BusLabel <- "A"
#Define maximum number of rows for table
maxRows <- numBuses

cartModal <- function(failed = FALSE, choices) {
  modalDialog(size = "l",
              tags$div(
                style = "position: relative;",
                img(src = "store.png", width = "870px", height = "500px", align = "middle"),
                tags$div(
                  style = "position: absolute; top: 42%; left: 66%; transform: translate(-50%, -50%);",
                  numericInput("numBusI", "Input quantity", width= "92px",value = 0, min = 0, max = 3, step = 1)),
                tags$div(
                  style = "position: absolute; top: 62%; left: 66%; transform: translate(-50%, -50%);",
                  numericInput("numBusII", "Input quantity", width= "92px", value = 0, min = 0, max = 5, step = 1)),
                tags$div(
                  style = "position: absolute; top: 10.4%; left: 86.6%; transform: translate(-50%, -50%);",
                  h2(htmlOutput("balance"))),
                tags$div(
                  style = "position: absolute; top: 78%; left: 66%; transform: translate(-50%, -50%);",
                  actionButton("addBuses", "Add Bus")),
                tags$div(style = "position: absolute; top: 78%; left: 87%; transform: translate(-50%, -50%);",
                         actionButton("deleteBuses", "Remove Bus")),
                tags$div(style = "position: absolute; top: 52%; left: 87%; transform: translate(-50%, -50%);",
                         selectizeInput("deleteBusId", "Select Bus to Remove:", choices = c('',choices), multiple = FALSE, width ="70px")),
                
                if (failed)
                  div(tags$b("Invalid quantity. Try again. Please input
                             a positive number and the quantity should be within budget and availability.", style = "color: red;")),
              ),
              footer = tagList(
                modalButton("Cancel")
              )
  )
}

passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Create a new password",
    textInput("username", "Enter your username:"),
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("username1", "Enter your username"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

endgameModal <- function(failed = FALSE){
  modalDialog(
    h2("You have reached the end of the game. Thank you for playing Shuttled! Proceed to the leaderboard to view your score."),
    
    
    footer = tagList(
      actionButton("leaderboard", "Leaderboard"),
      actionButton("bye", "Logout")
    )
  )
  
}

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student109",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student109",
    password = "B7mfsee%hHmx")
  conn
  
}

getPlayerID <- function(username,password){
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE user=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    playerid <- 0
  }
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}



createNewPlayerQuery <- function(conn,username,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  #conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderPlayer(user,password) VALUES(?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
}

registerPlayer <- function(password, username){
  #open the connection
  conn <- getAWSConnection()
  #playername <- getRandomPlayerName(conn)
  query <- createNewPlayerQuery(conn,username,password)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        playername <- getRandomPlayerName(conn)
        query <- createNewPlayerQuery(conn,playername,password) }, 
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) username = NULL
  #Close the connection
  dbDisconnect(conn)
  playername
}
title<- tags$a(tags$img(src="shuttled logo1.png", width = "600px", height = "180px", style="position:relative;top:50%;left:7%;transform:translate(-36%, -50%);"))

ui <- dashboardPage(
  dashboardHeader(title = title, 
                  titleWidth = 500),
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
      #https://fontawesome.com/icons?d=gallery
      menuItem("Login/Register", tabName = "home", icon = icon("circle-check")),
      menuItem("Home", tabName="success", icon=icon("house")),
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("Leaderboard", tabName = "leaderboard", icon = icon("ranking-star"))
    )
  ),
  dashboardBody(
    tags$audio(src="sound.mp3", type="mp3", autoplay = NA, controls = NA, loop = TRUE),
    use_theme(mytheme),
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              tags$head(tags$style('h1 {color:white;}')),
              h1("Welcome to Shuttled, A Dynamic Bus Routing Game"),
              h1(htmlOutput("loggedInAs")),
              tags$br(),
              actionButton("register", "Register"),
              actionButton("login", "Login"),
              tags$br()
              
      ),
      
      # Second tab content
      tabItem(tabName = "game",
              fluidRow(
                column(8,
                       tags$script(src="ShinyDraggableSprites.js"),
                       tags$div(id="playingfield"),
                       tags$script(src="https://d3js.org/d3.v3.min.js"),
                       tags$style(".spritelabel {fill:black;} .spriteborder {stroke:yellow;stroke-width:2} "),
                       div(
                         style = "position: absolute; top: 35%; left: 19%; z-index: 3;",
                         tags$div(
                           style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                           htmlOutput("busstop1", style = "font-size: 16px; font-weight: bold; position: relative; top: 10px; left: 20px;")
                         )
                       ), #yellow
                       div(
                         style = "position: absolute; top: 15%; left: 33%; z-index: 3;",
                         tags$div(
                           style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                           htmlOutput("busstop2", style = "font-size: 16px; font-weight: bold; position: relative; top: 10px; left: 20px;")
                         )
                       ), #green
                       div(
                         style = "position: absolute; top: 38%; left: 51%; z-index: 3;",
                         tags$div(
                           style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                           htmlOutput("busstop3", style = "font-size: 16px; font-weight: bold; position: relative; top: 10px; left: 20px;")
                         )
                       ), #purple
                       div(
                         style = "position: absolute; top: 70%; left: 72%; z-index: 3;",
                         tags$div(
                           style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                           htmlOutput("busstop4", style = "font-size: 16px; font-weight: bold; position: relative; top: 10px; left: 20px;")
                         )
                       ), #brown
                       div(
                         style = "position: absolute; top: 10%; left: 73%; z-index: 3;",
                         tags$div(
                           style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                           htmlOutput("busstop5", style = "font-size: 16px; font-weight: bold; position: relative; top: 10px; left: 20px;")
                         )
                       ), #grey
                ),
                column(4,
                       fluidRow(h1("Time Interval:")),
                       
                       fluidRow(
                         align = "center",
                         # Element 1: time
                         box(htmlOutput("time"), width = 6
                         )
                       ),
                       
                       
                       # Element 2: picture1
                       fluidRow(
                         tags$img(src = "Picture1.png", width = "300px", height = "190px", 
                                  height = 50, length = 50, 
                                  align = "left"),
                         
                         tags$img(id = "moveableImage", src = "Picture3.png", width = "3px", height = "163px", 
                                  style = "position: absolute; top: 40%; left: 0%;",
                                  align = "left")
                       ),
                       
                       
                       # Element 4: legend
                       fluidRow(
                         column(width = 12, align = "left",
                                tags$h1("Legend:")
                         )
                       ),
                       
                       # Element 5: picture2
                       fluidRow(
                         column(width = 12, align = "left",
                                tags$img(src = "Picture2.png", width = "200px", height = "50px",
                                         height = 100, length = 50)
                         )
                       )
                       
                ),
                
                useShinyjs(),
              ),
              fluidRow(column(8,
                              uiOutput("table_output"),
                              tags$style("
                         .table {
                         background-color: #d9ead3;
                         }
                         
                         .table td, .table th {
                         text-align: center;
                         }
                         
                         .table th {
                         background-color: #66b266;
                         color: #fff;
                         }
                         ")
                              ),
                       column(4,
                              
                              fluidRow(
                                column(width =12, align ="left",
                                       actionButton("simulate", img(src = "simulatebutton.png", size ="40px", width="40px")),
                                       actionButton("board_drop", "Board/Alight")
                                )
                              ),
                              fluidRow(
                                column(width = 12, align = "left",
                                       actionButton("goto_cart", img(src="cart.png", size ="40px", width="40px")),
                                       actionButton("goto_instructions", img(src="question mark.png", size ="40px", width="40px"))
                                       
                                )
                              )
                              ))
      ),
      
      tabItem(tabName="leaderboard",
              ui <- fluidPage(
                titlePanel(
                  div(
                    h1("🏆 Leaderboard 🏆 ", style = "text-align: center;")
                  )
                ),
                fluidRow(
                  column(10, 
                         align = "center",
                         h2("See who are the top scorers!", style = "text-align: center;", color = "white"),
                         numericInput("nrows", "Select number of players to be displayed:", 5),
                  ),
                  column(8,
                         tags$div(
                           style = "text-align: right;margin: 0 auto;",# Center-align the contents inside this div
                           
                           tableOutput("tbl"),
                           align = "center",
                           tags$style("
                                     .table {
                                     background-color: #059efc;
                                     font-size: 20px;
                                     }

                                     .table td, .table th {
                                     text-align: center;
                                     }

                                     .table th {
                                     background-color: #059efc;
                                     color: #fff;
                                     }
                                     ")# Your tableOutput element goes here
                         )
                         ),
                  column(4,
                         h1("Your score:"),
                         valueBoxOutput("TimeBox"),
                         valueBoxOutput("CashBox")))
              )
      ),
      
      tabItem(tabName = "success",
              img(src = "concept sketch.jpg", width = "1000px", height = "550px", style="display: block; margin-left: auto; margin-right: auto;"),
              actionButton("goto_home", "Home",style="position:absolute;top:20%;margin-left:65%"),
              actionButton("goto_leaderboard", "Leaderboard",style="position:absolute;top:30%;margin-left:64%"),
              actionButton("goto_instructions", "How to play",style="position:absolute;top:40%;margin-left:64.2%"),
              actionButton("goto_game", "Game",style="position:absolute;top:50%;margin-left:65.1%"),
              actionButton("goto_logout", "Logout",style="position:absolute;top:60%;margin-left:65%")
              
      )
      
      
    )
    
    
  )
)


server <- function(input, output, session) {
  shinyjs::onclick("simulate", {
    shinyjs::runjs("$('#moveableImage').animate({left: '+=11.5px'}, 'slow');")
  })
  
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "student109",
      host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
      username = "student109",
      password = "B7mfsee%hHmx")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn, paste0(
      "SELECT Player_Name AS 'Player Name', Score, Remaining_budget AS 'Remaining Budget', Date FROM LeaderScore ORDER BY Score ASC LIMIT ", input$nrows))
  })
  
  bus1 <- data.frame(
    RemainingCapacity = 50,
    CurrentCapacity = 0,
    TotalCapacity = 50,
    LocationID = "Hub"
  )
  # Compose data for display
  #Prof PJ Note: for my screen resolution, I had to change the width of the playing field
  #newdata <- list(divid="playingfield",imgname="citymap.png",width=1000,height=500)
  newdata <- list(divid="playingfield",imgname="concept sketch.png",width=800,height=500)
  newdata <- as.data.frame(newdata)
  # Convert the R object to a JSON string
  var_json <- toJSON(newdata)
  # Use the session variable to send the data to the client;
  session$sendCustomMessage(type="initPlayingField",var_json)
  
  # Initialize the dynamic game sprites
  sprites <- getInitialSpriteArray()
  
  #Set game parameters
  MAXTURNS <- 15 # Keep it short for debugging
  # reactiveValues objects for storing items like the user password
  source("backend coding shuttled copy.R")
  vals <- reactiveValues(password = NULL,playerid=NULL,username=NULL,n=1, dataframe = playerdf, balance = 10000, time = format(as.POSIXlt("09:00:00", format = "%H:%M:%S"), format = "%I:%M %p"), 
                         sprites= sprites, animationDuration=200,spriteschange=0, yellowstop = data.frame(),greenstop = data.frame(),
                         purplestop = data.frame(),brownstop = data.frame(),greystop = data.frame(), bus1p = data.frame(),bus2p = data.frame(), bus3p = data.frame(), bus4p = data.frame(),
                         homedf = data.frame(), simulatecount = 1, bs1df = bs1, bs2df = bs2, bs3df = bs3, bs4df = bs4, bs5df = bs5, map_data = NULL,
                         breakdown = data.frame(c(FALSE, FALSE, FALSE, FALSE)), spritebd = data.frame(c(FALSE, FALSE, FALSE, FALSE)), score_board = NULL)
  GRIDSIZE <- 4
  pieces <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  gamevals <- reactiveValues(turncount=0,pieces=pieces)
  # Initialize reactiveValues to store the data
  reactive_data <- reactiveValues(bus_data = NULL)
  #Store the original data when generated
  originalBusdata <- reactiveValues()
  ##------------------Functions place below-------------------------------------------
  #Function to add a new row of data for Buses to the dataframe
  addBus <- function(bus_capacity, travel_time, numBus_to_add,  .show_notification = TRUE, .warning_notification = FALSE) {
    if (!is.numeric(bus_capacity) || !bus_capacity %in% c(30,50)){
      showNotification("Invalid bus capacity. Please enter either 30 or 50.", type = "warning")
      return()
    }
    if (!is.numeric(numBus_to_add) || is.na(numBus_to_add) || numBus_to_add < 0) {
      showNotification("Please enter a valid number of buses to add.", type = "warning")
      return()
    }
    if (!is.null(reactive_data$bus_data) && nrow(reactive_data$bus_data) >= maxRows) {
      if (!.warning_notification) {
        showNotification("No more rows can be added. The table is full.", type = "warning")
        .warning_notification <- TRUE
      }
      return()
    }
    # Define a vector of all possible busIDs (1, 2, 3, 4)
    all_possible_busIDs <- c(1, 2, 3, 4)
    
    # Get the busIDs already present in reactive_data$bus_data
    existing_busIDs <- reactive_data$bus_data$busID
    
    # Find the available busIDs (not used yet)
    available_busIDs <- setdiff(all_possible_busIDs, existing_busIDs)
    
    # If there are no available busIDs, reset the available_busIDs vector to all_possible_busIDs
    if (length(available_busIDs) == 0) {
      available_busIDs <- all_possible_busIDs
    }
    
    for (i in 1:numBus_to_add) {
      next_busID <- min(available_busIDs)
      available_busIDs <- available_busIDs[available_busIDs != next_busID]
      if (length(available_busIDs) == 0) {
        available_busIDs <- c(1, 2, 3, 4)
      }
      new_row <- as.data.frame(list(
        Bus = current_BusLabel, 
        busID = next_busID,
        #Battery = 100,
        Remaining_Travel_Time = travel_time,
        Current_Capacity = 0,
        Remaining_Capacity = bus_capacity,
        Total_Capacity = bus_capacity,
        Current_Location = "Hub",
        #Next_Location = "Green",
        # origin=NA, #PJ Note: not used anymore
        destination=0,
        midway=FALSE,
        stringsAsFactors = FALSE
      ))
      current_BusLabel <<- rawToChar(as.raw(as.integer(charToRaw(current_BusLabel))+1))
      if (is.null(reactive_data$bus_data)) {
        reactive_data$bus_data <- new_row
      } else {
        reactive_data$bus_data <- rbind(reactive_data$bus_data, new_row)
      }
    }
    
    if (.show_notification) {
      showNotification("Buses added successfully!", type = "message")
    }
    
  }
  
  #Function to remove rows from table  
  removeBus <- function(busId){
    reactive_data$bus_data <- reactive_data$bus_data[!reactive_data$bus_data$Bus %in% busId, ]
    showNotification("Bus removed successfully!", type = "message")
  }
  
  #Function to board passengers from bus stop to bus
  boardbus <- function(bus, bus_stop, bus_passenger) {
    # calculate how many passengers the bus can take
    available_seats = min(bus$Remaining_Capacity, bus$Total_Capacity - nrow(bus_passenger))
    
    # if the bus has available seats
    if(available_seats > 0) {
      # if there are enough or more passengers at the bus stop than available seats
      if(nrow(bus_stop) >= available_seats) {
        # take as many passengers as there are available seats
        bus_passenger <- rbind(bus_passenger, head(bus_stop, available_seats))
        bus_stop <- tail(bus_stop, -available_seats)
      } else {  # if there are less passengers at the bus stop than available seats
        # take all passengers
        bus_passenger <- rbind(bus_passenger, bus_stop)
        bus_stop <- data.frame()
      }
    }
    
    return(list(bus_stop, bus_passenger))
  }
  
  #Function to drop player of at Hub
  drop <- function(bus_passenger, homedf, current_time_index){
    if (!is.null(bus_passenger) && nrow(bus_passenger) > 0){
      bus_passenger$TimeDrop <- current_time_index - 1
      print("...................")
      print(bus_passenger)
      print("...................")
      if (is.null(homedf)){
        homedf <- bus_passenger
      }else{
        homedf <- rbind(homedf, bus_passenger)
      }
      bus_passenger <- data.frame()
    } else {
      return(list(bus_passenger, homedf))
    }
    return(list(bus_passenger, homedf))
    
    
  }
  
  #Function to add passengers to bus stop
  add_rows_with_values <- function(bus_stop, random_number, current_time_index) {
    # Generate values from 1 to n
    # new_passengers <- 1:random_number
    # new_passengers
    #passenger <- 0
    # Create a new data frame with the generated values
    for (i in seq_len(random_number)){
      new_rows <- data.frame(
        Passenger = i ,
        TimeArrival = current_time_index
      )
      if (is.null(bus_stop)){
        bus_stop <- new_rows
      }else{
        bus_stop <- rbind(bus_stop, new_rows)
      }
    }
    
    return(bus_stop)}
  
  #Function to calculate score
  calculate_score <- function(homedf){
    time_intervals <- homedf$TimeDrop - homedf$TimeArrival
    homedf$WaitingTime <- time_intervals *30
    total_waiting_time <- sum(homedf$WaitingTime)
    score <- round(total_waiting_time/length(homedf$Passenger))
    return(score)
  }
  
  ##------------------Functions place above-------------------------------------------
  
  # Prof PJ Note: whenever you change the bus_data data frame, you should cause vals$spriteschange to change so that this code gets run.
  # This will update the sprites to match the bus_data.
  observeEvent(vals$spriteschange,{
    # This will get called as soon as the vals object is created because we set vals$spriteschange=0.
    print(vals$sprites)
    if (vals$spriteschange>0){
      if (!is.null(reactive_data$bus_data)){
        BUSID_List =c(BUSID1, BUSID2, BUSID3, BUSID4)
        for (busid in BUSID_List){
          busrow <- busid-BUSID1+1
          print("aaana")
          print(busrow)
          print("nnnaa")
          # #print(reactive_data$bus_data)
          if (busrow %in% reactive_data$bus_data$busID){
            print(busrow)
            vals$sprites$label[busid] <- reactive_data$bus_data$Bus[reactive_data$bus_data$busID == busrow]
            
          }else{
            #   # If there is no bus for this busid, set the location off the map
            print(busid)
            print(busrow)
            vals$sprites$x_pct[busid] <- 110
            vals$sprites$y_pct[busid] <- 110
          } 
        }
      }
    }
    session$sendCustomMessage(type="displaySprites",toJSON(vals$sprites))
    
  }
  )
  
  observeEvent(input$spriteRightClick,{
    id <- input$spriteRightClick$id
    showNotification(paste0("You right clicked sprite with id: ",id))
  })
  
  observeEvent(input$playingFieldClick,{
    click_pct <- input$playingFieldClick
    showNotification(paste0("You clicked on the field at ",click_pct[1]," and ",click_pct[2]))
  })
  
  observeEvent(input$spriteDrop,{
    dropLocation <-  input$spriteDrop
    id <- dropLocation$id
    # the x_pct and y_pct must be scaled
    dropLocation$x_pct <- dropLocation$x_pct*100
    dropLocation$y_pct <- dropLocation$y_pct*100
    oldx_pct <-  vals$sprites[id,"x_pct"]
    #print(paste0("oldposx:",oldx_pct))
    oldy_pct <- vals$sprites[id,"y_pct"]
    #print(paste0("oldposy:",oldy_pct))
    
    if (id == BUSID1 | id == BUSID2| id==BUSID3 |id==BUSID4) {
      vals$sprites[id,"isdraggable"] <- FALSE
      # Save the original location
      originalLocation <- list(x_pct = vals$sprites[id,"x_pct"], y_pct = vals$sprites[id,"y_pct"])
      originalStop <- getClosestBusStop(originalLocation)
      # PJ Note: extract the index of the bus stop
      busrow <- id-BUSID1+1
      closestLocation <- getClosestBusStop(dropLocation)
      destination <- closestLocation$busstop
      reactive_data$bus_data$destination[reactive_data$bus_data$busID==busrow] <- destination #PJ Note: we cannot store a list object in a data frame
      reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==busrow] <- busStops$name[destination]
      
      newlocation <- closestLocation
      # print(paste0("x_pct: ",newlocation$x_pct," y_pct: ",newlocation$y_pct))
      newlocation$x_pct <- newlocation$x_pct + busOffsets$x_pct[id-BUSID1+1]
      newlocation$y_pct <- newlocation$y_pct + busOffsets$y_pct[id-BUSID1+1]
      # print(paste0("x_pct: ",newlocation$x_pct," y_pct: ",newlocation$y_pct))
      
      vals$sprites[id,"x_pct"] <- newlocation$x_pct
      vals$sprites[id,"y_pct"] <- newlocation$y_pct
      
      # If the original location and BUSID's location match specified values,
      # set new values for BUSID's x_pct and y_pct
      
      # print(originalStop)
      # print(closestLocation)
      if(originalStop$x_pct == closestLocation$x_pct & originalStop$y_pct == closestLocation$y_pct ){
        vals$sprites[id,"isdraggable"] <- TRUE
      }
      if (originalStop$x_pct==busStops$x_pct[7] & originalStop$y_pct==busStops$y_pct[7] & 
          closestLocation$x_pct == busStops$x_pct[6]& closestLocation$y_pct == busStops$y_pct[6]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        vals$sprites[id,"x_pct"] <- 40 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 73 + busOffsets$y_pct[id-BUSID1+1]
      }
      if (originalStop$x_pct==busStops$x_pct[6] & originalStop$y_pct==busStops$y_pct[6] & 
          closestLocation$x_pct == busStops$x_pct[7]& closestLocation$y_pct == busStops$y_pct[7]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        
        vals$sprites[id,"x_pct"] <- 40 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 73 + busOffsets$y_pct[id-BUSID1+1]
      }
      # bus stuck from brown to yellow
      if (originalStop$x_pct==busStops$x_pct[6] & originalStop$y_pct==busStops$y_pct[6] & 
          closestLocation$x_pct == busStops$x_pct[2]& closestLocation$y_pct == busStops$y_pct[2]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        
        vals$sprites[id,"x_pct"] <- 40 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 55 + busOffsets$y_pct[id-BUSID1+1]
      }
      # bus stuck from yellow to brown
      if (originalStop$x_pct==busStops$x_pct[2] & originalStop$y_pct==busStops$y_pct[2] & 
          closestLocation$x_pct == busStops$x_pct[6]& closestLocation$y_pct == busStops$y_pct[6]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        
        vals$sprites[id,"x_pct"] <- 40 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 55 + busOffsets$y_pct[id-BUSID1+1]
      }
      # from hub to grey bus stop
      if (originalStop$x_pct==busStops$x_pct[7] & originalStop$y_pct==busStops$y_pct[7] & 
          closestLocation$x_pct == busStops$x_pct[5]& closestLocation$y_pct == busStops$y_pct[5]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        
        vals$sprites[id,"x_pct"] <- 10 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 5 + busOffsets$y_pct[id-BUSID1+1]
      }
      # from grey bustop to hub
      if (originalStop$x_pct==busStops$x_pct[5] & originalStop$y_pct==busStops$y_pct[5] & 
          closestLocation$x_pct == busStops$x_pct[7]& closestLocation$y_pct == busStops$y_pct[7]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        vals$sprites[id,"x_pct"] <- 10 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 5 + busOffsets$y_pct[id-BUSID1+1]
      }
      #green to brown
      if (originalStop$x_pct==busStops$x_pct[3] & originalStop$y_pct==busStops$y_pct[3] & 
          closestLocation$x_pct == busStops$x_pct[6]& closestLocation$y_pct == busStops$y_pct[6]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        vals$sprites[id,"x_pct"] <- 50 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 20 + busOffsets$y_pct[id-BUSID1+1]
      }
      if (originalStop$x_pct==busStops$x_pct[6] & originalStop$y_pct==busStops$y_pct[6] & 
          closestLocation$x_pct == busStops$x_pct[3]& closestLocation$y_pct == busStops$y_pct[3]) {
        reactive_data$bus_data$midway[busrow] <- TRUE
        
        vals$sprites[id,"x_pct"] <- 50 + busOffsets$x_pct[id-BUSID1+1]
        vals$sprites[id,"y_pct"] <- 30 + busOffsets$y_pct[id-BUSID1+1]
      }
    } 
    
    vals$spriteschange <- vals$spriteschange+1
  })
  
  #Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(passwordModal(failed=FALSE))
  })
  
  # Fire some code if the user clicks the passwordok button
  observeEvent(input$passwordok, {
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      #store the password and close the dialog
      vals$password <- input$password1
      vals$username <- input$username
      
      print(vals$password) # for debugging
      #vals$username = registerPlayer(vals$password)
      if (!is.null(vals$username)){
        vals$playerid <- getPlayerID(vals$username,vals$password)
        updateTabItems(session, "tabs", "success")
        
      }
      
      print(vals$playerid) # for debugging
      removeModal()
    } else {
      showModal(passwordModal(failed = TRUE))
    }
  })
  
  #Fire some code if the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  
  # Fire some code if the user clicks the loginok button
  observeEvent(input$loginok, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$username1,input$password3)
    if (playerid>0) {
      #store the playerid and playername and close the dialog
      vals$playerid <- playerid
      #print(vals$playerid) # for debugging
      vals$username <- input$username1
      updateTabItems(session, "tabs", "success")
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
      showModal(loginModal())
    }
  })
  
  observeEvent(input$goto_logout,{
    showModal(logoutModal(failed=FALSE))
  })
  
  observeEvent(input$logout, {
    vals$password <- NULL
    vals$playerid <- NULL
    vals$username <- NULL
    updateTabItems(session, "tabs", "home")
    removeModal()
    
  })
  
  observeEvent(input$startgame, {
    if (vals$simulatecount > 1) {
      showNotification("Game has already started.", type = "warning")
      return()  # Return without simulating
    }
    min_value = 5 #change for each interval
    max_value = 10
    random_number_y <- sample(min_value:max_value, 1)
    random_number_g <- sample(min_value:max_value, 1)
    random_number_p <- sample(min_value:max_value, 1)
    random_number_b <- sample(min_value:max_value, 1)
    random_number_gy <- sample(min_value:max_value, 1)
    vals$yellowstop<-add_rows_with_values(vals$yellowstop, random_number_y, vals$simulatecount )
    vals$greenstop<-add_rows_with_values(vals$greenstop, random_number_g, vals$simulatecount )
    vals$purplestop<-add_rows_with_values(vals$purplestop, random_number_p, vals$simulatecount )
    vals$brownstop<-add_rows_with_values(vals$brownstop, random_number_b, vals$simulatecount )
    vals$greystop<-add_rows_with_values(vals$greystop, random_number_gy, vals$simulatecount )
    vals$simulatecount <- vals$simulatecount + 1
    removeModal()
  })
  
  vals$clickCount <- 0
  observeEvent(input$simulate, {
    if (vals$simulatecount == 1) {
      showNotification("Cannot simulate. Please start game.", type = "warning")
      return()  # Return without simulating
    }
    if (is.null(reactive_data$bus_data) || nrow(reactive_data$bus_data) == 0) {
      showNotification("Cannot simulate. The table is empty.", type = "warning")
      return()  # Return without simulating
    }
    vals$n <- vals$n+1
    vals$time<- timebutton(vals$n)
    
    # vals$simulatecount <- vals$simulatecount +1 #count number of times simulate clicked <- name that variable simulatecount
    if (!is.na(vals$time)){
      #print(genpassengers(vals$time))
      vals$bs1df[[1]] <- genpassengers(vals$time)[[1]]
      vals$bs2df[[1]] <- genpassengers(vals$time)[[2]]
      vals$bs3df[[1]] <- genpassengers(vals$time)[[3]]
      vals$bs4df[[1]] <- genpassengers(vals$time)[[4]]
      vals$bs5df[[1]] <- genpassengers(vals$time)[[5]]
      
      random_number_y <- vals$bs1df[[1]]
      random_number_g <- vals$bs2df[[1]]
      random_number_p <- vals$bs3df[[1]]
      random_number_b <- vals$bs4df[[1]]
      random_number_gy <- vals$bs5df[[1]]
      vals$yellowstop<-add_rows_with_values(vals$yellowstop, random_number_y, vals$simulatecount )
      vals$greenstop<-add_rows_with_values(vals$greenstop, random_number_g, vals$simulatecount )
      vals$purplestop<-add_rows_with_values(vals$purplestop, random_number_p, vals$simulatecount )
      vals$brownstop<-add_rows_with_values(vals$brownstop, random_number_b, vals$simulatecount )
      vals$greystop<-add_rows_with_values(vals$greystop, random_number_gy, vals$simulatecount )
      vals$map_data$Bus_Stops <- c("Yellow", "Green", "Purple", "Brown", "Grey")
      vals$map_data$Passengers <- c(nrow(vals$yellowstop), nrow(vals$greenstop), nrow(vals$purplestop), nrow(vals$brownstop), nrow(vals$greystop))
      vals$simulatecount <- vals$simulatecount + 1
      #print(vals$map_data)
    }
    print("simulate button clicked")
    source("backend coding shuttled copy.R")
    
    vals$clickCount <- vals$clickCount + 1
    BUS_ID <- c(BUSID1, BUSID2,BUSID3,BUSID4)
    for (i in reactive_data$bus_data$busID){
      #print(nrow(reactive_data$bus_data))
      print("dadaa")
      print(BUS_ID[i])
      vals$sprites[BUS_ID[i],"isdraggable"] <- TRUE
      busrow <- BUS_ID[i]-BUSID1+1
      destination <- reactive_data$bus_data$destination[reactive_data$bus_data$busID==i]
      #print(paste("Destination", destination))
      # PJ Note: destination is an index into the busStops data frame
      isMidway <- reactive_data$bus_data$midway[reactive_data$bus_data$busID==i]
      #print(paste("mid", isMidway))
      vals$sprites[BUS_ID[i],"isdraggable"] <- !isMidway
      reactive_data$bus_data$midway[reactive_data$bus_data$busID==i] <- FALSE
      if(vals$sprites[BUS_ID[i],"isdraggable"] & destination > 0){
        vals$sprites[BUS_ID[i],"x_pct"] <- busStops$x_pct[destination] + busOffsets$x_pct[BUS_ID[i]-BUSID1+1]
        vals$sprites[BUS_ID[i],"y_pct"] <- busStops$y_pct[destination] + busOffsets$y_pct[BUS_ID[i]-BUSID1+1]
      }
    }
    print("vvvvv")
    vals$spriteschange <- vals$spriteschange+1
    
    print("bg")
    if (!is.null(reactive_data$bus_data)) {
      # Calculate the changes in capacity for each bus based on the map data
      for (i in reactive_data$bus_data$busID) {
        # Check if the remaining travel time is 0 and the current location is not "Hub"
        if (reactive_data$bus_data$Remaining_Travel_Time[reactive_data$bus_data$busID==i] == 0 && reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==i] != "Hub") {
          reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==i] <- "Hub"
        }
      }
      # Reduce traveling time by 20 and 1, respectively
      reactive_data$bus_data$Remaining_Travel_Time <- pmax(0, reactive_data$bus_data$Remaining_Travel_Time - 1)
      # Check if remaining traveling time is 0 and current location is not "Hub"
      time_zero_indices <- which(reactive_data$bus_data$Remaining_Travel_Time == 0 & reactive_data$bus_data$Current_Location != "Hub")
      if (length(time_zero_indices) > 0) {
        vals$breakdown[time_zero_indices,1] = !vals$breakdown[time_zero_indices,1]
        vals$spritebd[time_zero_indices,1] = !vals$spritebd[time_zero_indices,1]
        print("tzi")
        print(vals$breakdown[time_zero_indices,1])
        reactive_data$bus_data$Current_Location[time_zero_indices] <- "Hub"
      }
      if("Hub" %in% reactive_data$bus_data$Current_Location){
        hub_indices <- reactive_data$bus_data$busID[reactive_data$bus_data$Current_Location == "Hub"]
        if(!is.null(originalBusdata)){
          print("hind")
          print(vals$spritebd[hub_indices,1])
          for (i in hub_indices){
            print("vsbd")
            print(vals$spritebd[i, 1])
            if (vals$spritebd[i, 1]){
              print("fp")
              busId <- reactive_data$bus_data$Bus[reactive_data$bus_data$busID == i]
              originalvalues <- originalBusdata[[busId]]
              reactive_data$bus_data[reactive_data$bus_data$busID == i, ] <- originalvalues
              vals$sprites[BUS_ID[i],"x_pct"] <- busStops$x_pct[7] + busOffsets$x_pct[BUS_ID[i]-BUSID1+1]
              vals$sprites[BUS_ID[i],"y_pct"] <- busStops$y_pct[7] + busOffsets$y_pct[BUS_ID[i]-BUSID1+1]
              vals$spritebd[i,1] = !vals$spritebd[i,1]
            }else{
              busId <- reactive_data$bus_data$Bus[reactive_data$bus_data$busID == i]
              originalvalues <- originalBusdata[[busId]]
              destination <- reactive_data$bus_data$destination[reactive_data$bus_data$busID == i]
              midway <- reactive_data$bus_data$midway[reactive_data$bus_data$busID == i]
              reactive_data$bus_data[reactive_data$bus_data$busID == i, ] <- originalvalues
              reactive_data$bus_data$destination[reactive_data$bus_data$busID == i] <- destination
              reactive_data$bus_data$midway[reactive_data$bus_data$busID == i] <- midway
              
            }
          }
        }  
      }
    } 
    
    print("klklk")
    print(reactive_data$bus_data)
  })
  
  observeEvent(input$board_drop, {
    print("board/drop clicked")
    Bus_passenger_list <- list(vals$bus1p, vals$bus2p, vals$bus3p, vals$bus4p)
    Bus_stop_list <- list(Yellow = vals$yellowstop, Green = vals$greenstop, Purple = vals$purplestop, Brown = vals$brownstop, Grey = vals$greystop)
    if (!is.null(reactive_data$bus_data)){
      print("hello")
      for (i in reactive_data$bus_data$busID){
        current_bus <- reactive_data$bus_data$Bus[reactive_data$bus_data$busID==i]
        current_bus_location <- reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==i]
        total_capacity <- reactive_data$bus_data$Total_Capacity[reactive_data$bus_data$busID==i]
        remaining_capacity <- reactive_data$bus_data$Remaining_Capacity[reactive_data$bus_data$busID==i]
        if (current_bus_location != "Hub"){
          print("----------------bps-------------------")
          print(Bus_passenger_list[[i]])
          print("-----------------vals------------------")
          print(reactive_data$bus_data)
          print(reactive_data$bus_data$busID==i)
          print(reactive_data$bus_data[reactive_data$bus_data$busID==i, ])
          boardfromstop <- boardbus(reactive_data$bus_data[reactive_data$bus_data$busID==i, ], Bus_stop_list[[current_bus_location]], Bus_passenger_list[[i]])
          Bus_stop_list[[current_bus_location]] <- boardfromstop[[1]]
          Bus_passenger_list[[i]] <- boardfromstop[[2]]
          print(Bus_passenger_list[[i]])
          print(length(Bus_passenger_list[[i]]))
          print("--------BusPassenger-------")
          print(Bus_passenger_list[[i]])
        } else if (current_bus_location == "Hub"){
          print(i)
          print(vals$breakdown[1,1])
          if (vals$breakdown[i,1]){
            vals$simulatecount <- vals$simulatecount + 6
            print(vals$simulatecount)
            vals$breakdown[i,1] <- !vals$breakdown[i,1]
            drophub <- drop(Bus_passenger_list[[i]], vals$homedf, vals$simulatecount)
            Bus_passenger_list[[i]] <- drophub[[1]]
            vals$homedf <- drophub[[2]]
            vals$simulatecount <- vals$simulatecount - 6
            print(vals$simulatecount)
          }else{
          print(i)
          print(Bus_passenger_list[[i]])
          print(Bus_passenger_list[[1]])
          print(Bus_passenger_list[[2]])
          print(Bus_passenger_list[[3]])
          print(Bus_passenger_list[[4]])
          print(vals$homedf)
          print("::::::::::::::::::::::::::")
          drophub <- drop(Bus_passenger_list[[i]], vals$homedf, vals$simulatecount)
          print(Bus_passenger_list[[i]])
          print(drophub[[1]])
          print(drophub[[2]])
          Bus_passenger_list[[i]] <- drophub[[1]]
          vals$homedf <- drophub[[2]]
          print(vals$homedf)
        }
        }
      }
      for (i in reactive_data$bus_data$busID){
        if(reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==i] != "Hub"){
          if(i == 1){
            vals$bus1p <- Bus_passenger_list[[1]]
            print(nrow(vals$bus1p))
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus1p)
            print(vals$bus1p)
            print("-----------------------")
          }else if(i == 2) {
            vals$bus2p <- Bus_passenger_list[[2]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus2p)
            print(vals$bus2p)
          }else if(i == 3){
            vals$bus3p <- Bus_passenger_list[[3]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus3p)
          }else if(i == 4){
            vals$bus4p <- Bus_passenger_list[[4]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus4p)
          }
        } else if (reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==i] == "Hub"){
          if(i == 1){
            vals$bus1p <- Bus_passenger_list[[1]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus1p)
          }else if(i == 2) {
            vals$bus2p <- Bus_passenger_list[[2]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus2p)
          }else if(i == 3){
            vals$bus3p <- Bus_passenger_list[[3]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus3p)
          }else if(i == 4){
            vals$bus4p <- Bus_passenger_list[[4]]
            reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i] = nrow(vals$bus4p)
          }
          print(vals$homedf)
        } else {
          return()
        }
        reactive_data$bus_data$Remaining_Capacity[reactive_data$bus_data$busID==i] = reactive_data$bus_data$Total_Capacity[reactive_data$bus_data$busID==i] - reactive_data$bus_data$Current_Capacity[reactive_data$bus_data$busID==i]
        
      }
      vals$yellowstop <- Bus_stop_list$Yellow
      vals$greenstop <- Bus_stop_list$Green
      vals$purplestop <- Bus_stop_list$Purple
      vals$brownstop <- Bus_stop_list$Brown
      vals$greystop <- Bus_stop_list$Grey
    }
  })
  
  observeEvent(input$bye,{
    vals$password <- NULL
    vals$playerid <- NULL
    vals$username <- NULL
    updateTabItems(session, "tabs", "home")
    removeModal()
  })
  
  observeEvent(input$goto_home, {
    updateTabItems(session, "tabs", "home")
  })
  observeEvent(input$goto_leaderboard, {
    updateTabItems(session, "tabs", "leaderboard")
  }) 
  
  observeEvent(input$leaderboard, {
    updateTabItems(session, "tabs", "leaderboard")
    if(is.na(vals$time)){
      for (busid in reactive_data$bus_data$busID){
        if(reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID==busid] == "Hub"){
          if(reactive_data$bus_data$Total_Capacity[reactive_data$bus_data$busID==busid] == 50){
            vals$balance <- vals$balance + 1000
          }else{
            vals$balance <- vals$balance + 750
          }
        }
      } 
      score <- calculate_score(vals$homedf)
      vals$score_board$time <- score
      vals$score_board$cash <- vals$balance
      print(cat("Your score is: ", score, "mins with a cash balance of:",vals$score_board$cash, "\n"))
      print(vals$score_board)
      
    }
    
    removeModal()
  }) 
  
  observeEvent(input$goto_game, {
    updateTabItems(session, "tabs", "game")
    showModal(modalDialog(title = "Game tutorial",
                          h3("The objective of this game is to  minimize passenger waiting times 
       and enhance customer satisfaction by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l", img(src="tutorial1.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep2", "Next"))
                          
    ))
    
  }) 
  
  observeEvent(input$goto_game1, {
    showModal(modalDialog(title = "Game tutorial",
                          h3("The objective of this game is to  minimize passenger waiting times 
        by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l", img(src="tutorial1.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep2", "Next"))
                          
    ))
    
  }) 
  
  observeEvent(input$nextstep2, {
    showModal(modalDialog(title = "Game tutorial",
                          h3("The objective of this game is to  minimize passenger waiting times 
       and enhance customer satisfaction by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l",img(src="tutorial2.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("goto_game1", "Previous"), actionButton("nextstep3", "Next"))
                          
    ))
    
  })
  
  observeEvent(input$nextstep3,{
    showModal(modalDialog(title = "Game tutorial",
                          h3("The objective of this game is to  minimize passenger waiting times 
       and enhance customer satisfaction by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l",img(src="tutorial3.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep2", "Previous"), actionButton("nextstep4", "Next"))
                          
    ))
    
  })
  
  observeEvent(input$nextstep4,{
    showModal(modalDialog(title = "Game tutorial",
                          h3("The objective of this game is to  minimize passenger waiting times 
       and enhance customer satisfaction by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l",img(src="tutorial4.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep3", "Previous"), actionButton("nextstep5", "Next"))
                          
    ))
    
  })
  
  observeEvent(input$nextstep5,{
    showModal(modalDialog(title = "Game tutorial",
                          h3("The objective of this game is to  minimize passenger waiting times 
       and enhance customer satisfaction by planning and optimizing shuttle bus routes."),
                          h4("Click away or press ESC to close page."),
                          easyClose = TRUE,
                          fade = FALSE,    size = "l",img(src="tutorial5.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep4", "Previous"), actionButton("startgame", "Start Game") )
                          
    ))
    
    updateTabItems(session, "tabs", "game")
  })
  
  observeEvent(input$goto_instructions, {
    showModal(modalDialog(title = "Game tutorial",
                          h4("The objective of this game is to  minimize passenger waiting times by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l", img(src="tutorial1.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep2", "Next"))
                          
    ))
  })
  
  observeEvent(input$goto_instructions1, {
    
    showModal(modalDialog(title = "Game tutorial",
                          h4("The objective of this game is to  minimize passenger waiting times 
       and enhance customer satisfaction by planning and optimizing shuttle bus routes."),
                          easyClose = FALSE,
                          fade = FALSE,    size = "l", img(src="tutorial1.png", width = "870px", height = "500px", align="middle"),
                          footer = tagList(actionButton("nextstep2", "Next"))
                          
    ))
  })
  
  observeEvent(input$goto_cart, {
    if (vals$simulatecount == 1) {
      showNotification("Cannot open cart. Please start game.", type = "warning")
      return()  # Return without simulating
    }
    showModal(cartModal(failed=FALSE, choices = c("",reactive_data$bus_data$Bus)))
  })
  
  
  observeEvent(input$addBuses, {
    # source("Function.R")
    print("Add Bus button clicked!")
    num_typeIBus <- input$numBusI
    num_typeIIBus <- input$numBusII
    display_warning <- FALSE
    if (num_typeIBus == 0 && num_typeIIBus == 0) {
      return()
    }
    totalBus <- num_typeIBus + num_typeIIBus
    if (!is.null(reactive_data$bus_data)) {
      numdistinctBusIds <- length(unique(reactive_data$bus_data$Bus))
    } else {
      numdistinctBusIds <- 0
    }
    
    # Check if the table is already full
    if (numdistinctBusIds >= maxRows) {
      showNotification("Cannot add more buses. The table is already full.", type = "warning")
      return()
    }
    
    # The table is not full, check if adding new buses will exceed the remaining space
    remaining_rows <- maxRows - numdistinctBusIds
    if (totalBus > remaining_rows) {
      showNotification("Cannot add more buses. The table can only have 4 buses at one time.", type = "warning")
      return()
    }
    
    if(vals$balance<(num_typeIBus*3000 + num_typeIIBus*2000)){
      showNotification("Insufficient balance.", type = "warning")
      return()
    }
    source("backend coding shuttled copy.R")
    if (num_typeIBus > 0) {
      addBus(50, 5, num_typeIBus, .show_notification = !display_warning, .warning_notification = display_warning)
      display_warning <- TRUE
      vals$balance <- vals$balance - num_typeIBus*busdf$cost_of_bus[1]
    }
    
    if (num_typeIIBus > 0) {
      addBus(30, 8, num_typeIIBus, .show_notification = !display_warning, .warning_notification = display_warning)
      display_warning <- TRUE
      vals$balance <- vals$balance - num_typeIIBus*busdf$cost_of_bus[2]
    }
    
    if (!is.null(reactive_data$bus_data)){
      for(busId in unique(reactive_data$bus_data$Bus)){
        if(!exists(busId, where = originalBusdata)){
          busdata <- reactive_data$bus_data[reactive_data$bus_data$Bus == busId, ]
          if(is.null(originalBusdata[[busId]])){
            originalBusdata[[busId]] <- data.frame(busdata)
          }
        }
      }
    }
    removeModal()
    # PJ Note: Since we have changed the bus_data, we should update the map
    for (busid in c(BUSID1,BUSID2,BUSID3,BUSID4)){
      busrow <- busid-BUSID1+1
      #print(reactive_data$bus_data)
      if (busrow %in% reactive_data$bus_data$busID){
        if (reactive_data$bus_data$Total_Capacity[reactive_data$bus_data$busID == busrow] == 50){
          busbitmap <- "redbus.png"
        }else if(reactive_data$bus_data$Total_Capacity[reactive_data$bus_data$busID == busrow] == 30) {
          busbitmap <- "bluebus.png"
        }
        vals$sprites$img[busid] <- busbitmap
        
        # Initially the bus sprites are off the map, but when buses are added their Current_Location is set to "Hub"
        # So we detect that and reset their sprite location.
        if (reactive_data$bus_data$Current_Location[reactive_data$bus_data$busID == busrow] == "Hub"){
          vals$sprites$x_pct[busid] <- busStops$x_pct[7]+busOffsets$x_pct[reactive_data$bus_data$busID == busrow][1]
          vals$sprites$y_pct[busid] <- busStops$y_pct[7]+busOffsets$y_pct[reactive_data$bus_data$busID == busrow][1]
        }
      }else{
        # If there is no bus for this busid, set the location off the map
        vals$sprites$x_pct[busid] <- 110
        vals$sprites$y_pct[busid] <- 110
      }
    }
    vals$spriteschange <- vals$spriteschange +1
  })
  

  
  #Populate dropdown panel with BusID from Table
  observe({
    updateSelectizeInput(session, "deleteBusId", choices = c("",reactive_data$bus_data$Bus))
  })
  #React to button click event 
  observeEvent(input$deleteBuses, {
    BusId_to_remove <- input$deleteBusId
    if(length(BusId_to_remove)>0){
      hub_bus_indices <- which(reactive_data$bus_data$Current_Location %in% "Hub" & reactive_data$bus_data$Bus %in% BusId_to_remove)
      if (length(hub_bus_indices)==length(BusId_to_remove)){
        n <- which(reactive_data$bus_data[,1]==BusId_to_remove)
         print(reactive_data$bus_data[,1])
         print(n)
        if(reactive_data$bus_data$Total_Capacity[n] ==  50){
          vals$balance <- vals$balance + busdf$sellingpt_of_bus[1]
        }
        else{
          vals$balance <- vals$balance + busdf$sellingpt_of_bus[2]
        }
        removeBus(BusId_to_remove)
        removeModal()
      } else {
        showNotification("Only buses at the 'Hub' location can be removed.", type = "warning")
      }
    } else {
      showNotification("Please select one or more Buses to remove:", type = "warning")
    }
    removeModal()
    vals$spriteschange <- vals$spriteschange+1
  })
  

  # Initialize reactiveValues to store the data
  reactive_turn <- reactiveValues(turn_count = 0)
  
  output$table_output <- renderUI({
    # Check if the bus_data dataframe is empty
    if (is.null(reactive_data$bus_data) || nrow(reactive_data$bus_data) == 0) {
      # If empty, render a table with only the column headers
      table_html <- tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-striped table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Bus"),
              #tags$th("Battery"),
              tags$th(HTML("Remaining <br> Travel Time")),
              tags$th(HTML("Current <br> Capacity")),
              tags$th(HTML("Remaining <br> Capacity")),
              tags$th("Current Location")#,
              #tags$th("Next Location")
            )
          )
        )
      )
    }else{
      #Render table with data if not empty
      table_html <-tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-striped table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Bus"),
              #tags$th("Battery"),
              tags$th(HTML("Remaining <br> Travel Time")),
              tags$th(HTML("Passengers <br> On Board")),
              tags$th(HTML("Remaining <br> Capacity")),
              tags$th(HTML("Current <br> Location"))#,
              #tags$th("Next Location")
            )
          ),
          tags$tbody(
            lapply(1:nrow(reactive_data$bus_data), function(i) {
              tags$tr(
                tags$td(reactive_data$bus_data$Bus[i]),
                #tags$td(reactive_data$bus_data$Battery[i]),
                tags$td(reactive_data$bus_data$Remaining_Travel_Time[i]),
                tags$td(reactive_data$bus_data$Current_Capacity[i]),
                tags$td(reactive_data$bus_data$Remaining_Capacity[i]),
                tags$td(reactive_data$bus_data$Current_Location[i])#,
                #tags$td(reactive_data$bus_data$Next_Location[i])
              )
            })
          )
        )
      )
    }
    
    
    table_html
  })
  
  output$loggedInAs <- renderUI({
    if (is.null(vals$username))
      h2("Please log in to continue to the game.")
    else
      h2(vals$username)
  })
  
  output$time <- renderUI({
    if (!is.na(vals$time)){
      h2(vals$time)
    }else{
      showModal(endgameModal(failed=FALSE))
    }
  })
  
  output$balance <- renderUI({
    h2(vals$balance)
  })
  
  output$busstop1 <- renderUI({
    h2(nrow(vals$yellowstop))
  })
  
  output$busstop2 <- renderUI({
    h2(nrow(vals$greenstop))
  })
  
  output$busstop3 <- renderUI({
    h2(nrow(vals$purplestop))
  })
  
  output$busstop4 <- renderUI({
    h2(nrow(vals$brownstop))
  })
  
  output$busstop5 <- renderUI({
    h2(nrow(vals$greystop))
  })
  
  output$TimeBox <- renderValueBox({
    valueBox(
      paste0(vals$score_board$time), "Mins", icon = icon("clock"),
      color = "light-blue"
    )
  })
  
  output$CashBox <- renderValueBox({
    valueBox(
      paste0(vals$score_board$cash), "Cash Balance", icon = icon("money-bill-wave"),
      color = "aqua"
    )
  })

}

shinyApp(ui, server)