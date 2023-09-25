# library("gsubfn")
#start game initialise


#dataframe for buses

type_of_bus <- c("50 seater bus", "30 seeater bus")
capacity_of_bus <- c(50, 30)
runtime_of_bus <- c(5,8)
cost_of_bus <- c(3000,2000)
sellingpt_of_bus <- c(1000,750)

busdf <- data.frame(type_of_bus,capacity_of_bus,runtime_of_bus,cost_of_bus,sellingpt_of_bus)
busdf

#create a dataframe for the bus stops
no_of_passengers <- 0
bs1 <- data.frame(no_of_passengers)
bs2 <- data.frame(no_of_passengers)
bs3 <- data.frame(no_of_passengers)
bs4 <- data.frame(no_of_passengers)
bs5 <- data.frame(no_of_passengers)

busqty <- c(0,0)
playerdf <- data.frame(type_of_bus,busqty)
playerdf

#generating random number of passengers
genpassengers <- function(timeingame) {
  # set.seed(as.numeric(Sys.time()))
  # set.seed(10)
  passenger <- c()
  #time 930am-11am no of passengers mean 10
  #for (timeingame = timedf[])
if (any(timeingame==timedf[c(5:10,17:22),])){
  passenger1 <- round(runif(5, min=10, max=20))
  passenger <- c(passenger1)
  return(passenger)
}

else{
  passenger1 <- round(runif(5, min=0, max=10))
  passenger <- c(passenger1)
  return(passenger)
}
  
  
}
#time frame

# genpassengers("09:00 AM")

# Define the start time
start_time <- as.POSIXlt("09:00:00", format = "%H:%M:%S")

# Vector to store the updated times

time_vector <- c(format(start_time, format = "%I:%M %p"))
# Loop to add 30 minutes for 26 iterations
for (i in 1:24) {
  
  # Add 30 minutes to the start time
  start_time <- start_time + minutes(30)
  
  # Append the updated time to the vector
  time_vector <- c(time_vector, format(start_time, format = "%I:%M %p"))
  
}

# Create the dataframe
timedf <- data.frame(Time = time_vector)
timedf

timebutton <- function(n){
  
  return(timedf[n,])
}


#quantity = no. of buses, n = row 1 or 2
buybuses <- function(quantity, n, playerdf, balance){
  #check if input is positive and if the cost is less than avail balance and n is a string
  #if (quantity > 0 & quantity*busdf$cost_of_bus[n] <= budget) {
    #if yes, add the input to the no. of buses player has
    playerdf$busqty[n] <- playerdf$busqty[n] + quantity
    #deduct the cost from the avail balance
    balance <- balance - quantity*busdf$cost_of_bus[n]
    print(playerdf)
    print(balance)
    # Return both the modified playerdf and budbal
    return (list(dataframe = playerdf, budget = balance))
    
}


sellbuses <- function(quantity, n, playerdf, balance){
  # if (quantity > 0 &  quantity <= playerdf$busquantity[n]){
    playerdf$busqty[n] <- playerdf$busqty[n] - quantity
    balance <- balance + quantity*busdf$sellingpt_of_bus[n]
    print(playerdf)
    print(balance)
    return (list(dataframe = playerdf, budget = balance))
    
}


