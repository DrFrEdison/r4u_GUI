library("shiny")
library(this.path)
library(r4dt)
setwd(this.dir())


dt <- list()
dt$custom.list <- r4dt::dt_customer
dt$custom.list$LG <- gsub("LG SG", "SG", paste("LG", dt$custom.list$LG))
dt$custom.list$LG <- factor(dt$custom.list$LG, levels = c("LG 3", "LG 2", "SG", "LG 1"))
dt$custom.list$customer <- factor(dt$custom.list$customer, levels = c("CCEP", "MEG", "Pepsi", "CapriSun"))
dt$product.ID <- r4dt::dt_customer_product_ID
# custom <- dt$custom.list
product.ID <- dt$custom.list

# UI ####
ui <- fluidPage(
  titlePanel("Read, Inspect and Download LiquiGuard Data"),
  
  sidebarPanel(
    # selectizeInput(
    #   "line"
    #   , h3("Line")
    #   , choices = dt$custom.list$line[order(dt$custom.list$LG)]
    #   , options = list(
    #     placeholder = 'Please select an option below',
    #     onInitialize = I('function() { this.setValue(""); }')
    #   )
    # )
    
    selectizeInput(
      "line"
      , h3("Line")
      , choices = dt$custom.list$line[order(dt$custom.list$LG)]
      , selected = "G2"
    )
    
    # List for Location
    , selectizeInput(
      "location"
      , h3("Location")
      , choices = dt$custom.list$location[order(dt$custom.list$LG)]
      , options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    
    # Buttons for LG
    , radioButtons("LG", label = h3("LiquiGuard X")
                   , choices = levels(dt$custom.list$LG)
                   , selected = as.character(0))
    
    # Buttons for customer
    , radioButtons("customer", label = h3("Customer")
                   , choices = levels(dt$custom.list$customer), selected = as.character(0))
    
    # Date Menu
    , dateRangeInput("date", h3("Date range")
                     , start = "2022-03-01" #Sys.Date()-10
                     , end = "2022-03-18"
                     , min = as.POSIXct(paste0(year(Sys.Date()) - 2, "-01-01"))
                     , weekstart = 1)
    
    # Type of spectra
    , checkboxGroupInput("type", label = h3("Type of spectra") 
                         , choices = list("production spc" = "spc", "background spc" = "ref", "dark spc" = "drk")
                         , selected = c("ref", "drk"))
  )
  
  , mainPanel(
    
    img(src = "logo.png")
    , textOutput("overview.txt")
    , textOutput("overview.date")
    
    , actionButton("ref", label = "Background spectra")
    , actionButton("drk", label = "Dark spectra")
    , actionButton("ms", label = "Integration Time")
    , actionButton("acc", label = "Accumulations")
    , actionButton("download", label = "Download csv")
    
    , plotOutput("spc", width = "80%", height = "800px")
    
  )
  
  
)

# Server ####
server <- function(input, output) {
  
  # Output Text summary
  output$overview.txt <- renderText({paste0(input$location
                                            , " ", input$line
                                            , " (", input$LG
                                            , ", ", input$custom, ")"
                                            , " Product number NA")})
  
  output$overview.date <- renderText({paste0("Date range from "
                                             , paste0(wday(input$date[1], label = T, abbr = T, week_start = 1, locale = "English"), ", "
                                                      , format(input$date[1], format="%d %b '%y", locale = "English"))
                                             , " to "
                                             , paste0(wday(input$date[2], label = T, abbr = T, week_start = 1, locale = "English"), ", "
                                                      , format(input$date[2], format="%d %b '%y", locale = "English")))})
  
  # update by line
  update.line <- reactive({
    dplyr::filter(dt$custom.list, line %in% input$line)
  })
  observeEvent(update.line(), {
    choices.location <- update.line()$location
    choices.LG <- update.line()$LG
    choices.custom <- update.line()$customer
    updateSelectizeInput(inputId = "location", selected = update.line()$location)
    updateRadioButtons(inputId = "LG", selected = update.line()$LG)
    updateRadioButtons(inputId = "customer", selected = update.line()$customer)
  })
  # 
  # # update by location
  # update.location <- reactive({
  #   dplyr::filter(dt$custom.list, location %in% input$location)
  # })
  # observeEvent(update.location(), {
  #   choices.line <- update.location()$line
  #   choices.LG <- update.location()$LG
  #   choices.custom <- update.location()$custom
  #   updateSelectizeInput(inputId = "line", selected = update.location()$line)
  #   updateRadioButtons(inputId = "LG", selected = update.location()$LG)
  #   updateRadioButtons(inputId = "custom", selected = update.location()$custom)
  # })
  
  
  # Download csv ####
  observeEvent(input$download, {
    message("running script.R")
    LG.csv <- read.csv.LG(firstday = input$date[1]
                          , lastday = input$date[2]
                          , customer = input$customer
                          , location = input$location
                          , line = input$line
                          , product = NA
                          , typecode = NA
                          , Ringkessel = T
                          , typeof = input$type
                          , slim = T
                          , return.R = T
                          , product_ID = product.ID
                          , customer.list = dt$custom.list
                          , export_directory = wd$csvtemp)
  })
  
  # Plot drk ####
  observeEvent(input$drk, {
    message("Plot drk")
    LG.csv <- read.csv.LG(firstday = input$date[1]
                          , lastday = input$date[2]
                          , customer = input$customer
                          , location = input$location
                          , line = input$line
                          , product = NA
                          , typecode = NA
                          , Ringkessel = T
                          , typeof = "drk"
                          , slim = T
                          , return.R = T
                          , product_ID = product.ID
                          , customer.list = dt$custom.list
                          , export_directory = wd$csvtemp)
    
    output$spc <- renderPlot({
      
      LG.csv$spc.trs <- transfer_csv.num.col(LG.csv$drk)
      
      par(cex.main = 3, cex.lab = 2, cex.axis = 2, mar = c(6,6,6,1))
      layout(matrix(c(1,2,3), ncol = 1), heights = c(1,.5,.5))
      matplot(LG.csv$spc.trs$wl
              , LG.csv$spc <- t(LG.csv$drk[ , LG.csv$spc.trs$numcol , with = F])
              , type = "l", lty = 1
              , xlab = lambda, ylab = "Counts"
              , col = LG.csv$col <- viridis::viridis(ncol(LG.csv$spc))
              , main = paste0("Dark spectra in ", input$location, ", line ", input$line))
      
      plot(LG.csv$drk$datetime
           , as.numeric(unlist(LG.csv$drk[ , unique( grep( paste( c( "integration", "Integrationszeit" ), collapse="|"), names(LG.csv$drk), value=T)), with = F]))
           , xlab = "", ylab = "Integration time in ms", main = "Integration time", col = LG.csv$col)
      plot(LG.csv$drk$datetime
           , as.numeric(unlist(LG.csv$drk[ , unique( grep( paste( c( "accumulation", "Mittelungen" ), collapse="|"), names(LG.csv$drk), value=T)), with = F]))
           , xlab = "", ylab = "Accumulations", main = "Accumulations", col = LG.csv$col)
      
    })
    
  }
  )
  
  # Plot ref ####
  observeEvent(input$ref, {
    message("Plot ref")
    LG.csv <- read.csv.LG(firstday = input$date[1]
                          , lastday = input$date[2]
                          , customer = input$customer
                          , location = input$location
                          , line = input$line
                          , product = NA
                          , typecode = NA
                          , Ringkessel = T
                          , typeof = "ref"
                          , slim = T
                          , return.R = T
                          , product_ID = product.ID
                          , customer.list = dt$custom.list
                          , export_directory = wd$csvtemp)
    
    output$spc <- renderPlot({
      
      LG.csv$spc.trs <- transfer_csv.num.col(LG.csv$ref)
      
      par(cex.main = 2, cex.lab = 2, cex.axis = 2, mar = c(6,10,6,1))
      layout(matrix(c(1,2,3), ncol = 1), heights = c(1,.5,.5))
      matplot(LG.csv$spc.trs$wl
              , LG.csv$spc <- t(LG.csv$ref[ , LG.csv$spc.trs$numcol , with = F])
              , type = "l", lty = 1
              , xlab = "", ylab = ""
              , col = LG.csv$col <- viridis::viridis(ncol(LG.csv$spc))
              , main = paste0("Background spectra in ", input$location, ", line ", input$line)
              , axes = F)
      axis(1)
      axis(2, las = 1)
      mtext("Counts", 2, 8, cex = 1.5)
      mtext(lambda, 1, 4, font = 2, cex = 1.25)
      
      plot(LG.csv$ref$datetime
           , as.numeric(unlist(LG.csv$ref[ , unique( grep( paste( c( "integration", "Integrationszeit" ), collapse="|"), names(LG.csv$ref), value=T)), with = F]))
           , xlab = "", ylab = "", main = "Integration time", col = LG.csv$col, axes = T, yaxt = "n"
           , pch = 20, cex = 1.5)
      axis(2, las = 1)
      mtext("Integration time in ms", 2, 8, cex = 1.25)
      
      plot(LG.csv$ref$datetime
           , as.numeric(unlist(LG.csv$ref[ , unique( grep( paste( c( "accumulation", "Mittelungen" ), collapse="|"), names(LG.csv$ref), value=T)), with = F]))
           , xlab = "", ylab = "", main = "Accumulations", col = LG.csv$col, axes = T, yaxt = "n"
           , pch = 20, cex = 1.5)
      axis(2, las = 1)
      mtext("Accumulations", 2, 8, cex = 1.25)
      
    })
    
  }
  )
}

# Stop R Session when Desktop App is closed ####
shinyServer(function(input, output, session){
  session$onSessionEnded(function() {
    stopApp()
  })
})


# Run the app ----
shinyApp(ui = ui, server = server)
