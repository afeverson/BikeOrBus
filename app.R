library(shiny)
library(tidyverse)
library(readr)

##chicago historic bus and weather data
chi_bus <- read_csv("busdata.csv",
                    col_types = cols(date = col_date(format = "%m/%d/%Y")))
chi_weather <- read_csv("weatherdata.csv",
                        col_types = cols(date = col_date(format = "%m/%d/%Y")))
chi_weather$Events[is.na(chi_weather$Events)] <- 'Nothing'
chi_weather$DoW <- weekdays(chi_weather$date)
chi_data <- left_join(chi_bus, chi_weather, by = 'date')

##misery index for weather
chi_data$n_temp <- 1 - (chi_data$Temp_Avg - min(chi_data$Temp_Avg))/(max(chi_data$Temp_Avg)-min(chi_data$Temp_Avg))
chi_data$n_precip <- (chi_data$Precip_in - min(chi_data$Precip_in))/(max(chi_data$Precip_in)-min(chi_data$Precip_in))
chi_data$n_wind <- (chi_data$Wind_Avg_mph - min(chi_data$Wind_Avg_mph))/(max(chi_data$Wind_Avg_mph)-min(chi_data$Wind_Avg_mph))
chi_data$m_comp <- (40*chi_data$n_temp)+(40*chi_data$n_precip)+(20*chi_data$n_wind)
chi_data$w_misery <- 100*(1-((chi_data$m_comp - min(chi_data$m_comp))/(max(chi_data$m_comp) - min(chi_data$m_comp))))


##current weather at MDW for predicting bus traffic today
yr <- substr(Sys.Date(),1,4)
mth <- substr(Sys.Date(),6,7)
dy <- substr(Sys.Date(),9,10)
w_url <- paste("https://www.wunderground.com/history/airport/MDW/",
               yr,"/",mth,"/",dy,
               "/CustomHistory.html?dayend=",
               dy,
               "&monthend=",
               mth,
               "&yearend=",
               yr,
               "&req_city=NA&req_state=NA&req_statename=NA&format=1",sep="")
weather_now <-  read_csv(url(w_url),skip=1)[c(1,2,3,4,17,20,22)]
colnames(weather_now) <- c('date','Temp_High','Temp_Avg','Temp_Low','Wind_High_mph','Precip_in','Events')
weather_now$DoW <- weekdays(Sys.Date())
weather_now$Precip_in <- (if(weather_now$Precip_in==T) {
  0.001
}
else {
  weather_now$Precip_in
})

#current weather misery index
m_temp <- 1 - (weather_now$Temp_Avg - min(chi_data$Temp_Avg))/(max(chi_data$Temp_Avg)-min(chi_data$Temp_Avg))
m_precip <- (weather_now$Precip_in - min(chi_data$Precip_in))/(max(chi_data$Precip_in)-min(chi_data$Precip_in))
m_wind <- (weather_now$Wind_High_mph - min(chi_data$Wind_Avg_mph))/(max(chi_data$Wind_Avg_mph)-min(chi_data$Wind_Avg_mph))
m_comp <- (40*m_temp)+(40*m_precip)+(20*m_wind)
misery_now <- 100*(1-((m_comp - min(chi_data$m_comp))/(max(chi_data$m_comp) - min(chi_data$m_comp))))

# Define UI for application that draws a histogram
ui <- navbarPage(
   theme="bootstrap.css",
   
   "BIKE OR BUS",
   tabPanel("App",
      fluidRow(
        column(4,
        wellPanel(
          tags$h1("Make Your Selections",align="center"),
        selectInput("bus_route",
                    label="Pick Your Route",
                     choices=chi_data %>%
                       select(route) %>%
                       distinct %>%
                       arrange(as.numeric(route)),
                       selected=8),
        selectInput("tolerance",
                    label="How Tolerant of Bad Weather are you?",
                    choices=c("Not","Normal","Adventurous","Hardcore"),
                    selected="Normal"),
        style="height: 255px"
        )),
        column(4,
        wellPanel( #this is for the current weather
          tags$h1("Today's Weather",align="center"),
          tags$h3(textOutput("avgT"),align="center"),
          tags$h4(textOutput("hlT"),align="center"),
          tags$h4(textOutput("precip"),align="center"),
          tags$h4(textOutput("events"),align="center"),
          style="height: 255px"
        )),
        column(4,
        wellPanel(
          tags$h1("Predictions",align="center"),
          tags$h3(textOutput("predRides"),align="center"),
          tags$h4(textOutput("avg_rides"),align="center"),
          tags$h4(textOutput("bus_misery"),align="center"),
          tags$h4(textOutput("weather_mis"),align="center"),
          style="height: 255px"
        )
        )
       ),
       wellPanel(
         tags$h1("Suggestion:"),
         tags$h2(textOutput("what_to_do"),
         style="height: 100px")
       ),
      tags$footer("Powered by ",tags$a("Shiny",
                                       href="http://shiny.rstudio.com",
                                       target="_blank"),
                  ". Theme from ",tags$a("Bootswatch",
                                         href='http://bootswatch.com',
                                         target="_blank"),".",align = "right", style = "position:absolute;
                  height:0")
     ),
   
   tabPanel("About",
    tags$h2("This is a tool to decide whether to ride the bus or your bike - whatever the weather!",align="center"),
    tags$p("Hi, I'm Gus!  I created this tool using data from Weather Underground and the City of Chicago's open data portal.
           The data was combined and a general linear model was used to estimate the rider volume of a specified route
           given the daily weather measured by ORD.  The misery indexes are a normailization of the volume of bus ridership and combination of weather factors.
           'Toughness' levels are simply multipliers and parameters added on to the base model.  For a more detailed explanation visit my Medium page",
           tags$a("here",href="https://medium.com/@augustfeverson/when-should-i-ride-the-bus-in-chicago-21a4bc26e033",target="_blank"),
           "."),
    tags$p("More tweaking will be done to make this more accurate/relevant as time goes by."),
    tags$br(),
    tags$br(),
    tags$div(tags$img(src="City-Flag.jpg",align="middle"), style="text-align: center;"),
    tags$br(),
    tags$br(),
    tags$p("Thanks so much for stopping by!"),
    tags$br(),
    
    tags$footer("Powered by ",tags$a("Shiny",
                                     href="http://shiny.rstudio.com",
                                     target="_blank"),
                ". Theme from ",tags$a("Bootswatch",
                                       href='http://bootswatch.com',
                                       target="_blank"),".",align = "right", style = "position:absolute;
                height:5")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  inputData <- reactive({
    chi_data %>%
      filter(route==input$bus_route)
  })
  
  predRides <- reactive({
    sel_route <- inputData()
    mdl <- glm(rides ~ DoW + Precip_in + Temp_High + Temp_Low + Wind_High_mph, data = sel_route)
    weather_now$pred <- predict(mdl,weather_now)
    as.integer(weather_now$pred)
  })
  
  predMisery <- reactive({
    sel_route <- inputData()
    100*(predRides() - min(sel_route$rides))/(max(sel_route$rides) - min(sel_route$rides))
  })
  
  inputTough <- reactive({
    input$tolerance
  })
  
  output$avgT <- renderText({
    paste("Average Temp: ",weather_now$Temp_Avg)
  })
  
  output$hlT <- renderText({
    paste("High: ",weather_now$Temp_High,"    Low: ",weather_now$Temp_Low)
  })
  
  output$precip <- renderText({
    paste("Precipitation (in): ",weather_now$Precip_in)
  })
  
  output$events <- renderText({
    paste("Weather Events: ",weather_now$Events)
  })
  
  output$predRides <- renderText({
    paste("Estimated Rider Volume: ", as.numeric(predRides()))
  })
  
  output$bus_misery <- renderText({
    paste("Bus Crowd Misery Index: ", as.integer(predMisery()))
  })
  
  output$weather_mis <- renderText({
    paste("Weather Misery Index: ", as.integer(misery_now))
  })
  
  output$avg_rides <- renderText({
    sel_route <- inputData()
    paste("Average Rider Volume: ",as.integer(mean(sel_route$rides)))
  })
  
  output$what_to_do <- renderText({
    if(misery_now > 85 && predMisery() > 85) {
      "YOu may want to just take an UBER."
    }
    else {
      if(inputTough()=="Not") {
        if(weather_now$Precip_in > 0.05 || weather_now$Temp_Avg < 50) {
          "Hop on the bus you wimp."
        }
        else {
          if(misery_now*1.10 < predMisery()) {
            "What a nice day for a bike ride."
          }
          else {
            "Do you even own a bike?"
          }
        }
      }
      else if(inputTough()=="Normal") {
        if(weather_now$Temp_Avg < 32) {
          "Proabably a bit too chilly to bike ride."
        }
        else if(misery_now < predMisery()) {
          "Get off your ass and ride your bike."
        }
        else{
          "Ride the bus!"
        }
      }
      else if(inputTough()=="Adventurous") {
        if(misery_now*0.90 < predMisery()) {
          "Ride your bike!"
        }
        else {
          "The bus is probably the better decision today."
        }
      }
      else {
        "You're probably going to ride your bike no matter what this says."
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

