library(shiny)
library(plotly)
library(feasts)

monthly.series <- readRDS("timeseries.RDS")

ui <- fluidPage(
  titlePanel("Vancouver Police Department Reported Crime Incidents Visualization Tool"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This user-interactive app shows the crimes reported in Vancouver by neighbourhood from January 2015 to March 2021.",
               br(),
               br(),
               div("Select the type of offence and the neighbourhood(s) you wish to see.", style = "color:blue")),
      
      selectInput("offence",
                  label = "Type of Offence",
                  choices = c("Assaults", "Sex Offences", "Robbery",
                              "B&E", "Theft of MV", "Theft from Auto",
                              "Theft<>$5K", "Arson", "Mischief", "Offensive Weapons")),
      
      checkboxGroupInput("neighbourhood",
                         label = "Neighbourhood",
                         choices = c(unique(monthly.series$Neighbourhood)[-5], "All Neighbourhoods"),
                         selected = "All Neighbourhoods"),
      
      helpText("Author: Wendy Ngan",
               br(),
               "Data Source: The Vancouver Police Department (VPD)",
               br(),
               a("https://vpd.ca/police/organization/planning-research-audit/neighbourhood-statistics.html"))
    ),
    
    mainPanel(
      br(),
      br(),
      plotlyOutput("line")
    )
  )
)

server <- function(input, output) {
  data_input <- reactive({
    validate(
      need(input$neighbourhood, "Please select one or more neighbourhoods")
    )
    return(monthly.series[(monthly.series$Offence == input$offence &
                          monthly.series$Neighbourhood %in% input$neighbourhood),])
  })
  output$line <- renderPlotly({
    p <- ggplot(data = data_input(), aes(x = as.Date(Month), y = Count, colour = Neighbourhood,
                                         text = paste("Date: ", as.Date(Month),
                                                      "<br>Cases: ", Count,
                                                      "<br>Neighbourhood:", Neighbourhood),
                                         group = Neighbourhood)) +
      geom_line() +
      geom_point(size = 1) +
      ggtitle(paste("Reported", input$offence, "in Selected Neighbourhoods")) +
      scale_x_date("Year", date_labels = "%Y", date_breaks = "year")
    ggplotly(p, tooltip = "text", height = 700)
  })
}
shinyApp(ui, server)
