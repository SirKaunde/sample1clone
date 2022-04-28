
library(shiny)
library(ggplot2)
library(DBI)
library(dplyr)
library(RSQLite)
library(plotly)

source("modules/mod_ui.R")
source("modules/mod_server.R")

prim.conn <- dbConnect(SQLite(),"data/primary_enrollment_sctypesex.db")
crop.conn <- dbConnect(SQLite(),"data/crop.prod_db.db")


hortcrop <- dbGetQuery(crop.conn, statement = "select * from 'hf.crops.combined';")
primsumaries <- dbGetQuery(conn = prim.conn, statement = "select*from primary_long;")

# Define UI for application that draws a histogram

  ui <- navbarPage(
    
    
    "Makueni Dashboard",
                   tabPanel("Education",
                            sliderInput(inputId = "year", "Per Year", min = 2017, max = 2019, value = 2018),
                            plotOutput("lineplot"),
                            boxoutUI("education"),
                            
                            ),
                   tabPanel("Agriculture", 
                            boxoutUI("crop"),
                            
                            tags$head(
                              
                              tags$style(HTML(
                                "
      body {
        background-color: brown;
        #color: white;
           }"))
                            ),
                            ),
    
                   tabPanel("Component 3")
  )

 #   )
server <- function(input,output){
  
  output$lineplot <- renderPlot({
    primsumaries <- primsumaries %>% group_by(year,gender)%>% 
      
      summarise(total_enrollment=sum(enrolment))
    
    ggplot(primsumaries,aes(x=year,y=total_enrollment,color=gender,group=gender))+
      geom_line()+
      geom_point(size=4)+
    
      ggtitle(label = "Primary School Enrolment",subtitle = paste("For the year ",input$year))+
      labs(x="Year",y="Enrolment")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.subtitle = element_text(hjust = 0.5))
    
  }) 
  
  primsummary <- primsumaries %>% group_by(subcounty,year,gender)%>% 
   
    summarise(total_enrollment=sum(enrolment))
    
  boxOutServer(
    id = "education", 
    data = primsummary, 
    xaxis = "subcounty", 
    yaxis = "total_enrollment", 
    fill = "gender", 
    title = "Primary School Enrollment"
  )
  
  boxOutServer (
    id = "crop", 
    data = hortcrop, 
    xaxis = "subcounty", 
    yaxis = "value.Kshs", 
    fill = "crop_type", 
    title = "Value of crop per sub county"
  )
  
}

shinyApp(ui,server)