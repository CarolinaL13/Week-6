library(shiny)
library(tidyverse)
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

 state <- covid19 %>% 
  select(state) %>% 
  distinct(state) %>% 
  dplyr::arrange(state) %>% 
  pull(state)


ui <- fluidPage(selectInput(inputId = "state", 
                            label = "State(s)", 
                            choices = state,
                            multiple = TRUE),
                submitButton("Create!"),
                plotOutput(outputId = "CovidPlot"),
  
)

server <- function(input, output) {
  output$CovidPlot <- renderPlot({
    covid19 %>% 
      group_by(state) %>% 
      filter(cases>19) %>% 
      arrange(state, cases) %>% 
      group_by(state) %>% 
      mutate(dayssince20 = date - min(date)) %>% 
      filter(state %in% input$state) %>% 
      ggplot(aes(x = dayssince20, y = cases, color = state))+
      geom_line()+
      scale_y_log10()+
      labs (y = "Cumulative Cases", x = "Days Since 20+ Cases", title = "Cumulative Cases Per State")
    })
    
}
  
shinyApp(ui = ui, server = server)
