library(shiny)

data<- read.csv("data/data.csv")
langoptions <- unique(sort(data$Langs, desc=TRUE)) ## To get the list of languages for the pull down menu
langoptions <- sort(langoptions) 

ui <- fluidPage(
  titlePanel("Language Distributions in the USA"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("langID", "Language", choices = langoptions),
      radioButtons("dataID", "Data Type",  choices = c("percent", "speakers"), selected = "speakers"),width = 2),
  
    mainPanel(
                fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))),
              fluidRow(
                splitLayout(cellWidths = c("50%","50%"), tableOutput("results1"), tableOutput("results2")))
    
    )
  )
)



server <- function(input, output, session) {
  reduced_df2013 <- reactive({
    filter(
      data,
      Langs == input$langID,
      dataType == input$dataID,
      year == "2013"
    )
    
    })
  reduced_df2008 <- reactive({
    filter(
      data,
      Langs == input$langID,
      dataType == input$dataID,
      year == "2008"
    )
  })
  
  output$plot1 <- renderPlot({
    validate(
      need(!is.na(reduced_df2013()), "Insufficient or no reported data for the 2009-2013 Survey"
      )
    )
  state_choropleth(reduced_df2013(), title = paste0("Where ", input$langID," speakers can be found [2009-2013 ACS Survey]"),legend = input$dataID, num_colors = 7)
      })
  output$plot2 <- renderPlot({
    validate(
      need(!is.na(reduced_df2008()), "Insufficient or no reported data for the 2006-2008 Survey"
      )
    )
    state_choropleth(reduced_df2008(), title = paste0("Where ", input$langID, " speakers can be found [2006-2008 ACS Survey]"), legend = input$dataID, num_colors = 7)
  })
  
  output$results1 <- renderTable({
    reduced_df2013()[order(-reduced_df2013()$value), c("region","value")]

  }, 
  inputdigits = 0)
    
  output$results2 <- renderTable({

  reduced_df2008()[order(-reduced_df2008()$value), c("region","value")]
  
  },
  inputdigits = 0)
}

shinyApp(ui = ui, server = server)

