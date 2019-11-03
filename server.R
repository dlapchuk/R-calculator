library(shiny)
library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
options(shiny.maxRequestSize = 600*1024^2)

shinyServer(function(input,output,session){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    fread(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    summary(input$file)
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
     })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()[1:100,]
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()[1:100,]
  })
  
  output$result <- renderTable({
    #paste("You chose", input$y_input)
    table( data()[, c(input$y_input), with=FALSE ] )
  })
  
  observe({
    updateSelectInput(
      session,
      "y_input",
      choices=names(data()))
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- dt$sepal.length
    #bins <- seq(min(x), max(x), length.out = 5)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    data() %>%
      keep(is.numeric) %>% 
      gather() %>% 
      ggplot(aes(value)) +
      facet_wrap(~ key, scales = "free") +
      geom_histogram()
    
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Upload data to start")
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Summary", tableOutput("sum")), 
                  tabPanel("Histogram", plotOutput("distPlot")),
                  tabPanel("Imbalance coef", tableOutput("result")))
  })
})