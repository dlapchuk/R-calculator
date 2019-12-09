library(shiny)
shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 600 MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
      selectInput("y_input", label = "Select Target Variable",""),
      selectInput("sampling", "Sampling Method:",
                  c("None" = 'none',
                    "Undersampling: Random" = "UR",
                    "Undersampling: EasyEnsemble" = "UEE",
                    "UnderSampling: BalanceCascade" = "UBC",
                    "UnderSampling: NearMiss-1" = "UNM1",
                    "UnderSampling: NearMiss-2" = "UNM2",
                    "UnderSampling: NearMiss-3" = "UNM3",
                    "OverSampling: Random" = "OR",
                    "OverSampling: SMOTE" = "OS",
                    "OverSampling: BorderLine SMOTE" = "OBLS",
                    "OverSampling: ADASYN" = "OA",
                    "OverSampling: SL SMOTE" = "OSLS",
                    "OverSampling: MWMOTE" = "OM")),
      sliderInput("inbCoeff", "Desired inbalanced coefficient:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      selectInput("model", "Model:",
                  c("None" = "none",
                    "RandomForest" = "RF",
                    "Xgboost" = "xgboost",
                    "LogisticRegression" = "LG",
                    "KNN" = "knn")),
      fileInput("pred_file","Upload predict data"),
      actionButton("export", "Export to", style='width:230px;')
      ),
    mainPanel(
      uiOutput("tb")
      
# use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
#       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
#                   tabPanel("Data", tableOutput("table")), tabPanel("Algorithm", tableOutput("algorithm")))
      )
    
    )
  ))