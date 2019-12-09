

library(shiny)
library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
library("caret")
library("data.table")
library("xgboost")
library(kohonen)

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
  
  output$algorithm <- renderTable({
    
    df <- setDT(data(), keep.rownames=TRUE)
    
    # data partition for CV
    folds <- createFolds(df$target,3)
    
    #xgboost param
    xgb_param <- list(booster = "gbtree", 
                      max.depth = 3, 
                      eta = 0.07 ,
                      gamma = 1, 
                      subsample = 0.6, 
                      colsample_bytree = 0.5, 
                      min_child_weight = 3, 
                      objective = "binary:logistic", 
                      eval_metric = "error"
                      
    )
    
    downsample_func <- function(dt,label,coef){
      
      pos_cnt <- sum( ifelse(label==1,1,0) )
      neg_cnt <- sum( ifelse(label==0,1,0) )
      add_objects <- neg_cnt- floor( neg_cnt-(pos_cnt/coef) )
      
      dt$target <- label
      dt_to_sample <- dt[target==0,]
      sample_ind <- sample( 1:nrow(dt_to_sample),add_objects,replace=F )
      
      dt_to_sample <- dt_to_sample[sample_ind, ]
      
      data <- rbindlist (list ( dt_to_sample, dt[target==1,]  ),use.names = T)
      
      res <- list()
      
      res[["data"]] <- data[,-c("target"),with=F]
      res[["label"]] <- data$target
      
      return(res)
    }
    
    
    
    
    oversample_func <- function(dt,label,coef){
      
      pos_cnt <- sum( ifelse(label==1,1,0) )
      neg_cnt <- sum( ifelse(label==0,1,0) )
      add_objects <- floor( neg_cnt*coef-pos_cnt)
      
      dt$target <- label
      dt_to_sample <- dt[target==1,]
      sample_ind <- sample( 1:nrow(dt_to_sample),add_objects,replace=T )
      
      dt_to_sample_new <- dt_to_sample[sample_ind, ]
      dt_to_smaple_new <- rbindlist(list(dt_to_sample_new,dt[target==1,]),use.names = T)
      remove(dt_to_sample)
      
      data <- rbindlist(list(dt_to_smaple_new, dt[target==0,]),use.names = T)
      
      remove(dt_to_sample_new, sample_ind)
      
      ind <- sample( 1:nrow(data),nrow(data),replace=F )
      data <- data[ind,]
      
      
      res <- list()
      
      res[["data"]] <- data[,-c("target"),with=F]
      res[["label"]] <- data$target
      
      remove(data, ind)
      
      return(res)
    }
    
    
    resample_data <- function(dt,labels,type,coef){
      
      
      coef <- coef
      res <- list()
      
      type <- type
      
      print(type)
      print(coef)
      
      if (type=='none'){
        res[["data"]] <- dt
        res[["labels"]] <- labels
      }
      
      if (type=='UR'){
        
        resample <- downsample_func (dt,labels,coef)
        res[["data"]] <- resample[[1]]
        res[["labels"]] <- resample[[2]]
      }
      
      if (type=='OR'){
        
        resample <- oversample_func (dt,labels,coef)
        res[["data"]] <- resample[[1]]
        res[["labels"]] <- resample[[2]]
      }
      
      return(res)
    }
    
    #cross-validation function
    
    xgboost_classifier_cv_server <- function(dataset,labels_class,folders,pred_cutoff,param,
                                             n_iter,f1_beta,resampling_type,resampling_coef){
      
      accuracy_train <- c()
      kappa_train <- c()
      recall_train <- c()
      precision_train <- c()
      f1_score_train <- c()
      
      accuracy_test <- c()
      kappa_test <- c()
      recall_test <- c()
      precision_test <- c()
      f1_score_test <- c()
      
      for ( i in 1:length(folders)){
        
        test_index <- folders[[i]]
        train_index <- setdiff( seq(1,nrow(dataset)),folders[[i]] )
        
        train_data <- dataset[train_index]
        train_label <- labels_class[train_index]
        train_data_resampling <- resample_data(train_data,train_label,resampling_type,resampling_coef)
        train_data <- train_data_resampling[[1]]
        train_label <- train_data_resampling[[2]]
        
        remove(train_data_resampling)
        
        test_data <- dataset[test_index]
        test_label <- labels_class[test_index]
        
        
        xgb_train <- xgb.DMatrix((as.matrix(train_data)),label = train_label
        )
        
        xgb_test <- xgb.DMatrix((as.matrix(test_data)),label = test_label
        )
        
        set.seed(1567)
        xgb_train_model <- xgb.train(data=xgb_train, params=param,  nrounds=n_iter,
                                     maximize = F
        )
        
        
        train_predict_xgboost <- predict(xgb_train_model,xgb_train)
        train_pred_class <- ifelse(train_predict_xgboost >= pred_cutoff,1,0)
        
        test_predict_xgboost <- predict(xgb_train_model,xgb_test)
        
        test_pred_class <- ifelse(test_predict_xgboost >= pred_cutoff,1,0)
        
        train_result <- confusionMatrix( factor(train_pred_class,levels = 0:1),
                                         factor( train_label,levels=0:1),
                                         positive = "1")
        
        test_result <- confusionMatrix( factor(test_pred_class,levels = 0:1),
                                        factor( test_label,levels=0:1),
                                        positive = "1")
        
        tn_train <- c(train_result$byClass[2])
        accuracy_train <- c(accuracy_train, train_result$overall[1])
        kappa_train <- c(kappa_train,train_result$overall[2])
        recall_train <- c(recall_train,  train_result$byClass[6])
        precision_train <- c(precision_train, train_result$byClass[5]) 
        f1_score_train <- c(f1_score_train, 
                            ( (1+f1_beta^2)*precision_train*recall_train) /( (( f1_beta^2)*precision_train)+recall_train  ) )
        
        tn_test <- c(test_result$byClass[2])
        accuracy_test <- c(accuracy_test, test_result$overall[1])
        kappa_test <- c(kappa_test,test_result$overall[2])
        recall_test <- c(recall_test,  test_result$byClass[6])
        precision_test <- c(precision_test, test_result$byClass[5]) 
        f1_score_test <- c(f1_score_test, 
                           ( (1+f1_beta^2)*precision_test*recall_test) /( (( f1_beta^2)*precision_test)+recall_test  ) )
      }
      
      res_cv <- c(mean(accuracy_train), mean(kappa_train), mean(recall_train), mean(precision_train), mean(tn_train),  mean(f1_score_train),
                  mean(accuracy_test), mean(kappa_test), mean(recall_test), mean(precision_test), mean(tn_test),mean(f1_score_test))
      names(res_cv) <- c("accuracy train","kappa train","recall train", "precision train","true negative train", "f1 score train",
                         "accuracy test","kappa test","recall test", "precision test","true negative test","f1 score test")
      
      res <- list()
      res[["metric_result"]] <- res_cv[7:12]
      # 
      #res[["recall_test"]] <- recall_test
      #res[["test_res"]] <- 
      #res[["metric_result"]] <- res_cv[7:12]
      #res
      # 
      #test_result
      # res
      return(res)
      
    }
    
    
    
    #None, down, over
    resample_type <-input$sampling
    resample_coef <- input$inbCoeff
    pred_cut <- 0.5
    n_iter <- 50
    f1_coef <-1 
    
    
    
    xgb_cv_test <- xgboost_classifier_cv_server(
      df[,-c("ID_code","target"), with=FALSE],
      df$target,
      folds,
      pred_cut,
      xgb_param,
      n_iter,
      f1_coef,
      resample_type,
      resample_coef
    )
    
    #print(xgb_cv_test$metric_result)
    
    res_print <- as.data.frame(xgb_cv_test$metric_result)
    rownames(res_print) <- c("accuracy test","kappa test","recall test","precision test","true negative test","f1 score test ")
    
    res_print
    
    
    
    
    ####################################
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
    data_train_matrix <- as.matrix(scale(as.matrix(data()[,-c("ID_code","target"),with=F])))
    
    som_grid <- somgrid(xdim = 4, ydim=3, topo="hexagonal")
    
    som_model <- som(data_train_matrix, 
                     grid=som_grid, 
                     #rlen=500, 
                     rlen=250,
                     alpha=c(0.01), 
                     keep.data = TRUE )
    
    
    coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
    
    plot(som_model, type = "property", property = getCodes(som_model)[,4], main=colnames(getCodes(som_model))[4], palette.name=coolBlueHotRed)
    
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Upload data to start")
    else
      tabsetPanel(
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Summary", tableOutput("sum")), 
                  tabPanel("Histogram", plotOutput("distPlot")),
                  tabPanel("Imbalance coef", tableOutput("result")),
                  tabPanel("Algorithm", tableOutput("algorithm")),
                  tabPanel("Predict", tableOutput("predict")))
  })
})
