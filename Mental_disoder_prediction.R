# Import libraries
library(shiny)
library(data.table)
#library(randomForest)
library(rpart)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Mental Disorder Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Type yes or no')),
    textInput("feeling.nervous", 
                 label = "feeling nervous", 
                 value = ""),
    textInput("panic", 
              label = "panic:", 
              value = ""),
    textInput("breathing.rapidly", 
              label = "breathing rapidly", 
              value = ""),
    textInput("sweating", 
              label = "sweating", 
              value = ""),
    textInput("trouble.in.concentration", 
              label = "trouble in concentration", 
              value = ""),
    textInput("having.trouble.in.sleeping", 
              label = "having trouble in sleeping", 
              value = ""),
    textInput("having.trouble.with.work", 
              label = "having trouble with work", 
              value = ""),
    textInput("hopelessness", 
              label = "hopelessness", 
              value = ""),
    textInput("anger", 
              label = "anger", 
              value = ""),
    textInput("over.react", 
              label = "over react", 
              value = ""),
    textInput("change.in.eating", 
              label = "change in eating", 
              value = ""),
    textInput("suicidal.thought", 
              label = "suicidal thought", 
              value = ""),
    textInput("feeling.tired", 
              label = "feeling tired", 
              value = ""),
    textInput("close.friend", 
              label = "close friend", 
              value = ""),
    textInput("social.media.addiction", 
              label = "social media addiction", 
              value = ""),
    textInput("weight.gain", 
              label = "weight gain", 
              value = ""),
    textInput("material.possessions", 
              label = "material possessions", 
              value = ""),
    textInput("introvert", 
              label = "introvert", 
              value = ""),
    textInput("popping.up.stressful.memory", 
              label = "popping up stressful memory", 
              value = ""),
    textInput("having.nightmares", 
              label = "having nightmares", 
              value = ""),
    textInput("avoids.people.or.activities", 
              label = "avoids people or activities", 
              value = ""),
    textInput("feeling.negative", 
              label = "feeling negative", 
              value = ""),
    textInput("trouble.concentrating", 
              label = "trouble concentrating", 
              value = ""),
    textInput("blamming.yourself", 
              label = "blamming yourself", 
              value = ""),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("feeling nervous","panic","breathing rapidly","sweating",
               "trouble in concentration","having trouble in sleeping",
               "having trouble with work","hopelessness","anger","over react",
               "change in eating","suicidal thought","feeling tired",
               "close friend","social media addiction","weight gain",
               "material possessions","introvert","popping up stressful memory",
               "having nightmares","avoids people or activities",
               "feeling negative","trouble concentrating","blamming yourself"),
      
      Value = as.factor(c(input$feeling.nervous,
                             input$panic,
                             input$breathing.rapidly,
                             input$sweating,
                             input$trouble.in.concentration,
                             input$having.trouble.in.sleeping,
                             input$having.trouble.with.work,
                             input$hopelessness,
                             input$anger,
                             input$over.react,
                             input$change.in.eating,
                             input$suicidal.thought,
                             input$feeling.tired,
                             input$close.friend,
                             input$social.media.addiction,
                             input$weight.gain,
                             input$material.possessions,
                             input$introvert,
                             input$popping.up.stressful.memory,
                             input$having.nightmares,
                             input$avoids.people.or.activities,
                             input$feeling.negative,
                             input$trouble.concentrating,
                             input$blamming.yourself)),
                             
                             
      stringsAsFactors = FALSE)
    
    Disorder <- 0
    df <- rbind(df, Disorder)
    input <- transpose(df)
   
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    
    Output <- predict(model,newdata = test,type = "prob")
    #Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
