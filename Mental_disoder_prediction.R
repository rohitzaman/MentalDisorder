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
  headerPanel("Are you Depressed or in Traumatic Stress or Feeling Lonely or having Anxiety problem.Okay, Let's check !!"),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Please answer the following questions:')),
    selectInput("feeling.nervous","Do you feel nervous ?",c("yes","no"),selected = "no"),
    selectInput("panic","Do you feel panic?",c("yes","no"),selected = "no"),
    selectInput("breathing.rapidly","Are you breathing rapidly?",c("yes","no"),selected = "no"),
    selectInput("sweating","Do you have more Sweating problem?",c("yes","no"),selected = "no"),
    selectInput("trouble.in.concentration","Do you have trouble in concentration?",c("yes","no"),selected = "no"),
    selectInput("having.trouble.in.sleeping","Do you have trouble in sleeping?",c("yes","no"),selected = "no"),
    selectInput("having.trouble.with.work","Do you have trouble with work?",c("yes","no"),selected = "no"),
    selectInput("hopelessness","Do you think you are hopeless?",c("yes","no"),selected = "no"),
    selectInput("anger","Do you think you become angry so fast?",c("yes","no"),selected = "no"),
    selectInput("over.react","Do you have over reacting problem?",c("yes","no"),selected = "no"),
    selectInput("change.in.eating","Do you think you have changed in eating?",c("yes","no"),selected = "no"),
    selectInput("suicidal.thought","Do you think you have suicidal thought?",c("yes","no"),selected = "no"),
    selectInput("feeling.tired","Are you feeling tired?",c("yes","no"),selected = "no"),
    selectInput("close.friend","Do you have any close friend?",c("yes","no"),selected = "no"),
    selectInput("social.media.addiction","Do you have social media addiction?",c("yes","no"),selected = "no"),
    selectInput("weight.gain","Are you facing weight gain problem ?",c("yes","no"),selected = "no"),
    selectInput("material.possessions","Do you have material possessions ?",c("yes","no"),selected = "no"),
    selectInput("introvert","Are you introvert?",c("yes","no"),selected = "no"),
    selectInput("popping.up.stressful.memory","popping up stressful memory?",c("yes","no"),selected = "no"),
    selectInput("having.nightmares","always having nightmares?",c("yes","no"),selected = "no"),
    selectInput("avoids.people.or.activities","do you avoid people or activities?",c("yes","no"),selected = "no"),
    selectInput("feeling.negative","Are you feeling negative?",c("yes","no"),selected = "no"),
    selectInput("trouble.concentrating","Do you have trouble concentrating problem?",c("yes","no"),selected = "no"),
    selectInput("blamming.yourself","Do you blam yourself?",c("yes","no"),selected = "no"),
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
