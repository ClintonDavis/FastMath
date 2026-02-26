#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)      #boostrap
library(shinyjs)


score = 0

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fast Math"),
    


    # Sidebar with a slider input for number of bins 
    selectInput("type",
        "Type of Math:",
        c("Addition", "Subtraction", "Multiplication", "Division"),
        "Addition"
        ),
    
    actionButton("new", "New Problem"),     
    
     htmlOutput("equation"),
     
     textInput("response", "Response", value = "", updateOn = "blur"),
     
     htmlOutput("reply"),

    htmlOutput("score"),
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$score <- renderText(paste("Your score so far is:", score, "<br><br><br>"))
  
  observeEvent(input$new, {
    
    output$score <- renderText(paste("Your score so far is:", score, "<br><br><br>"))
    
    updateTextInput(inputId = "response", value = "")
    
    mathTest <-reactive({""})
    
    mathType <- reactive({input$type})

    mathOpp <- reactive({
      switch(
        mathType(),
        "Addition" = "+",
        "Subtraction" = "-",
        "Multiplication" = "X",
        "Division" = "&#247")
    })
    
    lowerBound <- reactive({
      switch(
        mathType(),
        "Addition" = 0,
        "Subtraction"=0,
        "Multiplication"= 0,
        "Division"=1)
    })
    
    intOne <- sample(lowerBound():9, 1, replace = FALSE)
    
    upperBound <- reactive({
      switch(
        mathType(),
        "Addition" = 9,
        "Subtraction"=intOne,
        "Multiplication"= 9,
        "Division"=9)
    })
    

    intTwo <- sample(lowerBound():upperBound(), 1, replace = FALSE)
    
    if (mathType() == "Division"){
     intOne <- intOne * intTwo 
    }
    
    mathResult <- reactive({
      switch(
        mathType(),
        "Addition" = intOne + intTwo,
        "Subtraction"=intOne - intTwo,
        "Multiplication"=intOne * intTwo,
        "Division"=intOne / intTwo)
    })
    
    mathEquation <-  reactive({
      paste("<h3> &nbsp", intOne, "<br>", mathOpp(), intTwo, "<br> _________________ </h3>")
    })
  
    output$equation <- renderText(mathEquation())
  
    mathTest <-reactive({ 
      if (input$response == ""){
        ""
        } else if (input$response == mathResult()){
          score <<- score + 1
          output$score <- renderText(paste("Your score so far is:", score, "<br><br><br>"))
          "<h4>That's Correct!<br></h4>"
        } else {
          paste("<h4>Try again<br></h4>")
        }
    })
    
    output$reply <- renderText(mathTest())
    
    output$equation <- renderText(mathEquation())
      

})
}  


# Run the application 
shinyApp(ui = ui, server = server)
