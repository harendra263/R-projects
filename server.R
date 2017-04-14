library(shiny)
library(arules)

shinyServer(function(input,output){
  
  output$mba <- renderPrint({
    rules <- apriori(Groceries,parameter = list(support=as.numeric(input$sup),
                                                confidence=as.numeric(input$conf)))
  })
})