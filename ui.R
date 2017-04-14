library(shiny)
library(arules)
library(arulesViz)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("sup","select the support",
                  choices = c(.1,.01,.05,.001,.005)),
      selectInput("conf","select the confidence",
                  choices = c(.5,.6,.7,.8,.9))
    ),
    mainPanel(
      verbatimTextOutput("mba")
    )
  )
))