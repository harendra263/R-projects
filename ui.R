library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "This is the Header"),
    dashboardSidebar(
      sliderInput("bins","Number of breaks",1,100,50),
      menuItem("Dashboard"),
      menuSubItem("Dashboard Finance"),
      menuSubItem("Dashboard Sales"),
      menuItem("dDetailed Analysis"),
      menuItem("Raw data")
    ),
    dashboardBody(
      fluidRow(
        box(plotOutput("histogram"))
        
      )
    )
  )
)