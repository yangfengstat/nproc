#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
classifiers <- c("logistic", "penlog", "svm", "randomforest",
                 "lda", "nb", "ada", "tree")
shinyUI(fluidPage(

  title = "Diamonds Explorer",

  plotOutput('plot'),

  hr(),

  fluidRow(
    column(3,
           h4("Diamonds Explorer"),
           fileInput("xfile", "Choose CSV File for training Input X",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),
           checkboxInput("headerx", "Header", TRUE),
           tags$hr(),
           fileInput("yfile", "Choose CSV File for training Output Y",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),
           checkboxInput("headery", "Header", TRUE),
           tags$hr(),
    ),
    column(4, offset = 1,
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
           selectInput('color', 'Color', c('None', names(dataset)))
    ),
    column(4,
           selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
           selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    )
  )
))

