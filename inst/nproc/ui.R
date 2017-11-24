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

ui <- pageWithSidebar(
  headerPanel('Neyman-Pearson Classification and Receiver Operating Charateristics'),
  sidebarPanel(
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
    selectInput('method', 'Classifier', classifiers),
    #selectInput('x', 'X', x),
    #selectInput('y', 'Y', y),
    sliderInput("delta",
                "delta:",
                min = 0.01,
                max = 0.5,
                value = 0.05),
    sliderInput("split",
                "split:",
                min = 1,
                max = 21,
                value = 1,
                step = 2),
   # actionButton("readdata", "Load Data"),
    #actionButton("nproc", "NP-ROC band"),
    tags$hr(),
    fileInput("xtestfile", "Choose CSV File for test Input X",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(),
    checkboxInput("headerxtest", "Header", TRUE),
    sliderInput("alpha",
                "alpha:",
                min = 0.01,
                max = 0.5,
                value = 0.05),
   # actionButton("npc", "NP classification"),
    # Button
    downloadButton("downloadData", "Download Prediction")

  ),
  mainPanel(
    plotOutput('plot1')
  )
)
