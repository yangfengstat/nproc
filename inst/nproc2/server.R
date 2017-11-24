#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nproc)
classifiers <- c("logistic", "penlog", "svm", "randomforest",
                 "lda", "nb", "ada", "tree")

# Define server logic required to draw a histogram
server <- function(input, output, session) {



   output$downloadData <- downloadHandler(
     filename = 'ytest.hat.csv',
     content = function(file) {
       inFilex <- input$xfile

       if (is.null(inFilex))
         return(NULL)

       x = as.matrix(read.csv(inFilex$datapath, header = input$headerx))
       inFiley <- input$yfile
       if (is.null(inFiley))
         return(NULL)
       y = unlist(read.csv(inFiley$datapath, header = input$headery))
       fit = nproc(x, y, method = input$method, delta = input$delta, split = input$split)

       inFilextestfile <- input$xtestfile

       if (is.null(inFilextestfile))
         return(NULL)

       xtest = as.matrix(read.csv(inFilextestfile$datapath, header = input$headerxtest))

       fit = npc(x, y, method = input$method, alpha = input$alpha, delta = input$delta, split = input$split)
       predy = predict(fit, newx = xtest)$pred.label
       write.table(predy, file, sep = ',', row.names = FALSE, col.names = FALSE)
     }
   )
   output$plot1 = renderPlot({
     inFilex <- input$xfile

     if (is.null(inFilex))
       return(NULL)

     x = as.matrix(read.csv(inFilex$datapath, header = input$headerx))
     inFiley <- input$yfile
     if (is.null(inFiley))
       return(NULL)
     y = unlist(read.csv(inFiley$datapath, header = input$headery))
       fit = nproc(x, y, method = input$method, delta = input$delta, split = input$split)
       plot(fit)


   })
    ##Plot the nproc curve



}


