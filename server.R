library(shiny)
library(gdata)
library(rpart)
library(rpart.plot)
library(R.matlab)
library(R.oo)
library(R.methodsS3)
library(R.utils)
library(session)

HIVdata <- read.xls("HIVDataOriginal.xlsx")
hivclasses <- read.xls("HIVClasses.xlsx")
fit2 <- rpart(Classes~ Maize + Potato + Cassava + WealthGinIndex, method = 'class', data = hivclasses,control =rpart.control(minsplit =1,minbucket=1, cp=0))
wt <- readMat("marginal_utils.mat")
wt1 <- wt$marginal.utils
wt2 <- wt1[4]
numargpie <- sapply(wt2, function(x) as.numeric(as.character(x)))
mumaize <- numargpie[1]
mupotato <- numargpie[2]
mucassava <- numargpie[3]
muwgi <- numargpie[4]

threshdata <- readMat("util_thresholds.mat")
thresh <- threshdata$util.thresholds
threshlevel <- thresh[1]

accuraciesdata <- readMat("accuracies.mat")


utilityFunction <- function(x) {
  
  tags$script("Shiny.addCustomMessageHandler('messageBox', function(msg){window.alert(msg);})")
  
}

shinyServer(function(input,output,session){
  
  data <-   reactive({
    switch(input$dataset,
           mz=HIVdata$Maize,
           pt=HIVdata$Potato,
           cs=HIVdata$Cassava,
           wgi=HIVdata$WealthGinIndex
           
    )
    
  })
  
  # Generate a table of the dataset
  output$table <- renderTable({
    
    data.frame(x=data())
    
  })
  
  # Generate a table of the dataset
  output$alltable <- renderTable({
    
    HIVdata
    
  }) 
  
  
  
  output$plot <- renderPlot({
    
    barplot(data(),main ="BARPLOT OF HIV INDICATORS",xlab = "NUMBER OF DISTRICTS")
    
  })
  #Visualise UTADIS marginal utilities
  output$utadisvis <- renderPlot({
    
    
    
    lbls = c("Maize","Potato","Cassava","WealthGinIndex")
    n=4
    
    pie(numargpie, col=rainbow(n),labels = lbls, radius=1.1, main  ="PIE CHART SHOWING THE RISK LEVEL OF CRITERIA ")
    
    
    
  })
  
  #Predict UTADIS classes
  
  output$utadispred <- renderUI({
    
    wellPanel(
      
      p(strong("Please Enter District Data")),
      
      tagList(
        #First remove arrows from numeric field
        tags$style(HTML("
                        input[type=number] {
                        -moz-appearance:textfield;
                        }
                        input[type=number]::{
                        -moz-appearance:textfield;
                        }
                        input[type=number]::-webkit-outer-spin-button,
                        input[type=number]::-webkit-inner-spin-button {
                        -webkit-appearance: none;
                        margin: 0;
                        }
                        ")),
        numericInput("maize", "Maize District Data:", 0),
        numericInput("potato", "Potato District Data:", 0),
        numericInput("cassava", "Cassava District Data:", 0),
        numericInput("wealthginindex", "WealthGinIndex District Data:", 0),
        actionButton('action', 'Predict')
        
        )
      
      
      
        )
    
})
  
  output$predictbutton <- function() {
    
    if (is.null(input$action) || input$action == 0)
      return(NULL)
    
    return(isolate({
      cgs <- ((input$maize) * mumaize) + ((input$potato) * mupotato) + ((input$cassava) * mucassava) + ((input$wealthginindex) * muwgi)
      
      if (cgs > threshlevel){
        session$sendCustomMessage("messageBox", paste(" With the District's global score of", cgs, "and threshold of ", threshlevel, "the district is in CRISIS"))
      }
      
      
      
      
      
      if (cgs < threshlevel){
        session$sendCustomMessage("messageBox", paste(" With District's global score of", cgs, "and threshold of ", threshlevel, "the district is in ALERT"))
      }
      
      
    }))
  }
  
  
  
  
  #Display results of the classification decision tree
  
  output$results <- renderPrint({
    
    printcp(fit2)
  })
  
  #Display classification summary
  output$classummary <- renderPrint({
    
    summary(fit2)
  })  
  
  #Display UTADIS weight summary
  output$utadisweight <- renderPrint({
    
    str(wt)
    str(threshdata)
    str(accuraciesdata)
    
    
    #summary(fit2)
  }) 
  
  #Plot the decision tree
  
  output$plotree <- renderPlot({
    
    prp(fit2, extra = 1, faclen=0, nn = T,
        box.col=c("green", "orange", "yellow")[findInterval(fit2$frame$yval, v = c(1,2,3))])
    
    
    
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function(){paste(input$dataset,".csv",sep=" ")},
    content = function(file){
      write.csv(data(),file)
      
    }
    
  )
  
} )
