library(shiny)
library(gdata)
library(rpart)
library(rpart.plot)
library(R.matlab)
library(R.oo)
library(R.methodsS3)
library(R.utils)
library(session)
library(htmltools)






shinyUI(pageWithSidebar(
  
  
  
  
  tags$a(
    href="https://www.gov.uk/government/organisations/department-for-international-development", 
    tags$img(src="dfid-ukaid.png",
             title="To DFID Website", 
             width="100",
             height="140",
             style="float:left;margin-top:-15px"
             
             
    ),
    
tags$a(href="https://en.wikipedia.org/wiki/Districts_of_Ivory_Coast", tags$h1("REPUBLIC OF COTE D'IVOIRE DISTRICTS HIV PROGRAM",style="white-space:nowrap;color:orange;overflow:hidden;text-overflow:ellipsis;margin-left:120px;position:fixed")),

     
    tags$a(
      href="http://www.gouv.ci/Main.php", 
      tags$img(src="FlagCI.png",
               title="To Ivory Coast Country Website", 
               width="100",
               height="90",
               style="float:right"
               
              
               )
      
      
      
    )
    
    ),
  
  
  
  
  
  
  
  sidebarPanel(
    selectInput("dataset","Choose Criterion:",
                list("MAIZE" ="mz",
                     "POTATO"="pt", 
                     "CASSAVA" = "cs",
                     "WEALTHGINI"="wgi"
                )),
    
    downloadButton("downloadData","Download")
    
    
    
    
    
    
    
    
    
  ),
  
  
  
  
  mainPanel(
    
    
    tags$script("Shiny.addCustomMessageHandler('messageBox', function(msg){window.alert(msg);})"),  
    tabsetPanel(id="tabSelected",
                tabPanel("BarCharts",plotOutput("plot")),
                tabPanel("CriterionData",tableOutput("table")),
                tabPanel("View all DataSet",tableOutput("alltable")),
                tabPanel("View Decision Tree",plotOutput("plotree")),
                tabPanel("View UTADIS output",verbatimTextOutput("utadisweight")),
                tabPanel("Criteria Risk Visualisation",plotOutput("utadisvis")),
                tabPanel("Predict Risk Class",uiOutput("utadispred"), uiOutput("predictbutton"))
                
    )
    
  )
))
