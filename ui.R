#Getting the required files for descriptives and charts
read.csv("./EE.csv",stringsAsFactors = F,header = T)
read.csv("./OO.csv",stringsAsFactors = F,header = T)
read.csv("./Te.csv",stringsAsFactors = F,header = T)
read.csv("./Th.csv",stringsAsFactors = F,header = T)
read.csv("./Vi.csv",stringsAsFactors = F,header = T)
read.csv("./Vo.csv",stringsAsFactors = F,header = T)

#File import for word cloud
read.csv("./nee.csv")
read.csv("./noo.csv")
read.csv("./nte.csv")
read.csv("./nth.csv")
read.csv("./nvi.csv")
read.csv("./nvo.csv")

#Getting the required packages

# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ECharts2Shiny")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("plotly")

library(shiny)
library(shinydashboard)
library(plotly)
# library(ECharts2Shiny)
 library(wordcloud)
 library(RColorBrewer)

title1 <- tags$a(href='https://www.google.com',
               tags$img(src="radio-tower.png", height = '50', width = '50'),
               'Telecom Dashboard', target="_blank")




shinyUI(
  dashboardPage(title=title1, skin = "purple",
    dashboardHeader(title = title1),
    dashboardSidebar(
      sliderInput("bins","Number of breaks",1,20,5),
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Th","Vo","Te","Vi","Eo","Oo")),
      selectInput(inputId="emotion",
                  label = "select an emotion",
                  choices = c("Positive","Negative")),
      selectInput("inField1","Select a field to create a Histogram", choices = names(Th[,3:12])),
      selectInput("inField2","Choose a month",choices=names(Th[,18:36]))),
    dashboardBody(
      fluidPage(
      dateRangeInput("dates", 
                     "Date range",
                     start = "2015-01-01", 
                     end = as.character(Sys.Date())),
      textOutput("DateRange")
    ),
      
                fluidRow(
                  infoBoxOutput("No.ofreviews", width = 3),
                  infoBoxOutput("Positive", width = 3),
                  infoBoxOutput("Neutral", width = 3),
                  infoBoxOutput("Negative", width = 3),
                  box(plotOutput("histogram1")),
                  box(plotOutput("histogram2")),
                  box(plotOutput("line1")),
                  box(plotOutput("bar1")),
                  box(plotOutput("bar2")),
                  box(plotOutput("line2")),
                  box(plotOutput("pie1")),
                  box(plotOutput("word1"))
                  
      )
      
      
    )
  )
)

