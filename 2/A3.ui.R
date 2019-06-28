library(shiny)
library(shinyjs)
library(DT)
library(graphics)
library(vcd)
library(Amelia)
library(missForest)
library(VIM)
library(lattice)
library(mice)
library(visdat)
library(ggplot2)

data <- read.csv("/Users/shunli/Desktop/ASS3DATA.CSV")
data <-data.frame(data)
#rownames
row.names(data) <- data$ID

#date-strings --> dates
data$Date <- as.Date(data$Date)

numeric <- sapply(data,is.numeric)
numData <- data[,numeric]



ui <- fluidPage(
  titlePanel("Assignment 3"),
  navbarPage("SHUN LI , 65322005"),
  tabsetPanel(
    tabPanel("Data Structure",sidebarLayout(
      sidebarPanel(
        sliderInput("n", label = h3("Number of Observations:"), min = 1, max = 280, step = 1, value = 280),
        checkboxGroupInput("x",label = h3("Select variables"),
                           choices = list("Y", "Author","Date", "Priority", 
                                          "Price", "Speed", "Duration", "Scarcity",
                                          "Location", "Agreed", "State", "Class","Surface",
                                          "Layer1","Layer2","Layer3","Layer4","Layer5","Layer6",
                                          "Layer7","Layer8","Layer9","Layer10","Layer11","Layer12",
                                          "Layer13","Layer14","Layer15","Layer16","Layer17","Layer18",
                                          "Layer19","Layer20","Layer21","Layer22","Layer23","Layer24",
                                          "Layer25","Layer26","Layer27","Layer28","Layer29","Layer30"),
                           selected = c("Y","Speed","Duration","Layer1","Layer2","Layer3"))
        
        
      ),
      mainPanel(DT::dataTableOutput("head"),verbatimTextOutput("str"),plotOutput("matplot"),plotOutput("mosaicplot")))),
    tabPanel("Outliers",sidebarLayout(
      sidebarPanel(
        radioButtons("Methods", label = h3("Methods"),
                     choices = list("do nothing", "remove"),
                     selected = "do nothing"),
        checkboxGroupInput("x1",label = h3("Select variables"),
                           choices = list("Y","Layer1","Layer2","Layer3","Layer4","Layer5","Layer6",
                                          "Layer7","Layer8","Layer9","Layer10","Layer11","Layer12",
                                          "Layer13","Layer14","Layer15","Layer16","Layer17","Layer18",
                                          "Layer19","Layer20","Layer21","Layer22","Layer23","Layer24",
                                          "Layer25","Layer26","Layer27","Layer28","Layer29","Layer30"),
                           selected = c("Y","Layer1","Layer2","Layer3"))),
      mainPanel(plotOutput("boxplot"),verbatimTextOutput("outlier"),p("The value of outliers :"),verbatimTextOutput("figure"),
                p("The outlierRows:"),verbatimTextOutput("outlierrow")))),
    
    tabPanel("Missing Value",sidebarLayout(position = "right",
                                           sidebarPanel(
                                             radioButtons("Method2", label = h3("Methods"),
                                                          choices = list("omit", "ignore","impute with mice"),
                                                          selected = "ignore"),
                                             h5("Main approach : ignore",style = "color:red"),
                                             p("Other alternatives also worth exploring"),
                                             p("But they are not be choosen for the following steps."),
                                             checkboxGroupInput("x2",label = h3("Select variables"),
                                                                choices = list("Y", "Author","Date", "Priority", 
                                                                               "Price", "Speed", "Duration", "Scarcity",
                                                                               "Location", "Agreed", "State", "Class","Surface",
                                                                               "Layer1","Layer2","Layer3","Layer4","Layer5","Layer6",
                                                                               "Layer7","Layer8","Layer9","Layer10","Layer11","Layer12",
                                                                               "Layer13","Layer14","Layer15","Layer16","Layer17","Layer18",
                                                                               "Layer19","Layer20","Layer21","Layer22","Layer23","Layer24",
                                                                               "Layer25","Layer26","Layer27","Layer28","Layer29","Layer30"),
                                                                selected = c("Y","Author","Layer1","Layer2","Layer3"))),
                                           mainPanel(plotOutput("miss_value2"),verbatimTextOutput("summary2"),plotOutput("miss_value1")))),
    tabPanel("Data Split",sidebarLayout(
      sidebarPanel(sliderInput("m", h3("Train and Test"), min = 1, max = 280, step = 1, value = 135)),
      mainPanel(h3("Test-Train Ratio"),
                verbatimTextOutput("ratio"),h3("Traing data"),DT::dataTableOutput("train_data"),
                h3("Testing data"), DT::dataTableOutput("test_data")))),
    tabPanel("Dim Reduction",sidebarLayout(
      sidebarPanel(
        h5("Since the cumulative proportion of the  first four components is over 95%, so just need to choose  from them",color = "red"),
        checkboxGroupInput("componet", label = h3("Choose Components"),
                           choices = list("component 1"="Comp.1", "component 2"="Comp.2","component 3"="Comp.3","component 4"="Comp.4"),
                           selected = c("Comp.1","Comp.2","Comp.3","Comp.4"))),
      mainPanel(h4("Danger-Zone ",style = "color:red"),
                h5("With dimensional reduction, the potential obs/parameters ratio need to be greater than 1, 
                   it means the number ofobserations need to be greater than 30."),
                h3("Summary"),
                verbatimTextOutput("summary"),plotOutput("pca2"),plotOutput("pca"),h3("Components:"),
                DT::dataTableOutput("t")))),
    tabPanel("Train",sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("w",label = h3("Select variables"),
                           choices = list("Author","Date", "Priority", 
                                          "Price", "Speed", "Duration", "Scarcity",
                                          "Location", "Agreed", "State", "Class","Surface",
                                          "Comp.1","Comp.2","Comp.3","Comp.4"),
                           selected = "Comp.1")),
      mainPanel(h3("Formula"),verbatimTextOutput("formula"),h3("Result"),verbatimTextOutput("result1"),
                h3("Optimised formula :"),h5("Y~Priority+Price+Speed+Duration+Location+State+Class+Surface+Comp.1+Comp.2+Comp.3+Comp.4")))),
    tabPanel("Test",sidebarLayout(
      sidebarPanel(h4("Optimised formula:"),h5("Y~Priority+Price+Speed+Duration"),
                   h5("+Location+State+Class+Surface"),h5 ("+Comp.1+Comp.2+Comp.3+Comp.4")),
      mainPanel(h4("MSE"),verbatimTextOutput("analysis"),plotOutput("difference"),p("blue point :Actural Y"),p("red triangle: Predicted Y"),
                h4("The number of trainging observations : "),
                verbatimTextOutput("j1"),h4("The number of test observation"),verbatimTextOutput("j2"),
                h4("The number of removed observations"),verbatimTextOutput("j3"),
                h4("The number of parameters used durning modelling : "),p(" [ 1 ]       13")
      )))
    
    )
)  
