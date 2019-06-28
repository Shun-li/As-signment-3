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



server <- function(input,output) {
  
  
  
  
  ################################################################################################################################## 
  
  output$head <- DT::renderDataTable({
    data <- data[, c(input$x)]
    data1 <- head(data,input$n)
    DT::datatable(data1)
  })
  
  ##################################################################################################################################
  
  output$str <- renderPrint({
    data <- data[, c(input$x)]
    data1 <- head(data,input$n)
    str(data1)
  })
  
  
  ################################################################################################################################## 
  
  
  output$matplot <- renderPlot({
    data <- data[, c(input$x)]
    data1 <- head(data,input$n)
    n = c()
    m <- colnames(data1)
    z <-c("ID","Author","Date","Priority","Price","Speed", "Duration","Scarcity","Location","Agreed","State","Class","Surface")
    for (i in m) {
      if (i %in% z){
        n <- c(n,i) 
        data1 <- data1[ , !(colnames(data1) %in% n ) ]  
      }
    }
    matplot(y = data1, main= "Matplot  (for numeric variables) ",type = "l", xlab = "Observation", ylab="Variables", lty = 1, lwd=1)
    legend(legend=colnames(data1),x="topright",y ="toproght",lty =1,lwd =1,ncol =2)
    
    
  })
  
  
  
  
  
  ##################################################################################################################################
  
  output$mosaicplot <- renderPlot({
    data <- data[, c(input$x)]
    data1 <- head(data,input$n)
    n = c()
    m <- colnames(data1)
    z <-c("Y","ID","Date","Layer1","Layer2","Layer3","Layer4","Layer5","Layer6",
          "Layer7","Layer8","Layer9","Layer10","Layer11","Layer12",
          "Layer13","Layer14","Layer15","Layer16","Layer17","Layer18",
          "Layer19","Layer20","Layer21","Layer22","Layer23","Layer24",
          "Layer25","Layer26","Layer27","Layer28","Layer29","Layer30")
    for (i in m) {
      if (i %in% z){
        n <- c(n,i) 
        data1 <- data1[ , !(colnames(data1) %in% n ) ]  
      }
    }
    form <- sprintf("~%s",paste0(colnames(data1),collapse="+"))
    
    mosaicplot(as.formula(form),main = "Mosaic  (for Categorical variables)",data = data1,shade=TRUE,legend=TRUE,color=TRUE)
    
  })
  
  
  #############################################################################################################################  
  
  output$boxplot <- renderPlot({
    
    outliers <- function(x, mult=1.5, na.rm = TRUE, ...) {
      if (is.numeric(x)) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- mult * (qnt[2]-qnt[1])
        H1 <- qnt[1] - H
        H2 <- qnt[2] + H
        tf <- x < H1 | x > H2
      } else {
        tf <- rep(FALSE, length(x))
      }
    }
    
    outlierRows <- function(data, mult=1.5, na.rm = TRUE, ...) {
      tf <- rep(x=FALSE, times=dim(data)[1])
      for (col in 1:dim(data)[2]) {
        coltf <- outliers(as.vector(data[,col]), mult, na.rm, ...)
        tf <- tf | coltf
      }
      tf[is.na(tf)] <- FALSE
      tf
    }
    
    
    if (input$Methods == "do nothing"){
      boxplot(data[,c(input$x1)], main = "Boxplot",range = 2.1, outline = TRUE,plot = TRUE,horizontal = TRUE) 
    } 
    if ( input$Methods == "remove"){
      data2 <- data[,c(input$x1)]
      bp <- outlierRows(data2, mult=2.1)
      data2[bp,] 
      rownames(data2[bp,])
      ## the outlierrows are the first 12 rows. 
      boxplot(data2[-c(1:12),], main = "Boxplot",range = 2.1, outline = TRUE,plot = TRUE,horizontal = TRUE)
    }
  })
  
  #############################################################################################################################  
  
  output$outlier <- renderPrint({
    
    if (input$Methods == "do nothing"){
      
      bp <- boxplot(data[,c(input$x1)], range = 2.1, outline = TRUE, plot = FALSE)  
      str(bp)
      
    } 
    if ( input$Methods == "remove"){
      print("No any outliers")
    }
  })
  
  #############################################################################################################################  
  output$figure <- renderPrint({
    
    if (input$Methods == "do nothing"){
      bp <- boxplot(data[,c(input$x1)], range = 2.1, outline = TRUE, plot = FALSE)  
      print(bp$out)
    } 
    if ( input$Methods == "remove"){
      print("No any value")
    }
  })
  
  
  #############################################################################################################################  
  output$outlierrow <- renderPrint({
    
    outliers <- function(x, mult=1.5, na.rm = TRUE, ...) {
      if (is.numeric(x)) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- mult * (qnt[2]-qnt[1])
        H1 <- qnt[1] - H
        H2 <- qnt[2] + H
        tf <- x < H1 | x > H2
      } else {
        tf <- rep(FALSE, length(x))
      }
    }
    
    outlierRows <- function(data, mult=1.5, na.rm = TRUE, ...) {
      tf <- rep(x=FALSE, times=dim(data)[1])
      for (col in 1:dim(data)[2]) {
        coltf <- outliers(as.vector(data[,col]), mult, na.rm, ...)
        tf <- tf | coltf
      }
      tf[is.na(tf)] <- FALSE
      tf
    }
    
    
    if (input$Methods == "do nothing"){
      data3 <- data[,c(input$x1)]
      dp3 <- outlierRows(data3, mult=2.1)
      print(data3[dp3,])
    } 
    if ( input$Methods == "remove"){
      print("No any outlierRow")
    }
  })
  
  
  ##################################################################################################################################     
  
  output$miss_value2 <- renderPlot({
    
    if (input$Method2 == "omit"){
      data4 <- data[,c(input$x2)]
      data4 <- na.omit(data4) 
      visdat::vis_miss(data4)
    }
    
    
    if (input$Method2 == "ignore"){
      data4 <- data[,c(input$x2)]
      visdat::vis_miss(data4)
    } 
    
    if (input$Method2 == "impute with mice"){
      data4 <- data[,c(input$x2)]
      imputed_Data <- mice::mice(data4, m=5,maxit=1, method='pmm', print=TRUE)
      data4_new <- mice::complete(imputed_Data,2) 
      visdat::vis_miss(data4_new)
    }
  })    
  
  ##################################################################################################################################  
  
  output$miss_value1 <- renderPlot({
    
    if (input$Method2 == "ignore"){
      data4 <- data[,c(input$x2)]
      VIM::aggr(data4, col=c('navyblue','yellow'),
                numbers=TRUE, sortVars=TRUE,
                labels=names(data4), cex.axis=.7,
                gap=3, ylab=c("Missing data","Pattern"))
      
      
    } 
    if (input$Method2 == "omit"){
      data4 <- data[,c(input$x2)]
      data4 <- na.omit(data4) 
      VIM::aggr(data4, col=c('navyblue','yellow'),
                numbers=TRUE, sortVars=TRUE,
                labels=names(data4), cex.axis=.7,
                gap=3, ylab=c("Missing data","Pattern")) 
    }
    if (input$Method2 == "impute with mice"){
      data4 <- data[,c(input$x2)]
      imputed_Data <- mice::mice(data4, m=5,maxit=3, method='pmm', print=TRUE)
      densityplot(imputed_Data,main="Density Plot")
    }
    
  })
  
  ##################################################################################################################################  
  output$summary2 <- renderPrint ({
    if (input$Method2 == "impute with mice"){
      data4 <- data[,c(input$x2)]
      imputed_Data <- mice::mice(data4, m=5,maxit=3, method='pmm', print=TRUE)
      summary(imputed_Data)
      
    }
  })
  
  ##################################################################################################################################  
  
  output$ratio <- renderPrint({
    
    
    test <- input$m
    train <- 280 - input$m
    ratio = test / train
    print("Test.data =")
    print(test)
    print("Train.data =")
    print(train)
    print("Ratio = Test.data / Train.data = ")
    print(ratio)
    
  }) 
  
  ##################################################################################################################################  
  output$train_data <- DT::renderDataTable({
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      DT::datatable((train))
    }
    
    #if (input$Method2 == "impute with mice"){
    
    #   test <- input$m
    #  train <- 280 - input$m
    # set.seed(101) 
    #train_ratio = train /280
    
    #  imputed_Data <- mice::mice(data, m=1, maxit = 1, method='pmm', print=TRUE)
    #  data <- mice::complete(imputed_Data,1)
    #  sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
    #  train <- data[sample, ]
    #  test  <- data[-sample, ]
    #  DT::datatable((train))
    #  } 
    
  })
  
  ##################################################################################################################################  
  output$test_data <- DT::renderDataTable({
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      DT::datatable((test))
    }
    #if (input$Method2 == "impute with mice"){
    
    #   test <- input$m
    #  train <- 280 - input$m
    # set.seed(101) 
    #train_ratio = train /280
    
    #  imputed_Data <- mice::mice(data, m=1, maxit = 1, method='pmm', print=TRUE)
    #  data <- mice::complete(imputed_Data,1)
    #  sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
    #  train <- data[sample, ]
    #  test  <- data[-sample, ]
    #  DT::datatable((test))
    #  } 
    
    
  })
  
  
  
  ##################################################################################################################################
  output$summary <- renderPrint({
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      
      
      
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      summary(pca,loadings = F)
      
    }
    
    
  })
  
  ##################################################################################################################################
  output$pca <- renderPlot({
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      
      
      
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      
      plot(pca,main ="Barchart")
      
    }
  })
  
  ##################################################################################################################################
  output$pca2 <- renderPlot({
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      
      
      
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      
      screeplot(pca,type="lines",main="ScreePlot")
      
    }
  })
  
  ##################################################################################################################################
  output$t <- DT::renderDataTable({
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      
      
      
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      
      
      
      x1<-predict(pca)
      x <- x1[,c(input$componet)]
      
      DT::datatable((x))
      
    }
  })
  
  
  ##################################################################################################################################
  output$formula <- renderPrint({
    form <- sprintf("%s~%s","Y",paste0(input$w,collapse="+"))
    print(form)
  })
  
  
  
  ##################################################################################################################################
  output$result1 <- renderPrint({
    
    form <- sprintf("%s~%s","Y",paste0(input$w,collapse="+"))
    
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      train <- na.omit(train)
      total<- cbind(train,x1)
      
      result <- lm(as.formula(form),data=total) 
      summary(result)
    }
    
    
    
  })
  
  
  ##################################################################################################################################
  
  output$analysis <- renderPrint({
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(test,is.numeric)
      numData <- test[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      test <- na.omit(test)
      total_1<- cbind(test,x1)
      
      #Coefficients:
      # Estimate Std. Error t value Pr(>|t|)    
      #(Intercept)        21.84033    0.48100  45.406  < 2e-16 ***
      #  PriorityLow        -2.91552    0.35517  -8.209 3.34e-13 ***
      # PriorityMedium     -2.08740    0.34038  -6.132 1.21e-08 ***
      #PriceCostly         0.74240    0.29891   2.484 0.014418 *  
      #PriceExtravagant    5.40129    0.46944  11.506  < 2e-16 ***
      #SpeedMedium         4.65908   0.27173  17.146  < 2e-16 ***
      #SpeedSlow          -2.16114    0.62505  -3.458 0.000761 ***
      #DurationPerpetual   6.07444    0.37622  16.146  < 2e-16 ***
      #DurationShort      -6.80745    0.29431 -23.130  < 2e-16 ***
      #LocationPlant A    -2.80345    0.32591  -8.602 4.14e-14 ***
      #LocationPress room -2.28169    0.33548  -6.801 4.66e-10 ***
      #StateUncertain      1.83423    0.32457   5.651 1.14e-07 ***
      #StateUnder review   1.86400    0.33108   5.630 1.26e-07 ***
      #ClassClass K       -2.37398    0.32650  -7.271 4.37e-11 ***
      #ClassClass Y       -2.17122    0.34300  -6.330 4.69e-09 ***
      #SurfaceSmooth       4.37329    0.34105  12.823  < 2e-16 ***
      #SurfaceTextured     1.27135    0.32203   3.948 0.000135 ***
      #Comp.1              1.33820    0.04880  27.422  < 2e-16 ***
      #Comp.2              0.34651    0.04962   6.982 1.88e-10 ***
      #Comp.3              1.02236    0.05137  19.902  < 2e-16 ***
      #Comp.4              0.44982    0.05323   8.450 9.28e-14 ***
      
      total_1$Y_predicted <- 21.84033 - 1.33820 * total_1$Comp.1 + 0.34651 * total_1$Comp.2 + 1.02236 * total_1$Comp.3  + 0.44982 * total_1$Comp.4 
      #  Y~Priority+Price+Speed+Duration+Location+State+Class+Surface+Comp.1+Comp.2+Comp.3+Comp.4 
      
      MSE <-  (sum((total_1$Y -  total_1$Y_predicted)**2))/ (nrow(total_1))
      print(MSE)
    }
  })
  
  
  ################################################################################################################################## 
  output$difference <- renderPlot({
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(test,is.numeric)
      numData <- test[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      test <- na.omit(test)
      total_1<- cbind(test,x1)
      total_1$Y_predicted <- 21.84033 - 1.33820 * total_1$Comp.1 + 0.34651 * total_1$Comp.2 + 1.02236 * total_1$Comp.3  + 0.44982 * total_1$Comp.4 
      #  Y~Priority+Price+Speed+Duration+Location+State+Class+Surface+Comp.1+Comp.2+Comp.3+Comp.4 
      
      m <- c(1:nrow(total_1))
      xxx3 <- data.frame( m, total_1$Y, total_1$Y_predicted )
      xxx3$Y <- xxx3[,2]
      xxx3$y <- xxx3[,3]
      
      
      ggplot(xxx3,aes(x=m, y=Y))+geom_point(colour="blue",size =3)+ 
        geom_point(data=xxx3, aes(x=m, y=y), colour="red", shape = 2, size=3)+
        xlab("observation") + ylab("Y value") + ggtitle("Scatter Plot")
      
    }
    
    
    
    
    
    
  })
  
  
  ################################################################################################################################## 
  output$j1 <- renderPrint({
    form <- sprintf("%s~%s","Y",paste0(input$w,collapse="+"))
    
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      train <- na.omit(train)
      total<- cbind(train,x1)
      
      print(nrow(total))
    } 
  })
  
  ################################################################################################################################## 
  output$j2 <- renderPrint({
    form <- sprintf("%s~%s","Y",paste0(input$w,collapse="+"))
    
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(test,is.numeric)
      numData <- test[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      test <- na.omit(test)
      total_1<- cbind(test,x1)
      
      print(nrow(total_1))
    } 
  })
  ################################################################################################################################## 
  output$j3 <- renderPrint({
    form <- sprintf("%s~%s","Y",paste0(input$w,collapse="+"))
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(train,is.numeric)
      numData <- train[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      train <- na.omit(train)
      total<- cbind(train,x1)
      a <- nrow(total)
    }
    
    form <- sprintf("%s~%s","Y",paste0(input$w,collapse="+"))
    
    
    if (input$Method2 == "ignore"){
      test <- input$m
      train <- 280 - input$m
      set.seed(101) 
      train_ratio = train /280
      sample <- sample.int(n = nrow(data), size = floor( train_ratio *nrow(data)), replace = F)
      train <- data[sample, ]
      test  <- data[-sample, ]
      numeric  <- sapply(test,is.numeric)
      numData <- test[,numeric]
      numData <- numData[,-1]
      pca <- princomp(formula=~.,data = numData,cor = T)
      x1<-predict(pca)
      
      test <- na.omit(test)
      total_1<- cbind(test,x1)
      b <- nrow(total_1)
    }
    
    c<- 280 -a -b
    print(c)
  })
  
  
  
  
  ################################################################################################################################## 
  
  
}






################################################################################################################################## 

shinyApp(ui = ui, server = server)

