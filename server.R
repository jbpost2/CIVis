library(shiny)
require(plotrix)

D <- 10000

mycolor <- function(endpoints, par) {
  if (par < endpoints[1]) 
    "Red"  # if the mean is below the left endpoint of the confidence interval
  else if (par > endpoints[2]) 
    "Red"  # if the mean is above the right endpoint of the confidence interval
  else "Black"  # if the mean lies between the endpoints
}

# Define server logic required to draw the plots
shinyServer(function(input, output, session) {

  
  #number of data set we are one
  v <- reactiveValues(valueButton = 1)
  
  observeEvent(input$newData, {
    v$valueButton <- v$valueButton + 1
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$newData10, {
    v$valueButton <- v$valueButton + 10
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$newData100, {
    v$valueButton <- v$valueButton + 100
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$newData1000, {
    v$valueButton <- v$valueButton + 1000
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$reset, {
    v$valueButton <- 1
  })
  
  simData<-reactive({
    set.seed(110)

    #sample size
    n <- input$sampleSize
    mean <- input$Param1
    sd <- input$Param2

    samples <- matrix(rnorm(D*n, mean = mean, sd = sd), nrow = D, ncol = n)
    samples
    
  })
  
  Intervals <- reactive({
    samples <- simData()
    samples <- samples[1:v$valueButton, , drop = FALSE]
    sd <- input$Param2
    n <- input$sampleSize
    cl <- input$confLevel
    #create CIs
    means <- apply(FUN = mean, X = samples, MARGIN = 1)
    #sds <- apply(FUN = sd, X = samples, MARGIN = 1)
    multiplier <- qnorm((1 + cl)/2)
    
    CIs <- cbind(means - multiplier*sd/sqrt(n), means + multiplier*sd/sqrt(n))  
    CIs
  })
  


  output$summaryStats <- renderTable({
    #Get data
    samples <- simData()    
    #just the last row of the data set
    samples <- samples[v$valueButton, ]
    #summary stats
    sumstat <- data.frame(
      Name = c("Min", "Q1", "Median", "Q3", "Max", "Mean", "SD"),
      Value = as.character(c(round(quantile(samples, c(0, 0.25, 0.5, 0.75, 1)), 3), round(mean(samples), 3), round(sd(samples), 3))),
      stringsAsFactors = FALSE)
  })

    
  output$coverage<-renderTable({

    mu <- input$Param1

    CIs <- Intervals()

    numCover <- sum(ifelse(CIs[, 1] > mu | CIs[, 2] < mu, 0, 1))
    numInterval <- v$valueButton

    cov <- data.frame( Attribute = c("# Intervals", "# Covering Mean", "Coverage %"), Statistic = c(numInterval, numCover, round(numCover/numInterval, 2))
      )
  })
  
    output$dataHist<-renderPlot({
      #Get data
      samples <- simData()
      samples <- samples[v$valueButton, ]
      #plot
      hist(samples, main = "Histogram of Most Recent Data Set Created")
    })  

  output$CIPlots<-renderPlot({
    #Get data
    samples <- simData()    
    CIs <- Intervals()
    mu <- input$Param1
    sd <- input$Param2
    n <- input$sampleSize
    cl <- input$confLevel
    
    col <- apply(FUN = mycolor, X = CIs, MARGIN = 1, par = mu)

    plotCI(x = 1:v$valueButton, 
           y = (CIs[, 1] + CIs[, 2])/2,
           li = CIs[, 1], 
           ui = CIs[, 2],
           col = col, 
           lwd = 1.5,
           ylim = c(min(CIs[, 1]), max(CIs[, 2])),
           ylab = "Intervals",
           xlab = "Sampled Data Set",
           main = "Visualization of CIs")
     #draw a line for true mean
             abline(h = mu, lwd = 2)
           
           
   # if(N>50){
   #   range<-c(N-49,N)
   #   plotCI(x = range[1]:range[2],y=means[range[1]:range[2]],
   #          li = CIs[range[1]:range[2],1], ui = CIs[range[1]:range[2],2],
   #          col = col[range], lwd = 1,
   #          ylab="Intervals",xlab="Sampled Data Set",main="Visualization of CIs")
   #     #draw a line for true mean
   #     abline(h = mu, lwd = 2) 
   #  } else {
   #   range<-c(1,N)
   #   plotCI(x = 1:50,y=c(means[range[1]:range[2]],rep(mu,50-N)),
   #          li = CIs[range[1]:range[2],1], ui = CIs[range[1]:range[2],2],
   #          col = col[range], lwd = 1,ylim=c(min(CIs[,1]),max(CIs[,2])),
   #          ylab="Intervals",xlab="Sampled Data Set",main="Visualization of CIs")
   #   #draw a line for true mean
   #   abline(h = mu, lwd = 2) 
   # }

      })

  output$currentCI <- renderUI({
    paste0("Calculated Interval: (", round(Intervals()[v$valueButton,1], 2), ", ", round(Intervals()[v$valueButton, 2], 2), ")")
  })
  
  output$sampDist <- renderPlot({

    mu <- input$Param1
    sd <- input$Param2
    n <- input$sampleSize
    
    multiplier <- qnorm((1 + input$confLevel)/2)
    
    CIs <- Intervals()
    
    means <- (CIs[, 1] + CIs[, 2])/2
    lcut <- mu-multiplier*sd/sqrt(n)
    ucut <- mu+multiplier*sd/sqrt(n)
    
    if(v$valueButton < 20){
      hist(means, breaks = seq(-max(abs(c(min(means), max(means)))) - sd/sqrt(n), max(abs(c(min(means), max(means))))+sd/sqrt(n), length = 20), xlab = "Sample means")
      abline(v = c(lcut, ucut), lwd = 2, col = "Red")
    } else {
      hist(means, breaks = 20, xlab = "Sample means")
      abline(v = c(lcut, ucut), lwd = 2, col = "Red")
    }

  
  })
  
})