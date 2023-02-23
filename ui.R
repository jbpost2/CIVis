###########################################################################
##R Shiny App to plot confidence intervals and find their coverage rate
###########################################################################

#Load package
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)


dashboardPage(skin="red",
              #add title
              dashboardHeader(title = "Confidence Interval Visualization", titleWidth = 750),
              
              #define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("archive")),
                menuItem("Application", tabName = "app", icon = icon("laptop"))
              )),
              
              #define the body of the app
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(6,
                                   #Description of App
                                   h1("What does this app do?"),
                                   #box to contain description
                                   box(background="red",width=12,
                                       h4("This app is meant to help visualize the idea of confidence interval coverage rates.  Specifically, this app simulates data from a normal distribution of the user's preference and fits the standard 'one-sample Z interval' where the population standard deviation is assumed known.  The user can also specify the sample size and confidence level.  "),
                                       h4("A visual of the confidence intervals and their coverage of the true mean is created.  More and more samples can be created to see the behavior of the intervals over repeated samples (up to 10,000 samples).  Information about the most recent sample is provided on the bottom left.  "),
                                       h4("The sampling distribution of sample means is also created and visualized as samples are created via a histogram on the bottom right.  Red lines represent boundaries for regions where a generated sample mean provides an interval that capture the true mean and does not capture it.")
                                   )
                            ),
                            
                            column(6,
                                   #How to use the app
                                   h1("How to use the app?"),
                                   #box to contain description
                                   box(background="red",width=12,
                                       h4("The controls for the app are located on the top left and top right.  The top left controls allow for changing the mean and standard deviation of the data generation process as well as the sample size and confidence level.  The controls on the top right allow for creation of more data sets and to reset the number of data sets.  "),
                                       h4("As you generate each sample, the information corresponding to the most recent sample is provided on the bottom left and a confidence interval is added to the main plot in the top middle of the application.  A sample mean is also added to the sampling distribution histogram on the bottom right.")
                                   )
                            )
                          )
                  ),
                  
                  #actual app layout      
                  tabItem(tabName = "app",  
                          
                          fluidRow(
                            column(2,
                              h2("Normal Distribution Parameters"),
                              numericInput("Param1", h4("Mu = "), value = 0, step = 1),
                              numericInput("Param2", h4("Sigma = "), value = 1, min = 0.01, step = 0.1),
                              sliderInput("confLevel", h4("Confidence Level:"), value = 0.95, min = 0.5, max = 0.9995, step = 0.01, animate = TRUE),
                              sliderInput("sampleSize", h4("Sample size:"), value = 10, min = 2, max = 50, step = 1, animate = TRUE)#,
                      #selectInput("intervalType","One Sample Interval Method:",choices=c("Normal","T"))
                            ), #end column

                            #Plots and summary stats
                             column(10,
                                    fluidRow(
                                      h1("Visual of Confidence Interval Coverage")
                                      ),
                                    fluidRow(
                                      column(8,
                                        fluidRow(
                                          box(width = 12, background = "red",
                                             h3("Red indicates the interval did not cover the true value"),
                                              plotOutput("CIPlots") %>% withSpinner(color="#0dc5c1")
                                          )
                                        )
                                      ),
                                      column(4,
                                           h2("Generate some data sets!"),
                                           actionButton("newData", h4("New Data Set")),
                                         div(br(), style = "font-size:10%"),
                                           actionButton("newData10", h4("10 New Data Sets")),
                                         div(br(), style = "font-size:10%"),
                                           actionButton("newData100", h4("100 New Data Sets")),         div(br(), style = "font-size:10%"),
                                           actionButton("newData1000", h4("1000 New Data Sets")),         div(br(), style = "font-size:10%"),
                                           actionButton("reset", h4("Reset the # of datasets to 1")),
                                         div(br(), style = "font-size:40%"),
                                           box(width = 12, background = "red",
                                              div(tableOutput("coverage"), style= "font-size: 125%")
                                         )
                                        )
                                    )
                             ) #end column
                          ),
                          fluidRow(
                            column(6, 
                              box(width = 12, background = "red",
                                  title = h3("Current Sample's Information"),
                                  column(8,
                                         plotOutput("dataHist")
                                  ),
                                  column(4,
                                         div(tableOutput("summaryStats"), style = "font-size:125%"),
                                         div(uiOutput("currentCI"), style = "font-size:125%")
                                  )
                              )
                            ),
                            column(6, 
                                   box(width = 12, background = "red",
                                       title = h3("Distribution of Sample Means"),
                                       plotOutput("sampDist"),
                                       h4("Red lines indicate where larger or smaller values yields an interval that doesn't capture the mean.")
                                   )
                            )
                          )
                  )
                )
              )
)

