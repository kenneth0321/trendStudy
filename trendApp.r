library(shiny)
library("janitor")
library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")

setwd("C:/Users/QinH/OneDrive - Willis Towers Watson/Desktop/PL Trend R")

agg_table <- read_excel("PL_trend_sample_data.xlsx") %>%
  clean_names()

agg_table$claim_freq <- agg_table$ult_claim_count/agg_table$on_level_premium*1000 
exponetial.modelF<-lm( log(claim_freq) ~ acc_year, data = agg_table)

period_all =  data.frame(acc_year = c(agg_table$acc_year, 2020,2021))

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PL Trend - D&O (US+Intl) (AY PY Combined)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: period 1 ----
      sliderInput(inputId = "option_1", 
                  label = "Period 1:",
                  min = 2007, 
                  max = 2021,
                  value = c(2007,2018),
                  sep=""),
      
      # Input: period 2 ----
      sliderInput(inputId = "option_2", 
                  label = "Period 2:",
                  min = 2007, 
                  max = 2021,
                  value = c(2010,2015),
                  sep=""),
      
      # Input: period 3 ----
      sliderInput(inputId = "option_3", 
                  label = "Period 3:",
                  min = 2007, 
                  max = 2021,
                  value = c(2012,2019),
                  sep="")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput("freq_trend")
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
 fitted_models <- reactive({
   input$option_1
   input$option_2
   input$option_3
   
  
   
   fitted.allYear <- exp(predict(exponetial.modelF, period_all, type = "response"))
   exp.model.df <- data.frame(x = period_all,
                              all_year = fitted.allYear)
   
   
   #no need to plot so many different curves, save each simulated freq as an additional column to the agg_table, then plot by columns
   #user input period 1 curve fitting
   
   period_1 <- data.frame(acc_year = c(seq(min(input$option_1),max(input$option_1),1)))
   agg_table_1 <- agg_table %>%
     filter(acc_year %in% period_1$acc_year)
   
   exponetial.modelF1<-lm( log(claim_freq) ~ acc_year, data = agg_table_1)
   fitted.p1 <- exp(predict(exponetial.modelF1, newdata = period_all, type = "response"))
   
   
   #user input period 2 curve fitting
   period_2 <- data.frame(acc_year = c(seq(min(input$option_2),max(input$option_2),1)))
   agg_table_2 <- agg_table %>%
     filter(acc_year %in% period_2$acc_year)
   
   exponetial.modelF2<-lm( log(claim_freq) ~ acc_year, data = agg_table_2)
   fitted.p2 <- exp(predict(exponetial.modelF2, newdata = period_all, type = "response"))
   
   
   #user input period 3 curve fitting
   period_3 <- data.frame(acc_year = c(seq(min(input$option_3),max(input$option_3),1)))
   agg_table_3 <- agg_table %>%
     filter(acc_year %in% period_3$acc_year)
   
   exponetial.modelF3<-lm( log(claim_freq) ~ acc_year, data = agg_table_3)
   fitted.p3 <- exp(predict(exponetial.modelF3, newdata = period_all, type = "response"))
   
   #append to a single table
   exp.model.df <- exp.model.df %>% 
     cbind( option_1 = fitted.p1,
            option_2 = fitted.p2,
            option_3 = fitted.p3) %>%
     gather(model_option, modeled_value, all_year:option_3, factor_key=TRUE)
 })
  
    
  # Show the values in an HTML graph ----
  output$freq_trend <- renderPlot({
    input$option_1
    input$option_2
    input$option_3
    
    ggplot(agg_table,aes(x=acc_year,y=claim_freq)) +
      geom_line(linetype=4, size = 2)+
      #geom_smooth(method="loess", formula = y ~ x)+
      
      geom_line(data = fitted_models(), aes(x = acc_year , y = modeled_value, colour = model_option), size=2, linetype=1)+
      
      scale_x_continuous(breaks = seq(min(period_all$acc_year),max(period_all$acc_year), by = 1))+
      theme_classic()+
      ggtitle("PL Trend")+
      xlab("AY") + ylab("Frequency")
    
    
  })
  
}

##__________________________________________________
# next step, make individule tabs for severity, and LR trends
# include a toggle for including or excluding a period
# also need a table to show the modeled factors, R^2, MSE, etc.


# Create Shiny app ----
shinyApp(ui, server)
