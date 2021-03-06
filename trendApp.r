library(shiny)
library("janitor")
library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")


#setwd("C:/Users/QinH/OneDrive - Willis Towers Watson/Desktop/PL Trend R")
setwd("F:/Hao Qin/trendStudy")

agg_table <- read_excel("PL_trend_sample_data.xlsx") %>%
  clean_names()

#calculating frequency / 1000 premium
agg_table$claim_freq <- agg_table$ult_claim_count/agg_table$on_level_premium*1000 

#calculating severity
agg_table$loss_severity<- agg_table$ult_aggregate_loss/agg_table$ult_claim_count 

#calculating loss ratio
agg_table$loss_ratio<- agg_table$ult_aggregate_loss/agg_table$on_level_premium 

#incorporating any prediction years
period_all =  data.frame(acc_year = c(agg_table$acc_year, 2020,2021))

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PL Trend - D&O (US+Intl) (AY PY Combined)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      #input: trending parameters
      radioButtons("trend_basis", "Trend Basis:",
                   c("Frequency" = "freq",
                     "Severity" = "sevr",
                     "Loss Ratio" = "lr")),
      
      # br() element to introduce extra vertical spacing ----
       br(),
      
      # Input: period 1 ----
      sliderInput(inputId = "option_1", 
                  label = "Period 1:",
                  min = 2007, 
                  max = 2019,
                  value = c(2007,2018),
                  sep=""),
      
      # Input: period 2 ----
      sliderInput(inputId = "option_2", 
                  label = "Period 2:",
                  min = 2007, 
                  max = 2019,
                  value = c(2010,2015),
                  sep=""),
      
      # Input: period 3 ----
      sliderInput(inputId = "option_3", 
                  label = "Period 3:",
                  min = 2007, 
                  max = 2019,
                  value = c(2012,2019),
                  sep="")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: tabset w/ plot, summary and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Long-term trend", 
                           plotOutput("plot_trend"), 
                           verbatimTextOutput("summary")),
                  
                  tabPanel("Short-term trend", 
                           plotOutput("plot_trend_short"), 
                           verbatimTextOutput("summary2")
                           )
      
    )
    )
  )
)
# Define server logic for slider examples ----
server <- function(input, output) {
  
  # selection parameter ----
  b <-  reactive({
    basis <-switch(input$trend_basis,
                   "freq" = "claim_freq",
                   "sevr" = "loss_severity",
                   "lr" = "loss_ratio"
                   )
  })
  
  period_1 <- reactive({
    p1 <- data.frame(acc_year = c(seq(min(input$option_1),max(input$option_1),1)))
  })
  
  period_2 <- reactive({
    p2 <- data.frame(acc_year = c(seq(min(input$option_2),max(input$option_2),1)))
  })
  
  period_3 <- reactive({
    p3 <- data.frame(acc_year = c(seq(min(input$option_3),max(input$option_3),1)))
  })
  
  # Reactive expression to create data frame of all input values ----
  # long-term fitted trends
  fitted_modelall <- reactive({
    f <- as.formula(paste("log(",b(),") ~ acc_year"))
    
    exponetial.model<-lm( f, data = agg_table)
    fitted.allYear <- exp(predict(exponetial.model, period_all, type = "response"))
    exp.model.df <- data.frame(x = period_all,
                               all_year = fitted.allYear)
  })
  
  fitted_model1 <- reactive({
    f <- as.formula(paste("log(",b(),") ~ acc_year"))
    
    period_1 <- data.frame(acc_year = c(seq(min(input$option_1),max(input$option_1),1)))
    agg_table_1 <- agg_table %>%
      filter(acc_year %in% period_1$acc_year)
    
    exponetial.model1<-lm( f, data = agg_table_1)
    
  })
  
  fitted_model2 <- reactive({
    f <- as.formula(paste("log(",b(),") ~ acc_year"))
    
    period_2 <- data.frame(acc_year = c(seq(min(input$option_2),max(input$option_2),1)))
    agg_table_2 <- agg_table %>%
      filter(acc_year %in% period_2$acc_year)
    
    exponetial.model2<-lm( f, data = agg_table_2)
    
  })
  
  fitted_model3 <- reactive({
    f <- as.formula(paste("log(",b(),") ~ acc_year"))
    
    period_3 <- data.frame(acc_year = c(seq(min(input$option_3),max(input$option_3),1)))
    agg_table_3 <- agg_table %>%
      filter(acc_year %in% period_3$acc_year)
    
    exponetial.model3<-lm( f, data = agg_table_3)
    
  })
  
  combined_results <- reactive({
    
    fitted.p1 <- exp(predict(fitted_model1(), newdata = period_all, type = "response"))
    fitted.p2 <- exp(predict(fitted_model2(), newdata = period_all, type = "response"))
    fitted.p3 <- exp(predict(fitted_model3(), newdata = period_all, type = "response"))
    
    exp.model.df_combined <- fitted_modelall() %>% 
      cbind( option_1 = fitted.p1,
             option_2 = fitted.p2,
             option_3 = fitted.p3) %>%
      gather(model_option, modeled_value, all_year:option_3, factor_key=TRUE)
    
  })
  

  # Show the long-term trend in graph ----
  output$plot_trend <- renderPlot({
    
    
    ggplot(agg_table,aes_string(x="acc_year",y=b())) +
      geom_line(linetype=4, size = 2)+
      #geom_smooth(method="loess", formula = y ~ x)+
      
      geom_line(data = combined_results(), aes(x = acc_year , y = modeled_value, colour = model_option), size=2, linetype=1)+
      
      scale_x_continuous(breaks = seq(min(period_all$acc_year),max(period_all$acc_year), by = 1))+
      theme_classic()+
      ggtitle("PL Trend")+
      xlab("AY") + ylab(b())
    
    
  })
  
  # show the long-term trend statistics ----
 output$summary <- renderPrint({
    
    periods <- c(paste(min(input$option_1),max(input$option_1),sep = "-"),
                 paste(min(input$option_2),max(input$option_2),sep = "-"),
                 paste(min(input$option_3),max(input$option_3),sep = "-"),
                 "Average")
    
    fit_trend <- c(fitted_model1()$coef[2],
                   fitted_model2()$coef[2],
                   fitted_model3()$coef[2])
    
    r_sqrd <- c(summary(fitted_model1())$r.squared,
                summary(fitted_model2())$r.squared,
                summary(fitted_model3())$r.squared)
    
    mse <- c(mean(fitted_model1()$residuals^2),
             mean(fitted_model2()$residuals^2),
             mean(fitted_model3()$residuals^2))
    
    summary_table <- data.frame(fit_trend,
                                r_sqrd,
                                mse) %>%
      rbind( c( mean(fit_trend),mean(r_sqrd),mean(mse))) %>%
      mutate(across(where(is.numeric), ~ round (., digits = 3)))
      
    rownames(summary_table) <- periods
    
    print(summary_table)
    
  })
  
  
  
  
  #----------------------------------------------------------------------------------------------
  
  # short-term fitted trends
  
  ## could not use cbind, since now the model is fitted only on the selected years, not all years##
  combined_results_short <- reactive({
    
    p_all =  data.frame(acc_year = c(agg_table$acc_year, 2020,2021))
    
    fitted.s.p1 <- data.frame(exp(predict(fitted_model1(), newdata = period_1(), type = "response"))) %>%
      setNames(c("fitted1"))
    
    fitted.s.p2 <- data.frame(exp(predict(fitted_model2(), newdata = period_2(), type = "response"))) %>%
      setNames(c("fitted2"))
    
    fitted.s.p3 <- data.frame(exp(predict(fitted_model3(), newdata = period_3(), type = "response"))) %>%
      setNames(c("fitted3"))
  
    
    join_table_short <- p_all %>%
      left_join(fitted.s.p1 %>%
                  mutate(acc_year = seq(min(input$option_1),max(input$option_1),1)), 
                by = "acc_year") %>%
      left_join(fitted.s.p2 %>%
                  mutate(acc_year = seq(min(input$option_2),max(input$option_2),1)),
               by = "acc_year") %>%
      left_join(fitted.s.p3 %>%
                 mutate(acc_year = seq(min(input$option_3),max(input$option_3),1)),
              by = "acc_year") %>%
      gather(model_option, modeled_value, fitted1:fitted3, factor_key=TRUE)
    
    })


  
  
   #Show the short-term trend in graph ----
  output$plot_trend_short <- renderPlot({
    
    
    ggplot(agg_table,aes_string(x="acc_year",y=b())) +
      geom_line(linetype=4, size = 2)+
      
     geom_line(data = combined_results_short(), aes(x = acc_year , y = modeled_value, colour = model_option), size=2, linetype=1)+
      
      scale_x_continuous(breaks = seq(min(period_all$acc_year),max(period_all$acc_year), by = 1))+
      theme_classic()+
      ggtitle("PL Trend")+
      xlab("AY") + ylab(b())
    
    
  })
  
  output$summary2 <- renderPrint({
    
    periods <- c(paste(min(input$option_1),max(input$option_1),sep = "-"),
                 paste(min(input$option_2),max(input$option_2),sep = "-"),
                 paste(min(input$option_3),max(input$option_3),sep = "-"),
                 "Average")
    
    fit_trend <- c(fitted_model1()$coef[2],
                   fitted_model2()$coef[2],
                   fitted_model3()$coef[2])
    
    r_sqrd <- c(summary(fitted_model1())$r.squared,
                summary(fitted_model2())$r.squared,
                summary(fitted_model3())$r.squared)
    
    mse <- c(mean(fitted_model1()$residuals^2),
             mean(fitted_model2()$residuals^2),
             mean(fitted_model3()$residuals^2))
    
    summary_table <- data.frame(fit_trend,
                                r_sqrd,
                                mse) %>%
      rbind( c( mean(fit_trend),mean(r_sqrd),mean(mse))) %>%
      mutate(across(where(is.numeric), ~ round (., digits = 3)))
    
    rownames(summary_table) <- periods
    
    print(summary_table)
    
  })
  
  
}

##__________________________________________________



# Create Shiny app ----
shinyApp(ui, server)
