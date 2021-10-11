library("janitor")
library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")

setwd("C:/Users/QinH/OneDrive - Willis Towers Watson/Desktop/PL Trend R")

df <- read_excel("PL_trend_sample_data.xlsx") %>%
  clean_names()

all_year =  data.frame(acc_year = c(df$acc_year, 2020,2021))

b <- switch(1,"claim_freq","loss_severity","loss_ratio")

#uses all year data to fit a curve and then predict new year
df$claim_freq <- df$ult_claim_count/df$on_level_premium*1000 

p1=c(seq(2010,2016,1))
p2=c(seq(2014,2019,1))
p3=c(seq(2010,2018,1))

f <- as.formula(paste("log(",b,") ~ acc_year"))

exponetial.modelF<-lm( f, data = df)
fitted.allYear <- exp(predict(exponetial.modelF, all_year, type = "response"))
exp.model.df <- data.frame(x = all_year,
                           all_year = fitted.allYear)

period_1 <- data.frame(acc_year = p1)
agg_table_1 <- df %>%
  filter(acc_year %in% period_1$acc_year)

exponetial.modelF1<-lm( f, data = agg_table_1)
fitted.p1 <- exp(predict(exponetial.modelF1, newdata = all_year, type = "response"))


#user input period 2 curve fitting
period_2 <- data.frame(acc_year = p2)
agg_table_2 <- df %>%
  filter(acc_year %in% period_2$acc_year)

exponetial.modelF2<-lm( f, data = agg_table_2)
fitted.p2 <- exp(predict(exponetial.modelF2, newdata = all_year, type = "response"))


exp.model.df <- exp.model.df %>% 
  cbind( option_1 = fitted.p1,
         option_2 = fitted.p2) %>%
  gather(model_option, modeled_value, all_year:option_2, factor_key=TRUE)

testPlot <- ggplot(df,aes_string(x="acc_year",y=b)) +
  geom_line(linetype=4, size = 2)+
  #geom_smooth(method="loess", formula = y ~ x)+
  
  geom_line(data = exp.model.df, aes(x = acc_year , y = modeled_value, colour = model_option), size=2, linetype=1)+
  
  scale_x_continuous(breaks = seq(min(all_year$acc_year),max(all_year$acc_year), by = 1))

testPlot



#put into function 

plot_trends <- function(df,ploting_basis,pp_1,pp_2,pp_3){
  
  f <- as.formula(paste("log(",b,") ~ acc_year"))
  
  exponetial.modelF<-lm( f, data = df)
  
  fitted.allYear <- exp(predict(exponetial.modelF, all_year, type = "response"))
  exp.model.df <- data.frame(x = all_year,
                             all_year = fitted.allYear)
  
  period_1 <- data.frame(acc_year = pp_1)
  agg_table_1 <- df %>%
    filter(acc_year %in% period_1$acc_year)
  
  exponetial.modelF1<-lm( f, data = agg_table_1)
  fitted.p1 <- exp(predict(exponetial.modelF1, newdata = all_year, type = "response"))
  
  
  #user input period 2 curve fitting
  period_2 <- data.frame(acc_year = pp_2)
  agg_table_2 <- df %>%
    filter(acc_year %in% period_2$acc_year)
  
  exponetial.modelF2<-lm( f, data = agg_table_2)
  fitted.p2 <- exp(predict(exponetial.modelF2, newdata = all_year, type = "response"))
  
  
  #user input period 3 curve fitting
  period_3 <- data.frame(acc_year = pp_3)
  agg_table_3 <- df %>%
    filter(acc_year %in% period_3$acc_year)
  
  exponetial.modelF3<-lm( f, data = agg_table_3)
  fitted.p3 <- exp(predict(exponetial.modelF3, newdata = all_year, type = "response"))
  
  #append to a single table
  exp.model.df <- exp.model.df %>% 
    cbind( option_1 = fitted.p1,
           option_2 = fitted.p2,
           option_3 = fitted.p3) %>%
    gather(model_option, modeled_value, all_year:option_3, factor_key=TRUE)
  
  
  # plot(agg_table$acc_year,agg_table$claim_freq, pch=16, ylim = c(1e-7,4e-6),xlim = c(2007,2021))
  # lines(xAxis,fitted.allYear,col="red",xlab="AY", ylab="freqency")
  
  testPlot <- ggplot(df,aes(x=acc_year,y=claim_freq)) +
    geom_line(linetype=4, size = 2)+
    #geom_smooth(method="loess", formula = y ~ x)+
    
    geom_line(data = exp.model.df, aes(x = acc_year , y = modeled_value, colour = model_option), size=2, linetype=1)+
    
    scale_x_continuous(breaks = seq(min(all_year$acc_year),max(all_year$acc_year), by = 1))
  
  testPlot
}

plot_trends(agg_table,b,p1,p2,p3)
