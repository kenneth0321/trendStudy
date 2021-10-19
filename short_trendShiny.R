library("janitor")
library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")

#setwd("C:/Users/QinH/OneDrive - Willis Towers Watson/Desktop/PL Trend R")
setwd("F:/Hao Qin/trendStudy")

agg_table <- read_excel("PL_trend_sample_data.xlsx") %>%
  clean_names()

b <- switch(3,"claim_freq","loss_severity","loss_ratio")

#loss ratio
agg_table$loss_ratio<- agg_table$ult_aggregate_loss/agg_table$on_level_premium 

#uses all year data to fit a curve and then predict new year

exponetial.modelF<-lm( as.formula(paste("log(",b,") ~ acc_year")), data = agg_table)

summary(exponetial.modelF)

all_year =  data.frame(acc_year = c(agg_table$acc_year, 2020,2021))

fitted.allYear <- exp(predict(exponetial.modelF, all_year, type = "response"))
exp.model.df <- data.frame(x = all_year,
                           all_year = fitted.allYear)


#no need to plot so many different curves, save each simulated freq as an additional column to the agg_table, then plot by columns
#user input period 1 curve fitting
period_1 <- data.frame(acc_year = c(seq(2008,2015,1)))
agg_table_1 <- agg_table %>%
  filter(acc_year %in% period_1$acc_year)

exponetial.modelF1<-lm( as.formula(paste("log(",b,") ~ acc_year")), data = agg_table_1)
fitted.p1 <- exp(predict(exponetial.modelF1, newdata = period_1, type = "response"))

agg_table_1 <- agg_table_1 %>%
  mutate(fitted1 = fitted.p1)


#user input period 2 curve fitting
period_2 <- data.frame(acc_year = c(seq(2010,2019,1)))
agg_table_2 <- agg_table %>%
  filter(acc_year %in% period_2$acc_year)

exponetial.modelF2<-lm( as.formula(paste("log(",b,") ~ acc_year")), data = agg_table_2)
fitted.p2 <- exp(predict(exponetial.modelF2, newdata = period_2, type = "response"))

agg_table_2 <- agg_table_2 %>%
  mutate(fitted2 = fitted.p2)

#user input period 3 curve fitting
period_3 <- data.frame(acc_year = c(seq(2012,2017,1)))
agg_table_3 <- agg_table %>%
  filter(acc_year %in% period_3$acc_year)

exponetial.modelF3<-lm( as.formula(paste("log(",b,") ~ acc_year")), data = agg_table_3)
fitted.p3 <- exp(predict(exponetial.modelF3, newdata = period_3, type = "response"))

agg_table_3 <- agg_table_3 %>%
  mutate(fitted3 = fitted.p3)


join_test1 <- agg_table %>%
  left_join(agg_table_1 %>%
              select(acc_year,fitted1), 
            by = "acc_year") %>%
  left_join(agg_table_2 %>%
              select(acc_year, fitted2),
            by = "acc_year") %>%
  left_join(agg_table_3 %>%
              select(acc_year, fitted3),
            by = "acc_year")







fitted.p1.df <- data.frame(exp(predict(exponetial.modelF1, newdata = period_1, type = "response")))
joint_test2 <- agg_table %>%
  left_join(fitted.p1.df %>%
              mutate(acc_year = period_1$acc_year),
            by = "acc_year")







#append to a single table
exp.model.df <- exp.model.df %>%
  left_join(join_test1 %>%
              select(acc_year,
                     fitted1,
                     fitted2,
                     fitted3),
  by = "acc_year") %>%
    gather(model_option, modeled_value, all_year:fitted3, factor_key=TRUE)


# plot(agg_table$acc_year,agg_table$claim_freq, pch=16, ylim = c(1e-7,4e-6),xlim = c(2007,2021))
# lines(xAxis,fitted.allYear,col="red",xlab="AY", ylab="freqency")

testPlot <- ggplot(agg_table, aes (x=acc_year,y=loss_ratio)) +
  geom_line(linetype=4, size = 1)+
  #geom_smooth(method="loess", formula = y ~ x)+
  
  geom_line(data = exp.model.df, aes(x = acc_year , y = modeled_value, colour = model_option), size=2, linetype=1)+
  
  scale_x_continuous(breaks = seq(min(all_year$acc_year),max(all_year$acc_year), by = 1))

testPlot




#severity trend
agg_table$lossSeverity<- agg_table$`Ult. Aggregate Loss`/agg_table$`Ult. Claim Count` 
exponetial.modelS<-lm(log(agg_table$lossSeverity)~ agg_table$`Acc Year`)

fitted.allYear<- exp(predict(exponetial.modelS,list(agg_table$`Acc Year`)))

plot(agg_table$`Acc Year`,agg_table$lossSeverity, pch=16)
lines(agg_table$`Acc Year`,fitted.allYear,lwd=,col="red",xlab="AY", ylab="Severity")



