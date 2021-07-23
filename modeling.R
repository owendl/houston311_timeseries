library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(lubridate)
library(stringr)
library(tsibble)
library(plotly)


# read data ---------------------------------------------------------------
df19=read.csv("../Houston311_storm_rec_police_traffic2019.csv",sep = ";")
df18=read.csv("../Houston311_storm_rec_police_traffic2018.csv",sep = ";")
df17=read.csv("../Houston311_storm_rec_police_traffic2017.csv",sep = ";")

# combine data ------------------------------------------------------------
df=rbind(df17,df18,df19)

# create date -------------------------------------------------------------
df$creationDate<-as.Date(as.character(df$SR.CREATE.DATE), format="%Y-%m-%d %H:%M:%S")

# weekly departmet --------------------------------------------------------

weekly_qty_by_sr <- df %>%
  mutate(ticket_week = yearweek(creationDate)) %>%
  group_by(DIVISION, ticket_week) %>%
  summarise(total.qty = n())

x <- seq(as.Date("2017-01-01"), as.Date("2019-09-30"), by = "1 day")
yw=unique(yearweek(x))
week_years=expand.grid(year_week=yw,
                       DIVISION=unique(df$DIVISION))

weeks2=merge(week_years, weekly_qty_by_sr, by.x=c("year_week","DIVISION"), 
             by.y=c("ticket_week","DIVISION"), all.x=TRUE)

weeks2$total.qty[is.na(weeks2$total.qty)]<-0

# hold out data -----------------------------------------------------------

train=weeks2[weeks2$year_week<as.Date("2019-04-30"),]
test=weeks2[!weeks2$year_week<as.Date("2019-04-30"),]
test$key='actual'

# Nest --------------------------------------------------------------------

weeks2_nest <- train %>%
  group_by(DIVISION) %>%
  nest()

test_nest<- test[,c("DIVISION","total.qty")] %>%
  group_by(DIVISION) %>%
  nest()

# create time_series -------------------------------------------------------------

weeks2_ts <- weeks2_nest %>%
  mutate(data.ts = map(.x       = data, 
                       .f       = tk_ts, 
                       select   = -year_week, 
                       start    = 2017,
                       freq     = 52))

weeks2_fit_ets <- weeks2_ts %>%
  mutate(fit = map(data.ts, ets))

weeks2_fit_arima <- weeks2_ts %>%
  mutate(fit = map(data.ts, auto.arima))

weeks2_fit_hw <- weeks2_ts %>%
  mutate(fit = map(data.ts, HoltWinters))

weeks2_fit_ets$model='ets'
weeks2_fit_arima$model='arima'
weeks2_fit_hw$model='hw'

weeks2_fit<-bind_rows(weeks2_fit_arima,weeks2_fit_ets, weeks2_fit_hw)
# weeks2_fit<-bind_rows(weeks2_fit_arima,weeks2_fit_ets)


# Plot residuals ----------------------------------------------------------
augment_fit <- weeks2_fit %>%
  mutate(augment = map(fit, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)

sr=unique(df$DIVISION)

pdf("division_residuals_plots.pdf")
for (s in sr){
  for (m in unique(augment_fit$model)){
    gg<-ggplot(augment_fit[(augment_fit$DIVISION==s)& (augment_fit$model==m),],aes_string(x = "date", y = ".resid")) +
      geom_hline(yintercept = 0, color = "grey40") +
      geom_line(color = palette_light()[[2]]) +
      geom_smooth(method = "loess") +
      labs(title = paste("Division:",s,"   Model:", m),
           subtitle = paste("Model:",m, "Residuals"), x = "") + 
      theme_tq() +
      scale_x_date(date_labels = "%Y-%m")
  print(gg)
  }
}
dev.off()

# Forecast ----------------------------------------------------------------

weeks2_forecast<- weeks2_fit %>%
  mutate(fcast = map(fit, forecast, h = 22))

weeks2_forecast=merge(weeks2_forecast, test_nest, by = "DIVISION",all.x = TRUE)
  
weeks2_forecast<-weeks2_forecast %>%
  mutate(acc=map2(fcast, data.y, function(x,y){accuracy(x,y[[1]])}))

weeks2_forecast=weeks2_forecast %>%
  mutate(RMSE = map(acc, function(x){unlist(x[2,c("RMSE")])}),
         MASE = map(acc, function(x){unlist(x[2,c("MASE")])}))# %>%


result <- weeks2_forecast %>% 
  group_by(DIVISION) %>%
  filter(MASE == min(unlist(MASE)))
    
result<-result %>%
mutate(fcast_long = map(fit, forecast, h = 40))

result2<-result %>%
mutate(sweep = map(fcast_long, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

# Plot Model Forecasts ----------------------------------------------------
sr=unique(result$DIVISION)
pdf("division_model_forecast_plots.pdf")
for (s in unique(result2$DIVISION)) {
  df<-result2[result2$DIVISION==s,]
  gg<-ggplot(df,aes(x = index, y = total.qty, color = key)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    geom_line() +
    labs(title = s,
         subtitle = paste("Model Forecast:",df$model[1], "MASE:", df$MASE),
         x = "", y = "Reported Incidents") +
    #scale_x_date(date_breaks = "1 quarter", date_labels = "%Y") +
    scale_x_date(date_breaks = "12 weeks", date_labels = "%Y-%m") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line(data=test[test$DIVISION==s,], mapping=aes(x=year_week, y=total.qty, color=key))
  print(gg)
}
dev.off()

# Best Models -------------------------------------------------------------
better=result[Reduce("&",list(result$MASE<1,result$DIVISION!="Test")),]
better=better[order(unlist(better$MASE)),]     

better<- better %>%
  mutate(sweep = map(fcast_long, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

for (s in unique(better$DIVISION)) {
  df<-better[better$DIVISION==s,]
  gg<-ggplot(df,aes(x = index, y = total.qty, color = key)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    geom_line() +
    labs(title = s,
         subtitle = paste("Model Forecast:",df$model[1], "MASE:", df$MASE) ,
         x = "", y = "Reported Incidents") +
    #scale_x_date(date_breaks = "1 quarter", date_labels = "%Y") +
    scale_x_date(date_breaks = "12 weeks", date_labels = "%Y-%m") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line(data=test[test$DIVISION==s,], mapping=aes(x=year_week, y=total.qty, color=key))

  htmlwidgets::saveWidget(ggplotly(gg),  paste(s," ",df$model[1],'.html', sep=""))
}
dev.off()

result$MASE2<-unlist(result$MASE)
gg<-ggplot(data=result, aes(x=MASE2)) +geom_histogram()+labs(title = "Histogram of MASE Errors for Division Time Series Models",
                                                           x = "MASE", y = "Count") + 
  scale_x_continuous(breaks = seq(0, 6, by = 0.5)) +
  theme(axis.text=element_text(size=12))
gg
