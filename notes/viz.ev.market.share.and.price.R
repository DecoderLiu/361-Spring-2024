## viz.ev.market.share.data.r
library(tidyverse)
library(pubtheme)

## Manually create the data frame using data from those visualizations
df = data.frame(year = 2015:2023, 
                ev = c(0.3, 0.4, 0.5, 1.2, 1.3, 
                       1.6, 2.6, 5.2, 6.9), 
                hev = c(1.8, 2, 2.1, 2.5, 3.5, 
                        4, 5, 5.5, 8.3))
df$other = 100 - df$ev - df$hev
df

## stacked bar chart. First let's pivot longer
dg = df %>%
  pivot_longer(cols = c(ev, hev, other)) %>%
  mutate(year = as.character(year)) ## df$year = as.character(df$year)

## two equivalent ways to do same thing
pivot_longer(df, cols = c(ev, hev, other))

df %>%
  pivot_longer(cols = c(ev, hev, other))

dg

ggplot(dg, 
       aes(x = value, 
           y = year,
           fill = name)) +
  geom_col() 


## copy from pubtheme and edit
title = "EV, HEV, and Other Vehicle Market Share" 
g = ggplot(dg, 
           aes(x = value, 
               y = year, 
               label = round(value,1), 
               fill = name)) +
  geom_col(width = 0.8) + 
  #geom_text(hjust = -0.1) + ## optional numbers with reasonable number of digits
  labs(title    = title,
       x        = 'Market Share', ## Optional. 
       y        = 'Year')  ## Optional. Upper Lower.
g

g %>%
  pub(type = 'bar')


## Stacked bar chart without "other"
title = "EV, HEV, and Other Vehicle Market Share" 
g = ggplot(dg %>% filter(name != 'other'), ## same as dg[dg$name != 'other',]
           aes(x = value, 
               y = year, 
               label = round(value,1), 
               fill = name)) +
  geom_col(width = 0.8, ) + 
  #geom_text(hjust = -0.1) + ## optional numbers with reasonable number of digits
  labs(title    = title,
       x        = 'Market Share', ## Optional. 
       y        = 'Year')  ## Optional. Upper Lower.
g

g %>%
  pub(type = 'bar')


title = "EV, HEV, and Other Vehicle Market Share" 
g = ggplot(dg %>% filter(name != 'other'), ## same as dg[dg$name != 'other',]
           aes(x = value, 
               y = year, 
               label = round(value,1), 
               fill = name)) +
  geom_col(width = 0.8, 
           position = position_dodge(width = 0.8)) + 
  geom_text(hjust = -0.1, 
            position = position_dodge(width = 0.8)) + 
  labs(title    = title,
       x        = 'Market Share', ## Optional. 
       y        = 'Year') + ## Optional. Upper Lower.
  theme(legend.position = 'top')
g

g %>%
  pub(type = 'bar')

## flip the coords
## need to change hjust to vjust
g = ggplot(dg %>% filter(name != 'other'), ## same as dg[dg$name != 'other',]
               aes(y = value, 
                   x = year, 
                   label = round(value,1), 
                   fill = name)) +
  geom_col(width = 0.8, 
           position = position_dodge(width = 0.8)) + 
  geom_text(vjust = -0.1,
            position = position_dodge(width = 0.8)) + 
  labs(title    = title,
       y        = 'Market Share', ## Optional. 
       x        = 'Year')  ## Optional. Upper Lower.

g 



## What chatGPT suggested
ggplot(dg, aes(x = year, y = value, fill = name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("blue", "red", "grey")) +
  labs(x = "Year", y = "Market Share (%)", 
       title = "Market Share of EVs, HEVs, and Other Vehicles",
       subtitle = "Source: IEA, 2019") +
  theme_minimal() +
  theme(legend.position = "bottom")

## F-150 

df = data.frame(date = c('2022-04-01', 
                         '2022-08-01', 
                         '2022-10-01',
                         '2022-12-01', 
                         '2023-03-01', 
                         '2023-07-01'), 
                price = c(39974, 
                          46974, 
                          51974, 
                          55974, 
                          59974,
                          49995))

df

g = ggplot(df, 
           aes(x = date, 
               y = price)) + 
  geom_line() + 
  geom_point()
g

## need to convert date to date
df$date = as.Date(df$date)

g = ggplot(df, 
           aes(x = date, 
               y = price)) + 
  geom_line() + 
  geom_point()
g

## add text labels to the points
g = ggplot(df, aes(x = date, 
                   y = price, 
                   label = dollar(price))) + 
  geom_line() + 
  geom_point() + 
  geom_text(hjust = -0.1) + 
  scale_x_date(limits = c(as.Date('2022-03-01'), 
                          as.Date('2023-08-30'))) + 
  scale_y_continuous(labels = dollar)
g

df$price 
dollar(df$price)
df$label = dollar(df$price)
df
