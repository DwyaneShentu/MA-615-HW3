#1
>library(readr)
>covid <- read_csv("C:/Users/73113/Downloads/covid_data.csv",col_types = cols(
>date = col_date(format = "%Y-%m-%d"), ), na = c("missing"))
> problems(covid)
# A tibble: 0 x 5
# i 5 variables: row <int>, col <int>, expected <chr>, actual <chr>,
#   file <chr>

#2
>library(dplyr)
>filtered <- covid %>% filter(!is.na(race), !is.na(state))
>race1 <- filtered %>% group_by(state, date) %>% summarize
(race2 = n_distinct(race))
>race9 <- race1 %>% filter(race2 == 9)
>state1 <- race9 %>% group_by(state) %>% summarize(reporting_days = n())
>state1 <- state1 %>% filter(reporting_days > 0)
>state1 <- state1 %>% arrange(desc(reporting_days))
>state
# A tibble: 14 x 2
state reporting_days
<chr>          <int>
  1 WA                95
2 CA                94
3 CO                90
4 MN                57
5 RI                53
6 IL                43
7 KS                41
8 DE                38
9 NH                37
10 IN                26
11 NV                19
12 UT                14
13 DC                 5
14 GA                 2
#(WA) had the highest number of reporting days while (GA) is the lowest, 
#and the trend is decreasing gradually.

#3
>library(lubridate)
>filtered <- covid %>% filter(race == "Total", wday(date) == 1, 
!is.na(Cases))
>weekdays <- unique(wday(filtered$date))
>filtered <- filtered %>% arrange(state, date) %>% group_by(state) %>% mutate(
lag_date = lag(date), week_diff = as.numeric(difftime(date, lag_date, units
= "weeks")))
>filtered <- filtered %>% filter(!(is.na(lag_date) | week_diff == 1))
>filtered
# A tibble: 1 x 11
# Groups:   state [1]
date       state race  state_name population Cases Deaths  Hosp Tests
<date>     <chr> <chr> <chr>           <dbl> <dbl>  <dbl> <dbl> <dbl>
  1 2020-05-10 ID    Total Idaho         2078513  2230     67    NA    NA
# i 2 more variables: lag_date <date>, week_diff <dbl>

#4
>library(ggplot2)
>cases <- covid %>% filter(!is.na(Cases))
>cases_month <- cases %>% group_by(race, state) %>% summarize(first_month = 
min(month_year))
> cases_plot <- ggplot(cases_month, aes(x = first_month)) + geom_bar() +
   facet_wrap(~race) + labs(x = "First Month for Cases",y = "States",
 + title = "Number of States by First Reporting Month for Cases") 
  +theme_minimal()
> print(cases_plot)

>hosp <- covid %>% filter(!is.na(Hosp))
>hosp <- hosp %>% mutate(month_year = format(date, "%Y-%m"))
>hosp_month <- hosp %>% group_by(race, state) %>% summarize(first_month = 
min(month_year))
> hosp_plot <- ggplot(hosp_month, aes(x = first_month)) +geom_bar()+ facet_wrap
(~race) + labs(x = "First Month for Hosp",y = "States", + title = "Number of 
States by First Reporting Month for Hosp ") +theme_minimal()
> print(hosp_plot)
#reporting cases from 2020-4, hospitalalizations from 2020-6. White and 
#Black races are the most people.
