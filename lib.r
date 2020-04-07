library(rvest)
library(tidyverse)
library(stringr)
library(zoo)
library(RCurl)
library(lubridate)


# get John Hopkins summary time series data
get_jh_summary_data <- function(){
    data_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    file_name_confirmed = "time_series_covid19_confirmed_global.csv" 
    file_name_death = "time_series_covid19_deaths_global.csv" 
    file_name_recovered = "time_series_covid19_recovered_global.csv"
    
    c_download <- getURL(paste0(data_url, file_name_confirmed))
    d_download <- getURL(paste0(data_url, file_name_death))
    r_download <- getURL(paste0(data_url, file_name_recovered))
                         
    confirmed = read.csv(text=c_download, stringsAsFactors = F) %>% transform_data_frame() %>% rename(confirmed = num)
    dead = read.csv(text=d_download, stringsAsFactors = F) %>% transform_data_frame() %>% rename(dead = num)
    recovered = read.csv(text=r_download, stringsAsFactors = F) %>% transform_data_frame() %>% rename(recovered = num)
    
    df <- confirmed %>% 
        left_join(recovered, on = c("Province.State", "Country.Region", "date")) %>% 
        left_join(dead, on = c("Province.State", "Country.Region", "date")) %>%
        replace_na(list(recovered = 0, dead = 0, confirmed = 0))
    
    return(df)
}

transform_data_frame <- function(df){
    df %>% gather(key = "date", value = "num", matches("X.*")) %>%
        mutate(date = mdy(substring(date, 2)))
}


# get john hopkins daily report
get_jh_detail_data <- function(date = today()){
    data_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
    filename = paste0(data_url, format(date, "%m-%d-%Y"), ".csv")
    print(filename)
    download = getURL(filename)
    return(read.csv(text=download, stringsAsFactors = F))
}

# initial load from John Hopkins
initial_load <- function(end_date){
    dates <- seq(as.Date("2020-01-22"), as.Date("2020-01-24"), by="days")
    df <- NA
    for(dt in dates){
        filename = paste0("daily_", format(as_date(dt), "%Y%m%d"), ".csv")
        if(is.na(df)){
            df <- get_jh_detail_data(as_date(dt))
            saveRDS(filename)
        } else {
            df <- bind_rows(df, get_jh_detail_data(as_date(dt)))
        }
    }
    return(df)
}


########
# Scrape Wikipedia
########

get_number <- function(input_str){
    str_replace_all(input_str, "\\[\\d+\\]", "") %>% as.numeric()
}

get_stats_wiki <- function(){
    url <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Australia"
    page <- read_html(url)
    
    tables <- html_nodes(page,'.wikitable')
    
    c1 <- c("Confirmed", "Recovered", "Deaths", "Existing")
    t1 = tables[1] %>% html_table() %>% as.data.frame() %>% mutate_at(c1, get_number) %>% na.locf()
    colnames(t1) <- c("Date", c1)
    
    c2 = c("NSW.b..c.", "Qld", "Vic","SA","WA","Tas","ACT","NT","Total.a.","New.Cases")
    t2 = tables[2] %>% html_table() %>% as.data.frame() %>% mutate_at(c2, get_number) %>% na.locf()
    colnames(t2) <- c("Date", "NSW", "Qld", "Vic","SA","WA","Tas","ACT","NT","Total","New.Cases")
    
    list("stats"=t1, "new_cases" = t2)
}


reformat_ts <- function(df){
    df %>% gather(key = "date", value = n, matches("X.*")) %>% 
        mutate(date = mdy(substring(date, 2))) %>%
        group_by(Country.Region, Province.State, date) %>%
        mutate(cumsum = cumsum(n)) %>% ungroup() %>%
        arrange(Country.Region, Province.State, date) %>% view()
}

