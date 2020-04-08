## Preprocessing script for Corona Sentiment Study Project
## Author : Daniel Pericich
## Date : 4/7/20
library(dplyr)
library(rvest)
library(lubridate)
library(stringr)

## Store main news page as object for rvest
url = 'https://www.whitehouse.gov/news/'

## Create list of all pages based off of news site naming procedure
## use url + 'page/#/'
pages <- c(1:763)
list_of_urls <- c()
for(val in pages) {
  list_of_urls <- append(list_of_urls, paste(url, 'page/', val, '/', sep = ""))
}

## Create 3 empty lists to store headlines, links and dates
full_headlines <- c()
full_links <- c()
full_dates <- c()

## Loop through all pages on whitehouse.gov/news to pull all headlines, links and dates
for(page in pages) {
  headlines <- read_html(list_of_urls[page]) %>% html_nodes('.briefing-statement__title a') %>% html_text()
  links <- read_html(list_of_urls[page]) %>% html_nodes('.briefing-statement__title a') %>% html_attr('href')
  dates <- read_html(list_of_urls[page]) %>% html_nodes('time') %>% html_text()
  
  full_headlines <- append(full_headlines, headlines)
  full_links <- append(full_links, links)
  full_dates <- append(full_dates, dates)
  
}

## Remove extra dates from full_dates list
full_dates <- full_dates[1:5669]

## Create Dataframe of the dates, headlines and links. Then change the Dateframe to a tibble
df <- cbind(full_dates, full_headlines, full_links)
colnames(df) <- c('Date', 'Headline', 'Link')
df <- as_tibble(df)
df <- df %>% mutate(Dates = mdy(full_dates))
df <- df %>% select(Dates, Headline, Link)

## Create year column to remove all articles from before January 1, 2020
df <- df %>% mutate(Year = year(Dates))
df <- df %>% filter (Year >= 2020)

## Filter dataframe to only contain Press Briefings that talk about Coronavirus and the President's remarks
df <- df %>% mutate(Headlines = ifelse(grepl("*Press Briefings*", Headline), Headline, ""))
df <- df %>% mutate(Headlines = ifelse(grepl("*Corona*", Headline), Headline, ""))
#df <- df %>% mutate(Headlines = ifelse(grepl("*Remarks by President Trump*", Headline), Headline, ""))
df_briefs <- df %>% filter(Headlines != "")




