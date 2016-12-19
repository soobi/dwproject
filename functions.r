library(readxl)
library(tidyverse)
library(stringr)
library(choroplethr)
library(choroplethrMaps)
library(broom)
library(rvest)
library(readxl)
library(gdata)

# Read.xls only allows the download of one sheet in an excel file, so I sought to use a function to pull multiple sheets in an excel file.
# This is a modified version of the function given by Jeromy Anglin on Stack Overflow: http://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
# Modification is primarily to specify the exact sheet numbers to pull from the excel document (and name the objects in the list by these sheet numbers).
# This is a very inefficient method to grab the data, as the excel document is re-downloaded for every sheet. Much faster to use download the full file once or to use the API. 
read_excel_allsheets <- function(url, sheets) {
  x <- lapply(sheets, function(X) 
    read.xls(url, sheet = X))
  names(x) <- sheets
  x
}

## Function to make data frames of the list objects while also cleaning and assigning index values.  
make_df <- function(obj) {
  y <- as.data.frame(obj)
  statename <- gsub(":.*$","",y[1,1])
  statename <- gsub("^.*?for","",statename)
  statename <- gsub("^.*?for","",statename)
  y <- clean_df(y) 
  y <- y %>%
    mutate(State = as.character(statename))
  y
}

## Specific function to remove the header rows from the data, and tidy the language names
clean_rows <- function(df){
  df <- df[!grepl("language", df[,1]),]
  df <- df[!grepl("incl",df[,1]),]
  df <- df[!grepl("LANGUAGE",df[,1]),]
  df <- df[!grepl("AND",df[,1]),]
  df$Langs <- gsub("[.]","",df$Langs)
  df
}

## Function to format the columns and remove rows with missing values. 
clean_df <- function(df){
  df <- df[-c(1,2), -c(3,4,5)]
  colnames(df)[1] <- "Langs"
  colnames(df)[2] <- "speakers"
  df[,1] <- as.character(df[,1])
  df[,2] <- as.numeric(gsub(",","", df[,2]))
  df <- df %>%
    filter(!is.na(speakers)) %>%
    select(Langs,speakers)
  df <- clean_rows(df)
  df[1,1] <- "Population"
  df[2,1] <- "Only English"
  df
}

## Function to utilize the previous functions and bind the separate dataframes into one 
make_full_df <- function(datalist){
  x = data.frame(matrix("",ncol = 3, nrow = 0))
  colnames(x) <- c("Langs","speakers","State")
  num = 1:length(datalist)
  
  for(num in 1:length(datalist)){
    y <- make_df(datalist[num])
    x <- rbind(x,y)}
  x
  }

## Function to clean up the index to match the specifications for the chloropleth regions
clean_index <- function(df){
  index <- df
  index$County <- as.character(index$County)
  index <- separate(index, County, c("county.name","state.abb"), sep = ",")
  index$county.name <- index$county.name %>%
    str_replace(" Municipality","") %>%
    str_replace(" County","") %>% 
    str_replace(" Parish","") %>%
    str_replace(" city","") %>%
    str_to_lower() %>%
    str_replace("prince george s","prince george's") %>%
    trimws()
  index
}

clean_df_counties <- function(df){
  index <- df
  index$State <- as.character(index$State)
  index <- separate(index, State, c("county.name","state.abb"), sep = ",")
  index$county.name <- index$county.name %>%
    str_replace(" Municipality","") %>%
    str_replace(" County","") %>% 
    str_replace(" Parish","") %>%
    str_replace(" city","") %>%
    str_to_lower() %>%
    str_replace("prince george s","prince george's") %>%
    trimws()
  index$county.name <- trimws(index$county.name)
  index$state.abb <- trimws(index$state.abb)
  index
}

## Function to find the sheet numbers for a specific state
find_county_index <- function(df, statesel){
  index <- clean_index(df)
  index$state.abb <- trimws(index$state.abb)
  index <- index %>%
    filter(state.abb == statesel)
  index
}


## Function to download the county data for specified index of sheets 
create_state_set <- function(index){
  state_sheets <- as.vector(index$Table)+1 # Adjusted by one to account for the first index sheet in the file
  state_data <- read_excel_allsheets(url2, state_sheets)
  state_data                         
}

## Function to join the county data with state-specific county data
join_data_and_region <- function(df1){
  data(county.regions)
  df1 <- clean_df_counties(df1)
  regions <- left_join(df1, county.regions)
}


## Function to prepare state-specific county data for a state choropleth
choro_state_set <- function(statesel){
  index <- find_county_index(counties_raw, statesel)
  raw <- create_state_set(index)
  data <- make_full_df(raw)
  choro <- join_data_and_region(data)
  choro <- tidy_population(choro)
  choro
}


## Function to pull the surveyed population total, to use as a ratio
tidy_population <- function(df){
  populations <- df %>%
    filter(Langs == "Population") %>%
    select(speakers, region)
  colnames(populations)[1] <- "POP"
  
  data <- left_join(df, populations)
  data <- data %>%
    group_by(region,Langs) %>%
    mutate(percent = 100*(speakers / POP)) %>%
    filter(Langs != "Population") %>%
    select(Langs, speakers, region, percent) %>%
    gather(dataType, value, speakers, percent)
  
}

## Function to plot the county choropleth for a state for a specific language
choro_state_zoom <- function(df, lang, datatype, state){
  data <- df %>%
    filter(Langs == lang,dataType == datatype)
  county_choropleth(data, state_zoom = c(state), title = paste0(lang," ",datatype," in", " ", state),num_colors = 7)
}

