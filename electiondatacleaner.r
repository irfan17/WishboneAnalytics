#### Clean up and transform the 2000-2020 election dataset ####
library(tidyverse)
library(usmap)
library(urbnmapr)
# Source data from s3 bucket

data <- read.csv('https://map-tool-wishbone.s3.amazonaws.com/RawData/countypres_2000-2020.csv')

# pull out states and counties
states <- data %>% select(state, state_po) %>% unique()
counties <- data %>% select(state, state_po, county_name, county_fips) %>% unique()

#drop version column

data <- data %>% select(-version)

#collapse and summarize 2020 data (to aggregate votes of all types)

data %>% select(-mode) %>% group_by(party, county_name, state) %>% summarize(candidatevotes = sum(candidatevotes), across()) %>% distinct() %>% data.frame()

#remove "counties" with zero votes

data <- data %>% filter(totalvotes > 0)

#pad fips codes to make compatible with usmaps

data$county_fips <- as.character(data$county_fips)

data$county_fips <- str_pad(data$county_fips,5,"left","0")

#create an id column

data$id <- paste0(data$year,"_",data$county_fips,"_",data$party)

#calculate the winners of each race in each county
winners <- data %>% group_by(year, county_name, county_fips, office) %>% filter(candidatevotes == max(candidatevotes, na.rm = TRUE))

#designate winners in the main dataset 
data$winners <- ifelse(data$id %in% winners$id, 1, 0)
data$perc <- data$candidatevotes/data$totalvotes
winners$perc <- winners$candidatevotes/winners$totalvotes
winners$color <- ifelse(winners$party == 'DEMOCRAT', "BLUE", ifelse(winners$party == 'REPUBLICAN', "BROWN","BLACK"))

#join with usmaps data (for 2020 only here)

merged_county_data <- function(year){
  comb <- as.data.frame(left_join((winners %>% filter(year == year)), urbnmapr::counties, by = c("county_fips" = "county_fips")))
  comb
}

merged_state_data <- function(year){
  comb <- as.data.frame(left_join((winners %>% filter(year == year)), urbnmapr::states, by = c("state_po" = "state_abbv")))
  comb
}

g <- ggplot() + 
geom_polygon(data = comb, mapping = aes(x = long, y = lat, group = group),
                                      fill = comb$color, color = "black") +
       coord_map(projection = "albers", lat0 = 39, lat1 = 45) + theme_void()