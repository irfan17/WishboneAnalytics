#functions
library("sf")
library("tidyverse")
library("tigris")
library("RSQLite")
library("shiny")
library("cowplot")
library("ggiraph")
library("ggthemes")

con <- dbConnect(RSQLite::SQLite(), "data//wishbone.sqlite")
#state_data <- read.csv("data//state_data.csv")
state_data <- dbGetQuery(con, "select * from elections_states")
#state_winners <- read.csv("pres_state_winners.csv")
state_winners <- dbGetQuery(con, "select * from elections_states where candidate = winner")
#county_winners <- read.csv("pres_county_winners.csv")
county_winners <- dbGetQuery(con, "select * from elections_counties where candidate = winner")

dbDisconnect(con)

	
state_comb <- function(winners, yr = 2020){

winners <- winners %>% filter(year == yr)
geo <- tigris::states(cb = TRUE, resolution = "500k")
geo <- geo %>% shift_geometry() %>% filter(GEOID < 57)
geo$NAME <- toupper(geo$NAME)
comb <- merge(winners , geo, by.x = "state", by.y = "NAME")
comb <- comb %>% st_as_sf
comb$dem_margin <- comb$dem_perc - comb$gop_perc
comb
}

county_comb <- function(winners, yr = 2020){

winners <- winners %>% filter(year == yr)
geo <- tigris::counties(cb = TRUE, resolution = "500k")
geo <- geo %>% shift_geometry() %>% filter(GEOID < 57)
geo$NAME <- toupper(geo$NAME)
#geo$county_fips <- paste0(geo$STATEFP, geo$COUNTYFP)
geo$key <- paste0(geo$STUSPS, geo$GEOID)
comb <- merge(winners , geo, by = "key")
comb <- comb %>% st_as_sf
comb$dem_margin <- comb$dem_perc - comb$gop_perc
comb
}

static <- function(data, type = 'shaded', states='All'){
  if('All' %in% states){
    dat = data  
  } else {
    dat = data %>% filter(state %in% states)
  }  
  
  states <- ifelse(states == 'All', 'United States',states)
  
  if(type == 'shaded'){
    m <- ggplot(data = dat, aes(fill = dem_margin,data_id = county_name)) + 
      geom_sf_interactive(color = 'gray33',size = 0.2, alpha = 1) + 
      theme_void()+scale_fill_gradient2_interactive(high = 'cornflowerblue', mid = 'white', low = 'brown1', 
                                        name = 'Margin', guide = 'legend', midpoint = 0, n.breaks = 10, labels = abs)
  } else {
    
    m <- ggplot(data = dat, aes(fill = winning_party)) + 
      geom_sf_interactive(color = 'gray33',size = 0.2, alpha = 0.5) + 
      theme_void()+scale_fill_manual_interactive(values = c('DEMOCRAT' = 'cornflowerblue','REPUBLICAN' = 'brown1'), 
                                     name = 'Party', guide = 'legend', labels = c("DEMOCRAT" = "Democrat","REPUBLICAN" = "Republican"))  
    
  }
  
  
  t <- m + ggtitle(paste0(dat$year," Presidential Election"),states) 
  t
}



### Assuming a dataframe df with the candidate and votes ###

sub <- state_data %>% select(year,candidate,party,candidatevotes) %>% group_by(year, candidate, party) %>% summarize(Votes = sum(candidatevotes))

two <- function(yr,sub){
    df <- sub %>% filter(year == yr)
    df <- df[order(df$Votes, decreasing = TRUE),][1:2,]
	df$party <- gsub("DEMOCRAT","Democrat", df$party)
	df$party <- gsub("REPUBLICAN","Republican", df$party)
    df
}

	
resultbar <- function(yr, sub){

# two <- function(yr,sub){
#     df <- sub %>% filter(year == yr)
#     df <- df[order(df$Votes, decreasing = TRUE),][1:2,]
# 	df$party <- gsub("DEMOCRAT","Democrat", df$party)
# 	df$party <- gsub("REPUBLICAN","Republican", df$party)
#     df
# }

df <- two(yr, sub)

df$adj <- round(100*(df$Votes/sum(df$Votes)),2)

b <- ggplot(data = df, aes(x = "",  y = adj), fill = candidate) + 
    geom_col(width = 1, fill = ifelse(df$party == 'Democrat','cornflowerblue',ifelse(df$party == 'Republican','brown1','gold'))) + 
    coord_flip() + theme_void() + xlab("") + 
    geom_text(label = paste0(df$candidate,"\n ",df$adj,"%"), size = 4, position = position_stack(vjust = 0.5), 
              color = 'white', family = 'mono', fontface = 'bold') + geom_hline(yintercept = sum(df$adj)/2, color = 'white')

b
}

