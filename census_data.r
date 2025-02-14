library(tidyr)
library(tidycensus)
library(dplyr)
census_api_key('e103c2c14003029207ba50ff2a9a1fc7cd21a0bd')

education <- read.csv("Education.csv")
poverty <- read.csv("PovertyEstimates.csv")
unemp <- read.csv("Unemployment.csv")

education <- education %>% filter(grepl('2016-20',Attribute))
education <- education %>% filter(grepl('Percent of',Attribute))

poverty <- poverty %>% filter(grepl('PCTPOVALL_2020',Attribute)) 

county_demo_state <- function(state, year){
  tot <- get_acs(geography = "county", variables = "B02001_001", state = toupper(state), year = year)
  white <- get_acs(geography = "county", variables = "B02001_002", state = toupper(state), year = year) %>% select(GEOID, estimate)
  black <- get_acs(geography = "county", variables = "B02001_003", state = toupper(state), year = year) %>% select(GEOID, estimate)
  native <- get_acs(geography = "county", variables = "B02001_004", state = toupper(state), year = year) %>% select(GEOID, estimate)
  asian <- get_acs(geography = "county", variables = "B02001_005", state = toupper(state), year = year) %>% select(GEOID, estimate)
  pi <- get_acs(geography = "county", variables = "B02001_006", state = toupper(state), year = year) %>% select(GEOID, estimate)
  other <- get_acs(geography = "county", variables = "B02001_007", state = toupper(state), year = year) %>% select(GEOID, estimate)
  multiple <- get_acs(geography = "county", variables = "B02001_008", state = toupper(state), year = year) %>% select(GEOID, estimate)
  all <- list(tot, white,black,native,asian,pi,other,multiple)
  comb <- all %>% reduce(full_join, by='GEOID')
  comb$moe <- NULL
  colnames(comb) <- c("GEOID", "NAME", "variable","Total","White","Black","Native","Asian","PI","Other","Multiple")
  comb$White <- round(comb$White/comb$Total,3)
  comb$Black <- round(comb$Black/comb$Total,3)
  comb$Native <- round(comb$Native/comb$Total,3)
  comb$Asian <- round(comb$Asian/comb$Total,3)
  comb$PI <- round(comb$PI/comb$Total,3)
  comb$Other <- round(comb$Other/comb$Total,3)
  comb$Multiple <- round(comb$Multiple/comb$Total,3)
  comb$Total <- round((comb$White + comb$Black + comb$Native + comb$Asian + comb$PI + comb$Other + comb$Multiple),3)
  comb$Diversity <- 0
  i <- 1
  while(i <= nrow(comb)){
  comb$Diversity[i] <- simpson(c(comb$White[i],comb$Black[i],comb$Native[i],comb$Asian[i],comb$PI[i],comb$Other[i],comb$Multiple[i]))
  i = i + 1
  }
  comb
}

county_demo_all <- function(year){
  tot <- get_acs(geography = "county", variables = "B02001_001",  year = year)
  white <- get_acs(geography = "county", variables = "B02001_002",  year = year) %>% select(GEOID, estimate)
  black <- get_acs(geography = "county", variables = "B02001_003",  year = year) %>% select(GEOID, estimate)
  native <- get_acs(geography = "county", variables = "B02001_004",  year = year) %>% select(GEOID, estimate)
  asian <- get_acs(geography = "county", variables = "B02001_005",  year = year) %>% select(GEOID, estimate)
  pi <- get_acs(geography = "county", variables = "B02001_006",  year = year) %>% select(GEOID, estimate)
  other <- get_acs(geography = "county", variables = "B02001_007",  year = year) %>% select(GEOID, estimate)
  multiple <- get_acs(geography = "county", variables = "B02001_008",  year = year) %>% select(GEOID, estimate)
  all <- list(tot, white,black,native,asian,pi,other,multiple)
  comb <- all %>% reduce(full_join, by='GEOID')
  comb$moe <- NULL
  colnames(comb) <- c("GEOID", "NAME", "variable","Total","White","Black","Native","Asian","PI","Other","Multiple")
  comb$White <- round(comb$White/comb$Total,3)
  comb$Black <- round(comb$Black/comb$Total,3)
  comb$Native <- round(comb$Native/comb$Total,3)
  comb$Asian <- round(comb$Asian/comb$Total,3)
  comb$PI <- round(comb$PI/comb$Total,3)
  comb$Other <- round(comb$Other/comb$Total,3)
  comb$Multiple <- round(comb$Multiple/comb$Total,3)
  comb$Total <- round((comb$White + comb$Black + comb$Native + comb$Asian + comb$PI + comb$Other + comb$Multiple),3)
  comb$Diversity <- 0
  i <- 1
  while(i <= nrow(comb)){
    comb$Diversity[i] <- simpson(c(comb$White[i],comb$Black[i],comb$Native[i],comb$Asian[i],comb$PI[i],comb$Other[i],comb$Multiple[i]))
    i = i + 1
  }
  comb
}

#calculate % with bachelors degree
geo <- get_acs(geography = "county", variables = "B15003_022E", year = 2020)$GEOID
bach <- get_acs(geography = "county", variables = "B15003_022E", year = 2020)$estimate /
  (get_acs(geography = "county", variables = "B15003_001E", year = 2020)$estimate)
ed <- data.frame(geo,bach)            
colnames(ed) <- c("GEOID","Bach%")

