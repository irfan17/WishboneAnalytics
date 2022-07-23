library(dplyr)
library(ggplot2)
library(urbnmapr)
library(maps)

data <- read.csv("countypres.csv")

data_clean <- data %>% select(c('year','state','state_po','county_name','county_fips','office','candidate','party','candidatevotes','totalvotes'))
data_clean <- aggregate(candidatevotes ~.,data_clean, FUN = sum)

winners <- data_clean %>% group_by(year, county_name, county_fips, office) %>% filter(candidatevotes == max(candidatevotes, na.rm = TRUE))
winners <- winners %>% filter(party %in% c('DEMOCRAT','REPUBLICAN'))
winners$color <- ifelse(winners$party == 'DEMOCRAT','CornflowerBlue','Brown1')
winners <- winners %>% filter(totalvotes > 0)

i = 1
while(i <= length(winners$county_fips)){
    if(nchar(winners$county_fips[i]) < 5){
        winners$county_fips[i] <- paste0('0',winners$county_fips[i])
    }
    i = i + 1
}

comb <- as.data.frame(left_join(winners, urbnmapr::counties, by = c("county_fips" = "county_fips")))


raw <- function(cycle, states){
    if(nchar(states) == 0){
      clean <- comb %>% filter(year == cycle)
    }else{
      clean <- comb %>% filter(year == cycle & state %in% toupper(states))
    }
    clean
}


swing_chart <- function(year1, year2, states=''){
    comb1 <- raw(year1, states)
    comb2 <- raw(year2, states)
    perc2 <- data.frame(comb2$order, ifelse(comb2$party == 'DEMOCRAT', 
        -1*comb2$candidatevotes/comb2$totalvotes,comb2$candidatevotes/comb2$totalvotes))
    colnames(perc2) <- c("order","perc2")
    comb3 <- left_join(comb1, perc2, by = "order")
    comb3$perc <- ifelse(comb3$party == 'DEMOCRAT', 
        -1*comb3$candidatevotes/comb3$totalvotes,comb3$candidatevotes/comb3$totalvotes)
    comb3$swing <- comb3$perc2 - comb3$perc
    comb3$color <- ifelse(comb3$swing >= 0, "brown1","cornflowerblue")
    fig <- ggplot() + 
    geom_polygon(data = comb3, mapping = aes(x = long, y = lat, group = group),
		fill = comb3$color, color = "black") +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + theme_void() + ggtitle(paste0("U.S. Presidential Election Swing Chart, ",year1," to ", year2))

    fig
}