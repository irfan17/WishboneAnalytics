library(tidyverse)
library(usmap)
library(XML)
library(xml2)
library(stringr)
library(ggplot2)
library(urbnmapr)

vote_url <- function(congress, session, vote_number){
  url <- paste0('https://www.senate.gov/legislative/LIS/roll_call_votes/vote',congress,session,'/vote_', congress,'_',session,'_',vote_number,'.xml')
  url
}
url <- vote_url(117,1,'00477')
data <- read_xml(url)
dat2 <- data %>% as_list()

memb <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(memb) <- c('Member','Last','First','Party','State','Vote','Member_ID')
i = 1
while(i <= length(dat2$roll_call_vote$members)){
  temp <- dat2$roll_call_vote$members[i]$member %>% as.data.frame()
  colnames(temp) <- c('Member','Last','First','Party','State','Vote','Member_ID')
  memb <- rbind(memb, temp)
  i = i + 1
}

test2 <- memb %>% select(c(State, Party, Vote)) %>% count(State,Party,Vote) %>% select(State, Party,Vote, n) %>% group_by(State)
states <- test2 %>% select(State) %>% unique()
states$col <- ''
states$party_col <- ''

#ifelse(test %>% filter(State == 'WA') %>% filter(Party == 'D') %>% nrow() == 0,0, test %>% filter(State == 'WA') %>% filter(Party == 'D') %>% pull(n))

i =1
while(i <= nrow(states)){
  temp <- test2[test2$State == states$State[i],]
  #temp_party <- test[test$State == states$State[i],]
  temp2 <- data.frame(cbind(c("Y","N"),0))
  colnames(temp2) <- c("vote","count")
  party <- data.frame(cbind(c("D","R"),0))
  colnames(party) <- c("party","count")
  temp2$count <- as.numeric(temp2$count)
  party$count <- as.numeric(party$count)
  temp3 <- temp %>% select(State, Party, n) %>% group_by(State,Party) %>% summarize(n=sum(n))
  temp2$count[1] <- ifelse(length(temp$n[temp$Vote == 'Yea']) > 0, as.numeric(temp$n[temp$Vote == 'Yea']), 0)
  temp2$count[2] <- ifelse(length(temp$n[temp$Vote == 'Nay']) > 0, as.numeric(temp$n[temp$Vote == 'Nay']), 0)
  party$count[1] <- ifelse(temp3 %>% filter(State == states$State[i]) %>% filter(Party == 'D') %>% nrow() == 0,0, temp3 %>% filter(State == states$State[i]) %>% filter(Party == 'D') %>% pull(n))
  party$count[2] <- ifelse(temp3 %>% filter(State == states$State[i]) %>% filter(Party == 'R') %>% nrow() == 0,0, temp3 %>% filter(State == states$State[i]) %>% filter(Party == 'R') %>% pull(n))
  #print(temp2)
  states$col[i] <- ifelse( temp2$count[temp2$vote == 'Y'] == 2, "Green",
                           ifelse(temp2$count[temp2$vote == 'N'] == 2, "Brown1","Yellow"))
  states$party_col[i] <- ifelse(party$count[party$party == 'D'] == 2, "CornflowerBlue",
                                ifelse(temp$State == 'VT', "CornflowerBlue",
                                       ifelse(party$count[party$party == 'R'] == 2, "Brown1","DarkViolet")))
  #print(states)
  i = i + 1
}

comb <- as.data.frame(left_join(states, urbnmapr::states, by = c("State" = "state_abbv")))

ggplot() + 
  geom_polygon(data = comb, mapping = aes(x = long, y = lat, group = group),
               fill = comb$party_col, color = "black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + theme_void() + ggtitle("117th Senate Delegations")
