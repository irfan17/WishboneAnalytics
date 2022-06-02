library(dplyr)
library(stringr)

#test <- 'Sen. Wicker, Roger F. [R-MS]'

senator_parse <- function(senator){
    test <- senator
    test <- gsub('Sen. ','',test)
    brac <- str_extract(test, '\\[(.+?)\\]')
    test <- str_extract(test, '([^\\[]+)')
    last <- gsub(',','',str_extract(test,'^(.+?),'))
    test <- gsub(paste0(last,', '),'',test)
    first <- str_extract(test, '([^\\s]+)')
    test <- gsub(paste0(first, ' '),'',test)
    middle <- test
    short <- paste(last, brac)
    brac <- gsub('\\[','',brac)
    brac <- gsub('\\]','',brac)
    party <- substr(brac, 1,1)
    state <- substr(brac, 3,5)
    output <- data.frame('Senate',first, middle, last, party, state, short, senator)
    colnames(output) <- c('Chamber','First',"Middle","Last","Party","State", "Short", "Full")
    output
    }

build_senators <- function(senators){
    output <- data.frame(matrix(ncol = 8,nrow = 0))
    colnames(output) <- c('Chamber','First',"Middle","Last","Party","State", "Short","Full")
    i = 1
    while(i <= length(senators)){
        temp <- senator_parse(senators[i])
        output <- rbind(output, temp)
        i = i + 1
    }
    output
}






