library(ggplot2)
library(dplyr)
source('billcounter.r')
source('HelperFunctions.r')

#bills <- bill_counter('S')

#bills <- write.csv(bills,'samplebills.csv',row.names = F)


issue_ideology <- function(issue){

    bills <- bills %>% filter(policy == issue)
    senators <- unique(union(bills$cosponsors, bills$sponsors))
    
    mat <- matrix(nrow=length(senators),ncol=length(senators))
    colnames(mat) <- senators
    rownames(mat) <- senators
    
    senator_db <- build_senators(senators)
    
    for(i in senators){
        for(j in senators){
            mat[i,j] <- sum(
                bills[bills$sponsor == i & bills$cosponsors == j,] %>% nrow(),
                bills[bills$sponsor == j & bills$cosponsors == i,] %>% nrow(),
                intersect(bills[bills$cosponsor == i,1] , bills[bills$cosponsor == j,1]) %>% length())


        }
    }
    #colnames(mat) <- senator_db$Short
    #rownames(mat) <- senator_db$Short
    mat <- sqrt(mat)
    load <- princomp(mat)$loadings[,1]
    run1 <- apply(data.frame(load), MARGIN = 2, FUN = function(x){(x-min(x))/(max(x)-min(x))})
    rownames(run1) <- senator_db$Short
    output <- run1[order(load),]
    output <- data.frame(names(output), output)
    colnames(output) <- c('names','output')
    output <- merge(output,senator_db, by.x = 'names', by.y = 'Short')
    output
}

ideology_plot <- function(issue1, issue2){
    i1 <- issue_ideology(issue1)
    i2 <- issue_ideology(issue2)
    names(i1)[names(i1) == 'output'] <- 'issue1'
    names(i2)[names(i2) == 'output'] <- 'issue2'
    
    comb <- merge(i1,i2[,c('names','issue2')], on = 'names')
    ggplot(comb, aes(x = issue1, y = issue2, label = names)) + 
        geom_point(color = ifelse(comb$Party == 'R', 'red', 'blue')) + 
        theme_classic() + geom_text(size = 4, hjust = 1, vjust = -1) +
        ggtitle(paste0('116th Senate, ', issue1, ' vs ',issue2)) +
        xlab(issue1) + ylab(issue2)
}

#ideology_plot('Taxation','Environmental Protection')


