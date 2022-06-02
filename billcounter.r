library(xml2)

bill_counter <- function(type){
setwd(paste0("bills/",type))
#library(xml2)
bills = data.frame(matrix(nrow = 0, ncol = 4))
colnames(bills) = c("bill","policy","sponsor","cosponsors")

i = 1
while(i <= length(dir())){
data = read_xml(dir()[i]) #read in xml file
policy = xml_text(xml_find_all(data, ".//policyArea//name"))[1]
sponsor = xml_text(xml_find_all(data, "bill/sponsors//fullName"))
cosponsors = xml_text(xml_find_all(data, "bill/cosponsors//fullName"))
cosponsors = if(is.na(cosponsors[1])){
    cosponsors <- as.character(sponsor)
    }else{
    cosponsors <- as.character(cosponsors)
    } #check for 1 cosponsor opps
num_co = length(cosponsors)
billnum = paste0(toupper(type),"_",xml_text(xml_find_all(data,".//billNumber")))
temp = data.frame(matrix(nrow = num_co, ncol = 3)) 
colnames(temp) = c("bill","policy","sponsor")
temp$bill = billnum
temp$policy = policy
temp$sponsor = sponsor
temp = cbind(temp,cosponsors)
bills = rbind(bills,temp)
print(i)
i = i + 1
}
bills
}