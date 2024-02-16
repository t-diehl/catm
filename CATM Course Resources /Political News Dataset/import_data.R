#Importing the Benjamin Political News dataset
#Based on the paper:https://arxiv.org/abs/1703.09398
#data source: https://github.com/BenjaminDHorne/fakenewsdata1

#Dr. Trevor Diehl
#Central Michigan University 
#School of Media and Cinematic Arts 
#diehl1th@cmich.edu


library(quanteda)
library(readtext)

#set the higher level folder path
path_data <- "/Users/trevordiehl/Desktop/Public Data/Random Poltical News Dataset"

#Fake News 
fake_text <- readtext(paste0(path_data, "/FAKE/*"),
                      dvsep = "_",
                      encoding = "ISO-8859-1")


fake_head <- readtext(paste0(path_data, "/FAKE_titles/*"),
                      dvsep = "_", 
                      encoding = "ISO-8859-1")

d = cbind(fake_head, fake_text[2])
d$type = 1
colnames(d) <- c("id", "headline", "text", "type")

#Real News Import
real_text <- readtext(paste0(path_data, "/real/*"),
                      dvsep = "_",
                      encoding = "ISO-8859-1")


real_head <- readtext(paste0(path_data, "/real_titles/*"),
                      dvsep = "_", 
                      encoding = "ISO-8859-1")

d2 = cbind(real_head, real_text[2])
d2$type = 2
colnames(d2) <- c("id", "headline", "text", "type")


#Satire News Import
sat_text <- readtext(paste0(path_data, "/satire/*"),
                      dvsep = "_",
                      encoding = "ISO-8859-1")


sat_head <- readtext(paste0(path_data, "/satire_titles/*"),
                      dvsep = "_", 
                      encoding = "ISO-8859-1")

d3 = cbind(sat_head, sat_text[2])
d3$type = 3
colnames(d3) <- c("id", "headline", "text", "type")

#Merge and save
news_data = rbind(d, d2, d3)

#save it
save(news_data, file = "fake_news.RData")

