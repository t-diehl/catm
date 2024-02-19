#Sentiment & Dictionary Analysis  

#Introduction to Computational Text Analysis Methods (CTAM)
#Dr. Trevor Diehl
#Central Michigan University 
#School of Media and Cinematic Arts 
#diehl1th@cmich.edu



#Quanteda Package: Quantitative Analysis of Textual Data
#https://quanteda.io/index.html

#Official Tutorials: https://tutorials.quanteda.io/introduction/

#Packages

install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("tm")

library(quanteda)
library(quanteda.textstats)
library(tm)
library(quanteda.textplots)
library(ggplot2)


#Workflow:
#Step 1: Import text and check format 
#Step 2: Clean, standardize, and inspect text
#Step 3: Analyze, model and visualize 



#Step 1: Import the data
#Download files and put them into your WD:
#https://github.com/t-diehl/catm

#Set your WD now if you haven't already 

load("~/Desktop/CATM Course Resources /news_data.RData")

d <- news_data[ , c(1,3:4)] #remove the headlines variable by selecting specific column 

d$type <- factor(d$type, labels = c("real", "fake", "satire"))


#Step 2: Inspect and clean the text 
#quick inspect cleaning
head(d, 1) #returns the first row

#quick standardization of text
#some dictionaries rely on stop words for context, so let's leave them in for now

d$text = removePunctuation(d$text) #remove punctuation 
d$text = char_tolower(d$text) #lower case
d$text = gsub(' j |-|[^\x01-\x7F]', " ", d$text)#removes emoticons (gsub uses grep commands)
d$text = gsub('\n', " ", d$text)# remove the \n and replace with empty space 
d$text = gsub(' [a-z] ', " ", d$text)#remove lonely letters 

#Inspect cleaning performance 
head(d, 1)


#Step 3: Arrange data into matrix as required, analyze and visualize 

#getting warnings b/c the package updated and commands are being phased out 
sent_dfm = dfm(d$text, dictionary = data_dictionary_LSD2015) #LSD = Lexicoder Sentiment Dictionary (built in package!)
head(sent_dfm) #check the 'head' of the matrix

#convert document feature matrix back into the data frame, merge sentiment counts to the original data
d2 = convert(sent_dfm, to = "data.frame")
d = cbind(d, d2[ ,2:5]) #just take the last 4 colunmns 

#check that variables are written properly 
head(d[ ,3:7])#just inspect the last 4 colunmns 

#crete the sentiment variables based on % of overall word count 


#a handful of stop words are in the LSD, so truncate before setting word count
d$text = removeWords(d$text, stopwords(kind = "en"))#remove stop words
d$wc = ntoken(d$text)#word count

#net sentiment based on previous work
d$net_sent = ((d$positive - d$neg_positive)/d$wc) - ((d$negative - d$neg_negative)/d$wc)

rm(d2, sent_dfm)

#insoect means and boxplot 
boxplot(net_sent~type,data=d, main="net sentiment by type",
        xlab="type", ylab="net sent")


#dot plot 
library(ggplot2)

ggplot(d) +
  aes(x = type, y = net_sent, color = type) +
  geom_jitter() +
  theme(legend.position = "none")

#check normality before anova
hist(d$net_sent)

#anova
anova <- aov(net_sent ~ type,
               data = d)

#results
summary(anova)


#plot distribution of residuals + anova
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(anova$residuals)

# QQ-plot
library(car)
qqPlot(anova$residuals,
       id = FALSE )# id = FALSE to remove point identification

#Regression

lm = lm(net_sent ~ type, data = d)
summary(lm)



