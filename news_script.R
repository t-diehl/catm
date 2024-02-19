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
d$text = removePunctuation(d$text) #remove punctuation 
d$text = char_tolower(d$text) #lower case
d$text = gsub(' j |-|[^\x01-\x7F]', " ", d$text)#removes emoticons (gsub uses grep commands)
d$text = removeWords(d$text, stopwords("english"))#remove stop words (pronouns, prepositions, to be verbs, etc.)



#Inspect cleaning performance 
head(d, 1)

#Still some encoding artefacts leftover 
d$text = gsub('\n', " ", d$text)# remove the \n and replace with empty space 
d$text = gsub(' [a-z] ', " ", d$text)#

#Inspect cleaning performance again, OK better now  
head(d, 1)

#Step 3: Arrange data into corpus or matrix as required, then analyze and visualize 

####Word clouds####

#Arrange data 

#quanteda wants a corpus object for word clouds 
news_corp <- corpus(d, text_field = "text")

print(news_corp, 5) #see first 5 documents 

#subset the corpus (for real news only)
news_sub <- corpus_subset(news_corp, type == 'real') |> 
  tokens() |>
  dfm() |>
  dfm_trim(min_termfreq = 20, verbose = FALSE)

#Plot 
textplot_wordcloud(news_sub)


#Let's compare news type 
news_sub <- corpus_subset(news_corp, 
              type %in% c("real", "fake", "satire")) |>
  tokens() |>
  dfm() |>
  dfm_group(groups = type) |>
  dfm_trim(min_termfreq = 25, verbose = FALSE) |>
  textplot_wordcloud(comparison = TRUE)

####Top words & Frequencies####

#quanteda wants a document feature matrix for running stats
news_dfm <- tokens(d$text)|>
  dfm() |>
  dfm_trim(min_termfreq = 20, verbose = FALSE)

tstat_freq_news <- textstat_frequency(news_dfm, n = 50) #the stats command 
tstat_freq_news #inspect 

#plot
ggplot(tstat_freq_news, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")


#Let's compare by news type
freq_grouped <- textstat_frequency(tokens(news_corp) |>
                                     dfm(), 
                                   group = type)
                                    
freq_grouped


#Pick some keywords from the word cloud and inspect for type

#Trump
freq_trump <- subset(freq_grouped, freq_grouped$feature %in% "trump") 
freq_trump

#plot
ggplot(freq_trump, aes(x = frequency, y = group)) +
  geom_point() + 
  labs(x = "Frequency", y = NULL,
       title = 'Frequency of "trump"')

#Russia
freq_russia <- subset(freq_grouped, freq_grouped$feature %in% "russia") 
freq_russia

ggplot(freq_russia, aes(x = frequency, y = group)) +
  geom_point() + 
  labs(x = "Frequency", y = NULL,
       title = 'Frequency of "russia"')



#Now the top words by type
dfmat_weight_news <- news_corp |>
  tokens() |>
  dfm() |>
  dfm_weight(scheme = "prop")

# Calculate relative frequency by president
dat_freq_weight <- textstat_frequency(dfmat_weight_news, n = 15, 
                                      groups = type)

ggplot(data = dat_freq_weight, aes(x = nrow(dat_freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(dat_freq_weight):1,
                     labels = dat_freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")


#Key words in context
key_subset <- corpus_subset(news_corp, 
                          type %in% c("fake", "real"))

# Create a dfm grouped by type
dfmat_key <- tokens(key_subset) |>
  tokens_group(groups = type) |>
  dfm()

# Calculate keyness and determine Trump as target group
tstat_keyness <- textstat_keyness(dfmat_key, target = "real")

# Plot estimated word keyness
textplot_keyness(tstat_keyness)




