#loading libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(twitteR)
library(tm)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'oiWrvtvCcHvvvJHbnNcNKMv2v'
consumer_secret <- 'DlFURJ7U7iFKWDwTTMDQT4b5wRtwtrak17l7PGn4XeV1EZX5U7'
access_token <- '1109333902009655296-CC8SPf8m3iTzoynn0L7mD8Np5jY2Zs'
access_secret <- 'TmUmTaalj3mlr8lUzLUsM85iy51TcM8M8ynSpYAuAlLKv'

?searchTwitteR # help

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tesla <- twitteR::searchTwitter('#Tesla', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(tesla) # posts that have hashtag Tesla, language Enligh, if NULL all sort of languges, since a particular date

ford <- twitteR::searchTwitter('#Ford', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(ford) # converting into dataframe 

mercedes <- twitteR::searchTwitter('#Mercedes', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(mercedes)

# Viewing the dataframe
View(a)

# longitude, latitude - location where the tweet came from 

# tesla, ford, mercedes = lists 
# d, e, a = dataframes

# Cleaning the datasets (this is a robus way of cleaning)
# Remove http and https elements manually
d$text <- gsub("http[^[:space:]]*","",  d$text) # For http
d$text <- gsub("http[^[:space:]]*","", d$text) # For https

e$text <- gsub("http[^[:space:]]*","",  e$text) # For http
e$text <- gsub("http[^[:space:]]*","", e$text) # For https

a$text <- gsub("http[^[:space:]]*","",  a$text) # For http
a$text <- gsub("http[^[:space:]]*","", a$text) # For https


#tokenizing all 3 datasets from twitter
tidy_tesla <- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_ford <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_mercedes <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

View(tidy_tesla)

#################################################
### Combining all 3 tidy data frames and creating correlograms
#################################################

library(tidyr)
frequency <- bind_rows(mutate(tidy_tesla, author="Tesla"), # bind_rows function combines all the dataframe, mutate function creates a new column -- so that we can manipulate only one dataframe
                       mutate(tidy_ford, author= "Ford"),
                       mutate(tidy_mercedes, author="Mercedes")) %>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>% # here we are starting to build a correlogram
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Ford`, `Mercedes`)

frequency 

View(frequency)

#let's plot the correlograms:
library(scales)

ggplot(frequency, aes(x=proportion, y=`Tesla`, 
                      color = abs(`Tesla`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Tesla", x=NULL)

#Taking a look at correlation coefficients -- FINAL EXAM!!
# correlation means the relationship between the datasets in text - how similar is this text to the one we're comparing 
# 48 is kinda a high one and even higher for Tesla & Mercedes - because they are doing a similar thing using technology and stuff

cor.test(data=frequency[frequency$author == "Ford",],
         ~proportion + `Tesla`)

cor.test(data=frequency[frequency$author == "Mercedes",],
         ~proportion + `Tesla`)

############################################
## Sentiment analysis 
#############################################
library(tidytext)
get_sentiments('afinn') # Show example of the table

# pulling in sentiment for these 3 tokenized datasets
tidy_tesla %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_ford %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_mercedes %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

# grouping by id because we want to get all the words that are in that tweeets and we want to sum or substract all the values of all the words that a tweet have -- we can see that this tweet is positive, this tweet is negative

#let's take a look at the most positive and most negative tokens in the tesla dataset

tidy_tesla_sentiment <- tidy_tesla %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

print(tidy_tesla_sentiment)

# the word super is mentioned 114 times 

tidy_tesla_sentiment %>%
  group_by(sentiment) %>% # we can group by anything we want and get an idea how the sentiment is for specific dimension that I selected
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# these are the positive and negative words that were the most frequently mentioned 

############################################
## TF-IDF analysis
#############################################
# shows the most characteristic words in the particular dataset
# e.g. Tesla - Tesla on the Tesla side but not Ford 
# I want to see stuff that it was mentioned just in Tesla

combined_cars <- bind_rows(mutate(d, make="Tesla"),
                           mutate(e, make= "Ford"),
                           mutate(a, make="Mercedes")
)

tesla_modif <- combined_cars %>%
  unnest_tokens(word, text) %>%
  count(make, word, sort=TRUE) %>% # counting the number of words
  ungroup() # descending order, not 100% sure 

tesla_modif2 <- tesla_modif %>%
  group_by(make) %>% # grouping by make
  summarize(total=sum(n))

tesla_leftjoined <- left_join(tesla_modif, tesla_modif2)

tidy_tesla_tfidf <- tesla_leftjoined %>%
  bind_tf_idf(word, make, n)

tidy_tesla_tfidf # we get all the zeors because we are looking at stop words ... too common

tidy_tesla_tfidf %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
tidy_tesla_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(make) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=make))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~make, ncol=2, scales="free")+
  coord_flip()

# lots of insights from this -- we can see mustang in word -- why are people talking about mustang when it comes to ford -- go back to dataset and find that out
# why these companies are being tweeted about 
# shows how characteristic are the words for each company 
