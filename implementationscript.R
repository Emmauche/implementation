install.packages("rtweet")
install.packages("httpuv")
install.packages("textmineR")
install.packages("GGally")
install.packages("data.table")
install.packages(ggrepe)
install.packages("esquisse") 
install.packages("rvg")
install.packages("officer")
install.packages("RColorBrewer")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("tidytext")
install.packages("stringr")
install.packages(readr)
install.packages(wordcloud)
install.packages(lubridate)
install.packages(scales)
install.packages(ggrepel)


library(RColorBrewer)
library(rvg)
library(esquisse)
library(GGally)
library(textmineR)
library(rtweet)
library(httpuv)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(stringr)
library(readr)
library(reshape2)
library(wordcloud)
library(lubridate)
library(scales)
library(ggrepel)



# streaming twitter data
# creating the parameters

sdata <- " #covid19uk,#COVID-19,#covid19UK,#govuk,#govUK,#GovUK,#borisjohnson,#endlockdownuk,#EndLockDownUK,#nhsUK +
,#coronavirusUK,#CORONA,#COVID,#2020,#cure,#lockdown,#hoax,#covid19uk,# fake,#Covid_19,#COVID19,#COVID2019,#CoronaVirus 
+ ,#COVIDIOTS,#conspiracy,#coronavirusuk"

# to stream for 30minutes
stime <-30*60 

# streaming for 30 minutes
rwt <-  stream_tweets(sdata = sdata, timeout = stime, parse = TRUE,
                      file_name = NULL,"lang = en",
                      verbose = TRUE)

# Twitter data was also collected by
??rtweets

tweets_covid19 <-search_tweets("#COVID-19", n= 2000, include_rts = TRUE, lang = "en")
tweets_corona <-search_tweets("#CORONA", n= 2000, include_rts = TRUE, lang = "en")
tweets_covid <-search_tweets("#COVID", n= 2000, include_rts = TRUE, lang = "en")
tweets_2020 <-search_tweets("#2020", n= 2000, include_rts = TRUE, lang = "en")
tweets_cure  <-search_tweets("#cure", n= 2000, include_rts = TRUE, lang = "en")
tweets_lockdown  <-search_tweets("#lockdown", n= 2000, include_rts = TRUE, lang = "en")
tweets_covid19uk  <-search_tweets("#covid19uk", n= 2000, include_rts = TRUE, lang = "en")
tweets_govuk  <-search_tweets("#govuk", n= 2000, include_rts = TRUE, lang = "en")
tweets_hoax  <-search_tweets("#hoax", n= 2000, include_rts = TRUE, lang = "en")
tweets_coronauk  <-search_tweets("#coronavirusuk", n= 2000, include_rts = TRUE, lang = "en")



# we will bind all the tweets in to one data frame

alltweets <- bind_rows(tweets_coronauk,tweets_hoax,tweets_govuk,tweets_lockdown,
                       tweets_cure,tweets_2020,tweets_covid,tweets_corona,tweets_covid19)
alltweets

# To view the collected data, We load library dplyr and use the glimpse function
hist(alltweets$followers_count, xlab = "followers count", xlim = c(0,100), main = "total followers in dataset", probability = TRUE)
hist(alltweets$retweet_count, xlab = "retweets count", xlim = c(0,100), main = "total retweets in dataset", probability = TRUE)

#ploting followers count against retweets count
ggplot(alltweets, aes(followers_count,retweet_count))+
  geom_point() +
  scale_x_continuous(labels = NULL)+
  scale_y_continuous(labels = NULL)+
  labs(title = "Distribution of followers to retweet",
       subtitle = "shows the relationship the user and tweeted message",
       caption = "Data from Twitter.com"
       )


  
# continuous variables
str(alltweets)
mean(alltweets$followers_count)
mean(alltweets$reply_count)
sd(alltweets$followers_count)

stable <- table(alltweets$screen_name)
stable
ftable <- table(alltweets$followers_count)
max(ftable)
min(ftable)

# creating a table of followers count and number of retweets
following <- matrix(alltweets$followers_count,alltweets$retweet_count, ncol = 2)

colnames(following) <- c("followers", "retweets")
following

count(alltweets,"screen_name")
plot(alltweets$followers_count)
# verifying that all variables in observation are complete
count(alltweets,"user_id")
count(alltweets)
count(alltweets, "followers_count")
counts <- table(alltweets$screen_name, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"

# confirming there are no empty values
alltweets %>% filter(is.na(screen_name))
alltweets %>% filter(is.na(followers_count))
alltweets %>% filter(is.na(verified))
alltweets %>% filter(is.na(retweet_count))
alltweets %>% filter(is.na(text))
# dealing with duplicate values

# determining users with same number of followers
duplicate <-data.frame(ftable)
duplicate[duplicate$Freq>1,]
alltweets[alltweets$followers_count %in% duplicate$Var1[duplicate$Freq>1],]

# This is to show if there are duplicate text
dupl <-data.frame(table(alltweets$text))
dupl[dupl$Freq>1,]
alltweets[alltweets$text %in% dupl$Var1[dupl$Freq>1],]

# To show the distribution of followers
barplot(ftable, ylim = c(0,50),xlab = "followers_count", ylab = "frequency", axis.lty = "solid", space = 2.5)


glimpse(alltweets)
# creating tibble of tweet text
raw_text <- tibble(section = alltweets$text)
raw_text

# plotted the tweets to view freqency of tweet by 4 hours 
timetweet <- ts_plot(alltweets, "4 hours ") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of covid related Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using Four-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
# determining the volume of all the followers
sum(alltweets$followers_count)

# checking when tweets were created
createdtweet <- table(alltweets$created_at)
retweetable <- table(alltweets$retweet_count)
followtweet <- table(alltweets$followers_count)

# we created a new dataframe containing the few components to work with
twdf <- alltweets[,c("screen_name","followers_count","text","media_type","retweet_count","verified","lang"), values_drop_na = TRUE] 
twdf
# retweet is a tweet re-shared by another user
# We use retweet_count to store number of retweets
# it helps to identify trends and popularity of a topic
# Sorting dataframe based on followers count in descending order

# Sorted new dataframe based on followers count in descending order
# to determine users with largest follows
sorttwdf <- arrange(twdf, desc(twdf$followers_count))
sorttwdf
# dealing with duplicate tweets
# We use unique functions to tackle duplication of tweets. example
sortunique <- unique(sorttwdf, by ="text")
sortunique
# At this stage data has been collected and reformated to the needed component.

# created regular expressions to tidy data

##################################################################
## Latent Dirichlet Allocation (LDA) approach
# created a new dataframe selecting text and status id for the first 19273 columns
data <- alltweets
data <- data %>% select(text,status_id) %>% head(19273)

#As observed from the text, there are many tweets which consist of 
#irrelevant information: such as RT, the twitter handle, punctuation, stopwords 
#(and, or the, and many more) and numbers. These will add noise to our dataset
#which we need to remove during the pre-processing stage
data$text <- sub("RT.*:", "", data$text)
data$text <- sub("@.* ", "", data$text)

replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

str_replace_all(data$text,replace_reg,"")
str_replace_all(data$text,unnest_reg,"")
str_replace_all(data$text,replace_reg1,"")
str_replace_all(data$text,replace_reg2,"")

# now we tokenize
text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)

text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)

text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)

text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)

tokens <- text_cleaning_tokens %>% filter(!(word==""))

tokens <- tokens %>% mutate(ind = row_number())

tokens <- tokens %>% group_by(status_id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)


tokens [is.na(tokens)] <- ""

tokens <- tidyr::unite(tokens, text,-status_id,sep =" " )

tokens$text <- trimws(tokens$text)

# A (Document Term Matrix) DTM was created, which is a sparse matrix 
# containing terms and documents as dimensions. The DTM was built to tokenise 
# (break up a sentence into 1 word or 2 words) text. 
#The algorithm was instructed to choose a window of maximum 2 words. 
# Because it is Twitter sentiment, a window size of 1–2 words, was allowed and 
# algorithm can decide the more important phrases to concatenate together

# Term frequency
# The term frequency matrix was used to shows word/phrase
# occurrence in the entire corpus of text. Terms less than 
# two(< 2 times) were discarded, as it does not add any value to the algorithm,
# and help to reduce computation time..


dtm <- CreateDtm(tokens$text,doc_names = tokens$status_id,ngram_window = c(1, 2))

# exploring the basic frequency
tf <- TermDocFreq(dtm = dtm)

original_tf <- tf %>% select(term, term_freq,doc_freq)

rownames(original_tf) <- 1:nrow(original_tf)
# Eliminating words appearing less than 2 times or in more than half of the
# documents

vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm

#With your DTM, the LDA algorithm was executed for topic modelling by 
#manually assigning number of topics “k”. The algorithm calculated a 
#coherence score to allow choosing the best topics from 1 to k and determine 
#the coherence score. 
#Coherence gives the probabilistic coherence of each topic; 
#it is a score that calculates if the words in the same topic make sense when 
#they are put together. This gives us the quality of the topics being produced. 
#The higher the score for the specific number of k, it means for each topic, 
#there will be more related words together and the topic will make more sense. 


k_list <- seq(1, 40, by = 1)

model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir"))

#model tuning
#choosing the best model

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)

# plotting

ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,40,1)) + ylab("Coherence")

# Upon plotting of the k, we realise that k = 12 gives us 
# the highest coherence score. In this case, even though 
# the coherence score is rather low and there will definitely 
# be a need to tune the model, such as increasing k to achieve better 
# results or have more texts. But for explanation purpose, 
# we will ignore the value and just go with the highest coherence score. 
# After understanding the optimal number of topics, 
# we want to have a peek of the different words within the topic.
# Each topic will have each word/phrase assigned 
# a phi value (pr(word|topic)) — probability of word given a topic.
# So we only take into account the top 20 values per word in each topic. 
# The top 20 terms will then describe what the topic is about.

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]

model$top_terms <- GetTopTerms(phi = model$phi, M = 40)

top40_wide <- as.data.frame(model$top_terms)

# The above picture shows the first 5 topics out of the 12 topics.
# The words are in ascending order of phi-value. 
# The higher the ranking, the more probable the word will belong to the topic.
# It seems like there are a couple of overlapping topics.
# It’s up to the analyst to think if we should combine the 
# different topics together by eyeballing or we can
# run a Dendogram to see which topics should be grouped together.
# A Dendogram uses Hellinger distance(distance between 2 probability vectors)
# to decide if the topics are closely related. For instance, the Dendogram 
# below suggests that there are greater similarity between topic 10 and 11.

model$topic_linguistic_dist <- CalcHellingerDist(model$phi)

model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")

model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])

plot(model$hclust)


# Visualisation
# We can create word cloud to see the words belonging to the certain topic,
# based on the probability. 
# Below represents topic 2. As ‘gopdebate’ is the most probable word in 
# topic2, the size will be the largest in the word cloud

#visualising topics of words based on the max value of phi

set.seed(1234)


final_summary_words <- data.frame(top_terms = t(model$top_terms))

final_summary_words$topic <- rownames(final_summary_words)

rownames(final_summary_words) <- 1:nrow(final_summary_words)

final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))

final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)


final_summary_words <- final_summary_words %>% group_by(topic,word)

final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)

word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))

pdf("cluster.pdf")
for(i in 1:length(unique(final_summary_words$topic)))
{  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))}
dev.off()

str(model)
model$r2
summary(model$coherence)
hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")
# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
head(t(model$top_terms))

# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")     
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)
head(model$labels)
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]

assignments <- predict(model, dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)
assignments_dot <- predict(model, dtm,
                           method = "dot")
barplot(rbind(assignments[10,], assignments_dot[10,]),
        col = c("red", "blue"), las = 2, beside = TRUE)
legend("topright", legend = c("gibbs", "dot"), col = c("red", "blue"), 
       fill = c("red", "blue"))


#####Analysis
# All tweets gathered
glimpse(alltweets)
str(alltweets)
#Relationship between retweet and status
alltweets %>%
  filter(!(country %in% "") | is.na(country)) %>%
  filter(!(country_code %in% 
             "") | is.na(country_code)) %>%
  ggplot() +
  aes(x = statuses_count, y = retweet_count) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal()
# Relationship between friends and the followers of users twitting
alltweets %>%
  filter(!(country %in% "") | is.na(country)) %>%
  filter(!(country_code %in% 
             "") | is.na(country_code)) %>%
  ggplot() +
  aes(x = followers_count, y = friends_count) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal()


# showing frequency of words 

Volume <- alltweets %>%
  group_by(screen_name) %>%
  count(followers_count,retweet_count, sort = TRUE) %>%
  left_join(alltweets %>%
              group_by(screen_name) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)
# volume of followers to Number of retweets
my.scale_aes <- list(
  scale_y_continuous(breaks = extended_breaks()),
  scale_y_continuous(breaks = extended_breaks()),
  scale_color_manual(values = c("black", "red"))
)

ggplot(data = Volume[1:100,],
       aes(retweet_count, followers_count, cut == "Ideal" )) +
  geom_point() +
  geom_smooth(se = FALSE)+
  my.scale_aes +
  labs(
    title = "Volume of twwiter users tweeting about Covid-19 and their followers",
    subtitle = "Relation between Followers and retweets",
    caption = "Data from twitter.com complied by Emmanuel"
  )

ggplot(data = Volume[1:100,],
       aes(retweet_count, followers_count, cut == "Ideal" )) +
  geom_point() +
  geom_smooth(se = FALSE)+
  my.scale_aes +
  theme_bw()+
  labs(
    title = "Volume of twwiter users tweeting about Covid-19 and their followers",
    subtitle = "Relation between Followers and retweets",
    caption = "Data from twitter.com complied by Emmanuel"
      )
# saving my plot
ggsave("my-plot.pdf")
#
fig.show = "hold"
# Total number of retweets
sum(alltweets$is_retweet)
sum(alltweets$retweet_count)
ggplot(alltweets, aes(verified, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()
top_words <- twdf%>%
  group_by(text) %>%
  filter(row_number(desc(followers_count)) == 1)

plot(top_words)


plot(Volume$followers_count,Volume$retweet_count)


#############################################################################

esquisse::esquisser()


library(dplyr)
library(ggplot2)

alltweets %>%
 filter(!(country %in% "") | is.na(country)) %>%
 filter(!(country_code %in% 
    "") | is.na(country_code)) %>%
 ggplot() +
 aes(x = screen_name, y = followers_count) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 theme_minimal()

alltweets %>%
 filter(!(country %in% "") | is.na(country)) %>%
 filter(!(country_code %in% 
    "") | is.na(country_code)) %>%
 ggplot() +
 aes(x = screen_name, y = followers_count) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 theme_minimal()
ggplot(final_summary_words) +
  aes(x = topic, y = word, colour = topic) +
  geom_tile(size = 1L) +
  scale_color_hue() +
  theme_minimal() +
  facet_wrap(vars(topic))


ggplot(final_summary_words) +
 aes(x = topic, y = word, colour = topic) +
 geom_tile(size = 1L) +
 scale_color_hue() +
 theme_minimal() +
 facet_wrap(vars(topic))

ggplot(coherence_mat) +
  aes(x = k, y = coherence) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

ggplot(final_summary_words) +
  aes(x = word, y = topic) +
  geom_tile(size = 1L) +
  theme_minimal()


alltweets %>%
  filter(!(country %in% "") | is.na(country)) %>%
  filter(!(country_code %in% 
             "") | is.na(country_code)) %>%
  ggplot() +
  aes(x = followers_count, y = friends_count) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

esquisse::esquisser()
