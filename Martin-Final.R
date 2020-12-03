#Martin-Final
#Final Project
#Ethan Martin

#Before I got started I had to register as a Twitter Ap developer to get the Consumer API keys I needed to access the data
#Set WD
#Load Packages------------
require(twitteR)
require(RCurl)
require(wordcloud)
require(tm)
require(tidyverse)
require(tidytext)
require(tibble)
require(topicmodels)
#Storing the Keys------------
api_key <- "zmI9oGmAkUH9fVehqMg8Vwlso"
api_secret <- "AjVI8GE7n5qZC1wOrelYqgNG7bAVgyOnPTeHIe9kfvufRLKSr6" 
access_token <- "1155137965561135105-o5L4ACZiXUZ9xfpr34HHpsrFPd2Vsk"
access_token_secret <- "T39PIAw0WraB6UXC536axLdCmsSY3WjBvv5orcCOCCcUF" 
#Accessing the data
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
1


#Time to download some twitter data--------------
dunedin_tweets <- searchTwitter("dunedin", n = 100, lang = "en")
dunedin_tweets_text <- sapply(dunedin_tweets, function(x) x$getText())
head(dunedin_tweets_text, 10)
#create corpus
dunedin_tweets_text_corpus <- Corpus(VectorSource(dunedin_tweets_text))
#clean up
dunedin_tweets_text_corpus <- tm_map(dunedin_tweets_text_corpus, content_transformer(tolower)) 
dunedin_tweets_text_corpus <- tm_map(dunedin_tweets_text_corpus, removePunctuation)
dunedin_tweets_text_corpus <- tm_map(dunedin_tweets_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(dunedin_tweets_text_corpus)


#I dont think this is the data I'm trying to aquire exactly to many tweets from scotland and new zealand (other Dunedins)
dunedinFL_tweets <- searchTwitter("dunedinFL", n = 100, lang = "en")
dunedinFL_tweets_text <- sapply(dunedinFL_tweets, function(x) x$getText())
head(dunedinFL_tweets_text, 10)
#This is mostly weather tweets, some of this is exactly what i"m looking for, but not enough of it. Also just not enough overall.
#I am not satisfied with this data set.  Think I am going to try a broader category.
beer_tweets <- searchTwitter("craftbeer", n=1000, lang = "en")
beer_tweets_text <- sapply(beer_tweets, function(x) x$getText())
head(beer_tweets_text, 10)
#I would like to narrow this down more to florida craft beers, which I have a very good knowledge of that will hopefully
#help me interpret the data.  skimming through the tweets I noticed this---"Enjoying a @jdubsbrewing Poolside by the ... poolside.
#\n\nI'm such a hack. \n\n#florida #craftbeer #drinkfloridacraft https://t.co/tH5HwqAmcO"  I'm very familiar with this brewery/beer
#and they used the hashtag #drinkfloridacraft.  I am going to try this hashtag and see how that goes.
FLbeer_tweets <- searchTwitter("drinkfloridacraft", n=1000, lang = "en")
FLbeer_tweets_text <- sapply(FLbeer_tweets, function(x) x$getText())
head(FLbeer_tweets_text, 10)
#This only returned 11 tweets, not a popular hashtag. I'm going to go on the internet and look up popular florida craft brewing hashtags and
#see if that helps.
#Ok.  Optimizing hashtags is apparently a much bigger deal than I realized.  Not that it doesn't make sense, I just never thought about it.
#I think there is no single hashtag that is going to do the trick.  I am going to see if I can figure out how to search for multiple
#hash tags at once. If possible that should give me the data I want.....I think.....
hashtags <- 'craftbeer + florida'
FLbeer_tweets <- searchTwitter(hashtags, n = 1000, lang = 'en') 
#I only got 41, but thanks to the magic of copy and paste im going to do a quick visualization to see how it looks.
FLbeer_tweets_text <- sapply(FLbeer_tweets, function(x) x$getText())
#create corpus
FLbeer_tweets_text_corpus <- Corpus(VectorSource(FLbeer_tweets_text))
#clean up
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, content_transformer(tolower)) 
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, removePunctuation)
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(FLbeer_tweets_text_corpus, max.words = 100)
#I learned two things. I don't think this is a big enough sample. Also, emoji's show up on wordclouds. In this case
#it was the two beer mugs clinking together.  So that was cool...back to the data mines we go.
#I may have to give up on making these florida specific for now. going to add drinklocal hashtag to craft beer. It's my hope
#this will eliminate the craft beers made by the conglomerates and will get more small brewery information.
hashtags <- 'craftbeer + drinklocal'
FLbeer_tweets <- searchTwitter(hashtags, n = 1000, lang = 'en') 
#strip retweets out of the data
FLbeer_tweets <-strip_retweets(FLbeer_tweets)

#Copy/paste...
FLbeer_tweets_text <- sapply(FLbeer_tweets, function(x) x$getText())
#create corpus
FLbeer_tweets_text_corpus <- Corpus(VectorSource(FLbeer_tweets_text))
#clean up
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, content_transformer(tolower))
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, removePunctuation)
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, removeNumbers)
FLbeer_tweets_text_corpus <- tm_map(FLbeer_tweets_text_corpus, removeWords, c(stopwords("english"), "craftbeer"))
wordcloud(FLbeer_tweets_text_corpus, max.words = 100)
#A lot of the words wouldn't fit at first so I added a max.words=100 to it. I think we are headed in the right direction. its a nice large set of the sort of data I was looking for.
#I think I like the data set, but I am going to switch to the tidy method we learned in datacamp for the midterm to further explore
#Convert to a format tidytext can use--------------------
FLbeerTweets <- twListToDF(FLbeer_tweets)

tidy_beer <- FLbeerTweets %>% 
  # Tokenize the twitter data
  unnest_tokens(word, text) %>%
  # Remove stop words
  anti_join(stop_words)

tidy_beer %>% 
  # Compute word counts
  count(word) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))
#Looks like I need to add some custom stop words
custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "https", "CUSTOM",
  "t.co", "CUSTOM", 
  "amp" , "CUSTOM"
)

# Bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)
#lets try this again
tidy_beer <- FLbeerTweets %>% 
  # Tokenize the twitter data
  unnest_tokens(word, text) %>%
  # Remove stop words
  anti_join(stop_words2)

tidy_beer %>% 
  # Compute word counts
  count(word) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))
#Everything seems to be working, lets try to visualize---------------------
word_counts <- tidy_beer %>% 
  count(word) %>% 
  filter(n > 100)

# Create a bar plot using the new word_counts
ggplot(word_counts, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Word Count for Craft Beer Twitter Data")
# I need to set a lower bar, I'm only returning 3 words
word_counts <- tidy_beer %>% 
  count(word) %>% 
  filter(n > 50)
ggplot(word_counts, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Word Count for Craft Beer Twitter Data")

#There are now 5 terms, lowering the bar....
word_counts <- tidy_beer %>% 
  count(word) %>% 
  filter(n > 25)
ggplot(word_counts, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Word Count for Craft Beer Twitter Data")
#That looks better, up to 16 words, but it needs to be put in order and a few more words would be good
word_counts <- tidy_beer %>% 
  count(word) %>% 
  filter(n > 15) %>%
  mutate(word2 = fct_reorder(word, n))
ggplot(word_counts, aes(x = word2, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Word Count for Craft Beer Twitter Data")
#There we go, that looks better
wordcloud(
  # Assign the word column to words
  words = word_counts$word, 
  # Assign the count column to freq
  freq = word_counts$n,
  max.words = 50
)
#Some how some numbers and jul snuck in there. Also going to get rid of the two hashtags I searched for in the first place.
custom_stop_words2 <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "https", "CUSTOM",
  "t.co", "CUSTOM", 
  "amp", "CUSTOM",
  "craftbeer", "CUSTOM",
  "drinklocal", "CUSTOM",
  "2", "CUSTOM",
  "3", "CUSTOM",
  "1", "CUSTOM",
  "4", "CUSTOM",
  "5", "CUSTOM",
  "6", "CUSTOM",
  "7", "CUSTOM",
  "8", "CUSTOM",
  "9", "CUSTOM",
  "10", "CUSTOM",
  "it's", "CUSTOM",
  "jul", "CUSTOM"
)
stop_words3 <- stop_words %>% 
  bind_rows(custom_stop_words2)
#lets try this once again
tidy_beer <- FLbeerTweets %>% 
  # Tokenize the twitter data
  unnest_tokens(word, text) %>%
  # Remove stop words
  anti_join(stop_words3)

word_counts <- tidy_beer %>% 
  count(word) %>% 
  filter(n > 15) %>%
  mutate(word2 = fct_reorder(word, n))
ggplot(word_counts, aes(x = word2, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Word Count for Craft Beer Twitter Data")
#There we go, that looks better
wordcloud(
  # Assign the word column to words
  words = word_counts$word, 
  # Assign the count column to freq
  freq = word_counts$n,
  max.words = 40,
  color = "blue")
#I see ipa.. That makes sense as it is probably the most popular type of craft beer.  There are a few hashtags in there it looks like as well.
#Things like "beerporn" (probably pictures of beer looking delicious). Also "instabeer" and "beerstagram" are instagram specific hashtags.
#There are a lot of date/time type words.  This is probably due to tweets related to beer festivals and releases. In the wordmap shown in fact,
#the words festival, tonight, starts, and time all appeared. 
#The words tap, taproom, and brewery suggest that the best place to get great craft beer is still on tap at your local brewery.

#Lets explore some sentiments-----------------------------
# Join tidy_beer and the bing sentiment dictionary
bing_sentiment_beer <- tidy_beer %>% 
  inner_join(get_sentiments("bing"))

#I would like to see more rows so I changing the tibble options
options(tibble.print_max = 30, tibble.print_min = 25)


bing_word_counts <- bing_sentiment_beer %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

#As expected it was pretty positive.  Also upon closer inspection even the negative words arent so negative in beer culture. Sour is not a bad thing,
#it is in fact a popular style of beer suited well to summertime. The next negative word is cold, which is obviously a good thing for beer to be
#in the summer. Pale is the next negative word and I'm sure that is just the beer style "pale ale" showing up and not likely a comlplexion issue. 
#Hazy is not good if its weather, but if its beer then it probably refers to hazy new england style IPA which is a popular new beer trend.
#Lets look closer
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
       title = "Sentiment Analysis",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
#I'd say at least 7 out of the top ten negative words are actually positive. This data set is pretty overwhelmingly positive, as I suspected.
#People like to brag about their craft beer, and even if they do have something they dislike they generally only post about the good ones.
#I personally believe this is because people want to be seen "living their best lives" on social media. Also, craft beer is delicious.

#Lets try LDA modeling, first we need the data to be a DTM------------------------------------

# Assign the DTM to dtm_beer
dtm_beer <- tidy_beer %>% 
  count(word, id) %>% 
  # Cast the word counts by tweet into a DTM
  cast_dtm(id, word, n)

# Coerce dtm_beer into a matrix called matrix_beer (do this if you want to look at specific parts of the data)
#matrix_beer <- as.matrix(dtm_beer)

#Create a topic model
lda_out <- LDA(
  dtm_beer,
  k = 2,
  method = "Gibbs",
  control = list(seed = 22)
)
#Take a look at the topic model
glimpse(lda_out)

# Tidy the matrix of word probabilities
lda_topics <- lda_out %>% 
  tidy(matrix="beta")

# Arrange the topics by word probabilities in descending order
lda_topics %>% 
  arrange(desc(beta))

# Select the top 25 terms by topic and reorder term
word_probs <- lda_topics %>% 
  group_by(topic) %>% 
  top_n(25, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word_probs2, color and facet based on topic
ggplot(
  word_probs, 
  aes(term2, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Modeling 2 Topics",
       y = "Probability",
       x ="Terms")
#The second topic seems to be beer related words and the first appears to be when and where to get beer.
#We need more topics
#Create a model with more topics------------------------
lda_out2 <- LDA(
  dtm_beer,
  k = 3,
  method = "Gibbs",
  control = list(seed = 22)
)
# Tidy the matrix of word probabilities again
lda_topics2 <- lda_out2 %>% 
  tidy(matrix="beta")

# Arrange the topics by word probabilities in descending order again
lda_topics2 %>% 
  arrange(desc(beta))

# Select the top 25 terms by topic and reorder term
word_probs2 <- lda_topics2 %>% 
  group_by(topic) %>% 
  top_n(25, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word_probs2, color and facet based on topic
ggplot(
  word_probs2, 
  aes(term2, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Modeling 3 Topics",
       y = "Probability",
       x ="Terms")
#The second topic seems to be when people are taking pictures of beer, the first topic is what people are doing while they drink beer,
#and the third topic is summer beer words maybe?
#Lets try 4 topics now-------------
lda_out3 <- LDA(
  dtm_beer,
  k = 4,
  method = "Gibbs",
  control = list(seed = 22)
)
# Tidy the matrix of word probabilities again
lda_topics3 <- lda_out3 %>% 
  tidy(matrix="beta")

# Arrange the topics by word probabilities in descending order again
lda_topics3 %>% 
  arrange(desc(beta))

# Select the top 25 terms by topic and reorder term
word_probs3 <- lda_topics3 %>% 
  group_by(topic) %>% 
  top_n(25, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word_probs3, color and facet based on topic
bestLDAplot <- ggplot(
  word_probs3, 
  aes(term2, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Modeling 4 Topics",
       y = "Probability",
       x ="Terms")
bestLDAplot
#The first topic appears to be related to ticketed events where people are drinking. The second topic appears to be related to advertising new beers
#live music and trivia.  these are more common types of beer news as opposed to the special events in the first topic.  The third topic seems to
#relate to posting pictures of beer.  The fourth topic seems to be how much fun people are having drinking beer.
#Let's look at 5 topics now.------------------
lda_out4 <- LDA(
  dtm_beer,
  k = 5,
  method = "Gibbs",
  control = list(seed = 22)
)
# Tidy the matrix of word probabilities again
lda_topics4 <- lda_out4 %>% 
  tidy(matrix="beta")

# Arrange the topics by word probabilities in descending order again
lda_topics4 %>% 
  arrange(desc(beta))

# Select the top 25 terms by topic and reorder term
word_probs4 <- lda_topics4 %>% 
  group_by(topic) %>% 
  top_n(25, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))
# Plot word_probs3, color and facet based on topic
ggplot(
  word_probs4, 
  aes(term2, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Modeling 5 Topics",
       y = "Probability",
       x ="Terms")
#I think the topics got a little muddled here.  1 and 4 are similar and the topics aren't as distinct. 
#The best model was with 4 topics.------------------------
bestLDAplot
#I would like to name the facets.  After some research this should do the trick.
#Make a list of the names I want the plots to have tied to the current names of the plots.
plot_names <- list(
                   "1" = "Ticketed Events",
                   "2" = "Advertising Beer News",
                   "3" = "Beer Pics", 
                   "4" = "Beer Fun")

#Create a labeller function
beer_labeller <- function(variable,value){
  return(plot_names[value])
}
#Pass that function into the facet_wrap
bestLDAplot <- ggplot(
  word_probs3, 
  aes(term2, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free", labeller=beer_labeller) +
  coord_flip() +
  labs(title = "Topic Modeling 4 Topics",
       y = "Probability",
       x ="Terms")
bestLDAplot
#There it is! Success!
#Now that I have the topics labeled, lets see if any more information can be gleaned from them.

#Topic 1, Ticketed Events-----------------

#Words like tonight, food, starts, tickets, week, special, and fun lead me to believe this column features special events that are less frequent
#and more "special." To judge by the 4th word, love, people tend to enjoy these types of things.

#I would say that festivals, are a great way to get people to your brewery/bar.  Also when you get them there provide food. This is supported
#by words like food and eatlocal.  The typical way to accomplish this would be food trucks.  So have a festival and get some foodtrucks there.
#This will also help keep people from getting too blasted at your event in my experience. These are designed to bring in huge crowds on already
#good days.

#Topic 2, Advertising Beer News-----------------

# Words like beer, news, brewed, fresh, citrus indicate this topic is about new beer releases.  The presence of trivia, live, and rock also
#support that this is a topic related to not ticketed special events. the presence of Wednesday supports that these are more low key weekly
#events.


#It is always important to announce your new beer releases.  This is a great chance to get your regulars to come in on a day they might not 
#normally to try a new beer.  Also you might gain a new customer if it is a particular flavour or style they enjoy.  Also trivia and live
#music are a great way to have that same effect.  These types of things should be planned more durring the week to lure in customers on slower
#days, unlike topic 1.

#Topic 3 Beer Pics-----------------------------------------------

# Hashtags donminate this list.  beerporn, like its close relative foodporn, are hashtags that often accompany the sharing of artfully staged
#photos of beer/food. Beerstagram, drinkcraft. beergeek, and craftbeerguy also make an appearence. People often take a picture of their beer
#and then post it with a string of hashtags. 

#I would say that propper hashtag useage will help spread your social media efforts wider.  I might encourage a bar/brewery owner to consider
#posting popular beer-related hashtags somewhere to help increase their social media presence.  Maybe even a special little table somewhere 
#with a backdrop featuring your logo, maybe some props (frames, some hops, funny glasses, that sort of thing) as a sort of social media 
#photo-booth/beerporn presentation station.

#Topic 4, Beer Fun----------------------------------------

#This topic seems to be about how much fun people are having drinking beer! I also have fun drinking beer so I can see why this is a topic.
#Words like delicious, cold, enjoy, beautiful, and tasty seem to support this.  Also, when you are having fun you often use social media as a 
#way to let all your friends know how much fun you are having and to invite them to join you for a beer, maybe even a chicken taco.

#Again, social media is a great way to draw in customers. If people are having fun they want to show everyone that they are
#"living their best lives." As was mentioned for the previous topic, Encourage social media use any way that you can.  Have someone in charge of
#posting and responding to others.  If you don't have someone talented running your social media you should consider hiring someone to do that
#specifically, or more likely, look for that quality in people you hire for other things and tack that responsibility onto their others.

#Thank you for a great class!-------------------------------------------------