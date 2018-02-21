library(twitteR)
library(ROAuth)
library(base64enc)
library(purrr)
library(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(tm)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence <- data.frame(text = iconv(sentence, "latin1", "ASCII", "byte"), stringsAsFactors = FALSE)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    sentence <- gsub("(?:#|@)[a-zA-Z0-9_]+ ?", "", sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

consumer_key <- "XWJBnpwkNEWthV326ikjl7RsB" #To be used in the setup.
consumer_secret <- "GZCUgCgRI78chwRZ0n9HvEMlOE5dRm1cIG5EZeBTyVIw9QvsN1"
access_token <- "757620681412530176-CMKavEaVf6giLGMIBwN4o2u5hgA0hzh"
access_secret <- "VB7xhq7L5yNq8UDR1Wn64Y1no12zF0xWTjDWqY5wY0Lym"

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

cred <- OAuthFactory$new(consumerKey='XWJBnpwkNEWthV326ikjl7RsB',
                         consumerSecret='GZCUgCgRI78chwRZ0n9HvEMlOE5dRm1cIG5EZeBTyVIw9QvsN1',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize',)

cred$handshake(cainfo="cacert.pem")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

pos.words <- scan("positive-words.txt", what = "character", comment.char = ";")
neg.words <- scan("negative-words.txt", what = "character", comment.char = ";")

Tweets <- searchTwitteR("@MKBHD", n = 100)
length(Tweets)
Tweets.txt <- laply(Tweets, function(t)t$getText())
Tweets.txt
Tweets_text <- twListToDF(Tweets)
result <- score.sentiment(Tweetx_txt$text, pos.words, neg.words)
tweet <- Tweets[[1]]
class(tweet)
tweet$getText()
head(Tweets.txt)
sample <- c("An error has occurred", "Special thanks to John Verostek for putting together such an interesting event, and for providing valuable feedback and help with these slides", "Wow I must confess you make some very trhecnant points.", "love this movie", "hate this song", "He is quite adaptive")
table(result)
as.list(result$score)
result$score
sum(result$score)
library(ggplot2)
ggplot(data = result, aes(x = result$score)) + geom_histogram(binwidth = 1, col = "red", fill = "lightgreen")






