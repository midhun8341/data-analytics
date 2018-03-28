tweet <- searchTwitter("#Republicday" , n = 2000)
Tweetx_txt <- twListToDF(tweet)
mydata.corpus <- c(Tweetx_txt$text,Tweetx_txt$text)
require(tm)
mydata.corpus <- Corpus(VectorSource(mydata.corpus))
mydata.corpus <- Corpus(VectorSource(mydata.corpus))
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
mydata.dtm <- TermDocumentMatrix(mydata.corpus)
findFreqTerms(mydata.dtm, lowfreq=30)

findAssocs(mydata.dtm, 'tesla', 0.20) 
mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.95)
mydata.df <- as.data.frame(inspect(mydata.dtm2))
nrow(mydata.df)
ncol(mydata.df)

mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram?

groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")
class(mydata.df)
x <- as.data.frame(mydata.dtm)
x <- as.character(mydata.df)
