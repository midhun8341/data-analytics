emojis_b <- read.csv("emDict.csv", sep =";")
emojis_s <- read.csv("emoji_dictionary.csv", sep =",")
names(emojis_s)

emojis_s$R.encoding <- emojis_s$R_Encoding
emojis_s <- emojis_s[ ,-2]
emojis_use <- merge(emojis_b, emojis_s, by = "R.encoding" )

names(emojis_use)

emoticons <- emojis_use[, -2]

emoticons <- arrange(emoticons, emoticons$Number)

tweet <- searchTwitter("#Bitcoin" , n = 3000)
Tweetx_txt <- twListToDF(tweet)

Tweetx_txt <- data.frame(text = iconv(Tweetx_txt$text, "latin1", "ASCII", "byte"), stringsAsFactors = FALSE)

library(DataCombine)

tweets <- FindReplace(data = Tweetx_txt, Var = "text", 
                      replaceData = emoticons,
                      from = "R.encoding", to = "Name", 
                      exact = FALSE)
emogrepl <- grepl(paste(emoticons$Name, collapse = "|"), tweets$text)

emogreplDF<-as.data.frame(emogrepl)

tweets$ID7 <- 1:nrow(tweets)
emogreplDF$ID7 <- 1:nrow(emogreplDF)
tweets <- merge(tweets,emogreplDF,by="ID7")
emosub <- tweets[tweets$emogrepl == "TRUE", ]


word.list = str_split(emosub$text, '\\s+')
words <- unlist(word.list)

emoticons$Name <- as.character(emoticons$Name)
emoticons$Name = gsub(' ', '', emoticons$Name)

result.f <- match(words, as.character(emoticons$Name))

library(ggplot2)
x <- as.data.frame(result.f)
x <- na.omit(x)
table((x))
x$Number <- x$result.f
table(output$Number)
output <- merge(emoticons, x, by = "Number")

z <- as.data.frame(table(output$Name))

z$Name <- z$Var1
final <- merge(z, unique(output), by = "Name")

Em_frq <- final %>% select(Name,Freq, Native)

ggplot(data = Em_frq, aes(y = Em_frq$Freq, x = Em_frq$Name, col = factor(Em_frq$Name))) + geom_col()
