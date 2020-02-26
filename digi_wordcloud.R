# input data
setwd("F:/TM_7W")
readLines("Digital_Marketing.txt")

#testing paste function
a<-c("Hello", "My", "name", "is","Nithin")
a
b=paste(a, collapse = " ")
b

library("utils")

# concatenating into a single element
digi<-readLines("Digital_Marketing.txt")
digi_past<-paste(digi, collapse = " ")
#head(digi_past)

###  CLEANING DATA  ###

# Let us now lower case this data

clean_digi1<-tolower(digi_past)
#head(clean_digi1)

# Cleaning the punctuations, pattern= "\\W"
# We are going to replace punctuations by space
# if we don't do so we may create new words

clean_digi2<-gsub(pattern="\\W", replace= " ", clean_digi1)
#head(clean_digi2)
clean_digi1<-tolower(clean_digi2)
#head(clean_digi1)

# Cleaning the digits, pattern="\\d"

clean_digi3<-gsub(pattern="\\d", replace= " ", clean_digi2)
#head(clean_digi3)

# Cleaning the stopwords

#install.packages("tm")
library("tm")

# Let us see a preview of stopwords

stopwords()

# Let us remove them using function removewords()

clean_digi4<-removeWords(clean_digi3, stopwords())
#head(clean_digi4)

# Let us remove single letters, here \\b[A-Z]
# represents that a string starts with any letter in A-Z
# and string can take upper case as well as lower case letters
# and the subsequent \\b{1} says that the string ends with length one

clean_digi5<-gsub(pattern="\\b[A-z]\\b{1}", replace= " ", clean_digi4)
#head(clean_digi5)

# We can finally remove whitespaces using stripWhitespace() function, 
# which is part of tm library

clean_digi6<-stripWhitespace(clean_digi5)
#head(clean_digi6)

###  FREQUENCY OF THE WORDS  ###
# We now have a chunk of lines, and we are looking for counting the words
# If you remember, we had joined various lines and made a chunk
# So, we split individual words and add a space between them as splitter

clean_digi7<-strsplit(clean_digi6, " ")
#head(clean_digi7)

word_freq1<-table(clean_digi7)
#head(word_freq1)

word_freq2<-cbind(names(word_freq1), as.integer(word_freq1))
#head(word_freq2)

write.csv(word_freq2, "Word Frequency2.csv")

###   Word Cloud   ###
# We will use cloud library to come up with word cloud

#install.packages("RColorBrewer")
#install.packages("wordcloud")
library("RColorBrewer")
library("wordcloud")

# After installing the packages, we want to organize our words as per wordcloud()

class(clean_digi7)

# Class of data is stored as words
# We want the class to be as characters
# One way to do it is to unlist the list

word_cloud1<-unlist(clean_digi7)
wordcloud(word_cloud1, min.freq = 3, color='red')
wordcloud(word_cloud1, min.freq = 3, random.order = FALSE, color='red')
wordcloud(word_cloud1, min.freq = 3, random.order = TRUE, color='red')

# We want to add colors to words
# We chose rainbow function to add multiple colors
# Number of colors in parentheses

pal2<-brewer.pal(10,"Dark2")
wordcloud(word_cloud1, min.freq = 3, scale = c(4,1), colors = pal2)
wordcloud(word_cloud1, min.freq = 3, scale = c(4,1), figpath= "1.jpg", colors = pal2)
wordcloud(word_cloud1, min.freq = 3, scale = c(4,1), figpath= "2.jpg", colors = pal2)
wordcloud(word_cloud1, min.freq = 3, scale = c(4,1), figpath= "Mercedes-logo.jpg", colors = pal2)
??brewer.pal

#install.packages("wordcloud2")
library("wordcloud2")

# wordcloud2 command does not take list as argument, need to create a frequency table
mydata <- table(word_cloud1)
mydata <- as.data.frame(mydata)

# for limiting words above certain frequency and ordering 
# not used for the current wordcloud
r <- with(mydata, which(mydata$Freq >= 3, arr.ind = TRUE))
mydata <- mydata[r, ]
mydata <- mydata[order(-mydata$Freq),]

# just looking for some basic info about data
head(mydata, 4)
str(mydata)

# wordcloud2 provides results in Viewer instead of plot
# uses functionalities of browser, works best with Internet Explorer (IE)
# change system's default browser to Internet explorer(IE)
# maybe the library is using html code compatible with IE
# For Windows 10, search default app settings, go to Web browser, change default to IE
# If wordcloud not visible in viewer, click on 'Show in New window' option
# works best with black vector silhouette image with white background
wordcloud2(mydata, color = pal2, size = 1, minRotation = -pi/2,
           maxRotation = pi/2, rotateRatio = 0.5, 
           backgroundColor = "wheat", figPath = "guitar8.png")
wordcloud2(mydata, color = pal2, figPath = "guitar8.png")

###   SENTIMENTAL ANALYSIS   ###
# Getting the bags of positive and negative words

positive<-scan("positive.txt", what = "character", comment.char = ";")
negative<-scan("negative.txt", what = "character", comment.char = ";")

# Now we have our positive and negative words
# We will use them to match words in our text using match()

senti_analysis<-unlist(clean_digi7)
match(senti_analysis, positive)
match(senti_analysis, negative)

p_score<-sum(!is.na(match(senti_analysis, positive)))
p_score

n_score<-sum(!is.na(match(senti_analysis, negative)))
n_score

sentiment_score=p_score-n_score
sentiment_score
# As sentiment_score is 20, a positive value, it can be concluded that the article is of positive sentiment.
