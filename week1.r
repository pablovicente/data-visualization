# Set working directory


############################
### 	Just once		 ###
############################

# Install streamR from CRAN
install.packages("streamR")
install.packages('twitteR')

# Install streamR from github
install.packages("devtools")
library(devtools)
install_github("pablobarbera/streamR", subdir = "streamR")

# Install ROAuth
install.packages('ROAuth')

# Setting keys
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

# To get new keys go to https://apps.twitter.com/app/new
api_key <- ''
api_secret <- ''
access_token <- ''
access_token_secret <- ''



my_oauth <- OAuthFactory$new(consumerKey = api_key, consumerSecret = api_secret, 
    requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")


############################
### 	Real fun		 ###
############################

library(ROAuth)
library(streamR)
library(twitteR)

load("my_oauth")
filterStream("tweets.json", track = c("Obama", "Biden"), timeout = 10, oauth = my_oauth)
tweets.df <- parseTweets("tweets.json", simplify = TRUE)


############################
### 	  GGPLOT		 ###
############################

install.packages("ggplot2")
install.packages("ggrepel")
install.packages("ggmap")
install.packages("ggtheme")
install.packages("dplyr")
install.packages("reshape")
install.packages("data.table")

library(ggplot2)
library(ggrepel)
library(ggmap)
library(ggtheme)
library(dplyr)
library(reshape)
library(data.table)


# Loading housing data
housing <- read.csv("dataSets/landdata-states.csv")
housing$Year <- as.numeric(substr(housing$Date, 1, 4))
housing$Qrtr <- as.numeric(substr(housing$Date, 5, 5))
housing$Date <- housing$Year + housing$Qrtr/4

# Histogram	
ggplot(housing, aes(x = Home.Value)) +
    geom_histogram(bins=20)


# Colored scatter plot
ggplot(subset(housing, State %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()    

# Colored line plot
ggplot(subset(housing, State %in% c("MA", "TX")),
        aes(x=Date,
            y=Home.Value,
            color=State, 
            ))+
     geom_line()

# Colored scatter plot
ggplot(subset(housing, Date == 2001.25),
	aes(y = Structure.Cost, x = Land.Value, color='red')) +
	geom_point(show.legend = FALSE)

# Lines (Prediction Line)
hp2001Q1 <- subset(housing, Date == 2001.25)

hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))

p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))

p1 + geom_point(aes(color = Home.Value)) +
  geom_line(aes(y = pred.SC))

# Text (Label Points)
p1 + geom_text(aes(label=State), size = 3) 

p1 + 
  geom_point() + 
  geom_text_repel(aes(label=State), size = 3)



############################
### 	  GAPMINDER		 ###
############################  

install.packages("gapminder")
library(gapminder)

# Plotting scatter plot
p <- ggplot(data=gapminder, aes(x=gdpPercap, y=lifeExp))
p + geom_point()

# Scaling x axis
p + geom_point() + scale_x_log10()

# Setting color by continent 
p <- ggplot(data=gapminder, aes(x=gdpPercap, y=lifeExp)) + scale_x_log10()
p + geom_point(aes(color=continent), alpha=0.3, size=2)

# Variants form previous plot
p + geom_point(aes(color=continent, alpha=pop), size=2)

	
p + geom_point(aes(color=continent)) + geom_smooth(color='black', lwdz=2, std=FLASE)	
p + geom_point(aes(color=continent, size=pop), alpha=0.3)

# Dividing each plot after aes by continent
p + geom_point(aes(color=continent)) + geom_smooth(color='black', lwd=2, se=FALSE)
p + aes(color=continent) + geom_point() + geom_smooth(lwd=2, se=FALSE)

# Creating a grid for each continent


p + geom_point(aes(color=continent, alpha=pop), size=2) + facet_grid(. ~continent)
p + geom_point(aes(color=continent, alpha=pop), size=2) + facet_grid(. ~continent) + geom_smooth(color='black', lwd=1, se=FALSE)
