# Set working directory
setwd('/Users/pablo/Desktop/Pablo/MS_in_Computer_Science/Data\ Visualization/data-visualization')

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
install.packages('rmarkdown')
install.packages('plotly')

library(ggplot2)
library(ggrepel)
library(ggmap)
library(ggtheme)
library(dplyr)
library(reshape)
library(data.table)
library(rmarkdown)
library(plotly)


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
### 	   GAPMINDER	  	 ###
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

############################
###      BABYNAMES       ###
############################

# Loading required libraries
library(babynames)
library(ggplot2)    # the king of plotting 
library(magrittr)   # chain operators, e.g. to "pipe" a value forward
library(dplyr)      # for data manipulation 



# Summary of information
str(babynames)
summary(babynames)

# Displaying first rows
head(babynames)


# Counting different names
length(unique(babynames$name))

# How many kids in database
sum(babynames$n)/10^6

# Plotting for both sexs
ggplot(babynames, aes(year, n)) +
  geom_line(data = filter(babynames, name=="James"))

# We can see that with the following command
head(filter(babynames, name=="James"))

# Filtering by name but also splitting by sex
ggplot(babynames, aes(year, n)) +
  geom_line(data = filter(babynames, name=="James"), aes(color=sex))


# Try to follow this code chunk at home, using dplyr() and magrittr()
top10 <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  group_by(sex) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10)  %>%
  arrange(sex, rank)

top10f <- top10 %>% filter(sex=="F")
top10m <- top10 %>% filter(sex=="M")  

# Plotting most 10 common names 
babynames %>%
  filter(sex=="F") %>%
  filter(name %in% top10f$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))

babynames %>%
  filter(sex=="M") %>%
  filter(name %in% top10m$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))


#Plot the most common names in 2013 over the entire period.
top10_2013 <- babynames %>%
  filter(year==2013) %>%
  group_by(sex, name) %>%  
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  group_by(sex) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10)  %>%
  arrange(sex, rank)

top10f_2013 <- top10_2013 %>% filter(sex=="F")
top10m_2013 <- top10_2013 %>% filter(sex=="M") 
top10u_2013 <- Map(c, top10_2013 %>% filter(sex=="M") %>% head(., n=5), top10_2013 %>% filter(sex=="F") %>% head(., n=5))


babynames %>%
  filter(sex=="F") %>%
  filter(name %in% top10f_2013$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))

babynames %>%
  filter(sex=="M") %>%
  filter(name %in% top10m_2013$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))


#Explore which names are most often used as unisex names. 
top10_unisex <- babynames %>%
  group_by(sex,name) %>%
  summarize(total = sum(n)) %>%
  group_by(name) %>%
  filter(n() > 1) %>%
  group_by(sex)

f <- top10_unisex %>% filter(sex=="F") %>% arrange(desc(name))
m <- top10_unisex %>% filter(sex=="M") %>% arrange(desc(name))
m['diff'] <- abs(m$total - f$total)
names_diff <- m[order(-m$diff), ]

head(names_diff)

tail(names_diff)

#For which names has the popularity over time changed a lot?  
CV <- function(mean, sd){
      (sd/mean)*100
      }

baby_stats <- babynames %>% 
  group_by(sex) %>% 
  as.data.frame() %>% 
  aggregate(n~name, ., mean)

baby_stats <- plyr::rename(baby_stats, c("n"="mean"))

baby_stats['sd'] <- babynames %>% 
  group_by(sex) %>% 
  as.data.frame() %>% 
  aggregate(n~name, ., sd) %>%
  .$n

baby_stats['cv'] <- CV(baby_stats$mean, baby_stats$sd)
baby_stats <- baby_stats %>% arrange(desc(cv))

top10_var <- head(baby_stats, n = 10)$name

top10_records <- babynames %>%
  filter(name %in% top10_var) %>% 
  group_by(name,year) %>%
  summarize(total = sum(n))


ggplot(data = top10_records, aes(year, total)) +
  geom_line( aes(color=name, group=name))







