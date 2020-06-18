# Which is the best streaming service for me?
#This is something that is becoming a harder question to answer with each passing day as more and more content is being either acquired or produced by a plethora of streaming services. 

#This report aims to answer this question by using visualizations and drive insights out of the data. 

#![We will consider only these Streaming Services in this notebook](https://media.comicbook.com/2020/01/netflix-disney-plus-amazon-hulu-1205084-1280x0.jpeg)


library(dplyr)

library(ggplot2)

library(tidyr)

library(plotly)

library(purrr)

library(treemapify)

library(reshape2)

library(gganimate)

library(ggridges)


#Reading in the data
setwd("C:\\Users\\aishwarya.sharma\\OneDrive - insidemedia.net\\Dell Lattitude - 5300\\Aishwarya\\Kaggle\\TV Show")

df <- read.csv("tv_shows.csv", stringsAsFactors = F)

head(df)


#The data is in a wide format and consists of a couple of unnecessary columns. We'll remove the unncessary columns and change the data format from wide to long

#Removing unwanted columns

df <-  df[,c(-1,-11)]

#Changing the data from wide to longer format
df <- gather(df, Service, Value, Netflix:Disney.)%>%
  filter(Value == 1)%>%
  select(-Value)

head(df)

#Converting % 
df$Rotten.Tomatoes <- as.numeric(sub("%","",df$Rotten.Tomatoes))/100


### **Which Service has the highest number of TV Show titles?**


ggplot(df)+
  geom_bar(aes(Service, fill = Service))+
  ggtitle("TV SHOWS BY STREAMING SERVICE",subtitle = "Prime Video has the lead in terms of sheer content")+
  xlab("Streaming Service")+
  ylab("No of TV Shows")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


#We'll try to determine which subscription service offers the best content and use IMDb ratings for our visualization

### **Building a violn chart to see distribution of IMDb ratings**


df$Service <- as.factor(df$Service)


ggplot(df, aes(x = factor(Service),y = IMDb))+
  geom_violin(aes(fill = factor(Service)), draw_quantiles = c(0.25,0.50,0.75))+
  ggtitle("IMDB RATING DISTRIBUTION BY STREAMING SERVICE",subtitle = "It seems Disney is a laggard in terms of content quality")+
  xlab("Streaming Service")+
  ylab("IMDb Rating of Titles")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


### **Which Streaming Service has the best content quality?**

#The dataframe has been narrowed down to the TOP 250 TV Shows by using rating approvals from both Rotten Tomatoes and IMDb. Only TV Shows with both, IMDb rating of greater than 8 & a Rotten Tomatoes rating  of more than 80% is considered and sorted in the list.


df$Value <- 1

Top250 <- df %>%
  filter(IMDb >= 8 & Rotten.Tomatoes > 0.8)%>%
  arrange(-IMDb)%>%
  top_n(250, IMDb)%>%
  select(Service,Value)%>%
  group_by(Service)%>%
  summarise_each(funs(sum))

head(Top250,10)


#To further analyze content quality we'll see a distribution of the TOP 250 TV Shows amongst various streaming services using a treemap.

ggplot(Top250, aes(area = Value, fill = Service, label = Service)) +
  geom_treemap() + 
  geom_treemap_text(fontface = "italic", color = "white", place = "centre")

##### It seemed like Amazon Prime & Netflix had an edge over Hulu but if the Top 250 TV shows are considered Amazon Prime falls behind Hulu with Netflix being the leader

### **Content by Different Age Groups **

#TV Shows are segregated by age-groups and here we'll build a bar chart with a drop down to see the count of TV Shows for different Age Groups

##Building the data in a certain format

temp <- dcast(df, Service ~ Age, fill = 0)%>%
  select(1,3:7)


##Plotting the bar chart


p <- plot_ly(data = temp,
             x = ~Service,
             y = ~all,
             type = 'bar'
) %>%
  layout(
    title = "TV Shows for Different Age Groups",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "TV Show Count"),
    updatemenus = list(
      list(
        y = 1,
        x = -0.1,
        buttons = list(
          list(method = "restyle",
               args = list("y", list(temp$`13+`)),  # put it in a list
               label = "Teenagers"),
          list(method = "restyle",
               args = list("y", list(temp$`16+`)),  # put it in a list
               label = "Mature Teenagers"),
          list(method = "restyle",
               args = list("y", list(temp$`18+`)),  # put it in a list
               label = "Adult"),
          list(method = "restyle",
               args = list("y", list(temp$`7+`)),  # put it in a list
               label = "Kids"),
          list(method = "restyle",
               args = list("y", list(temp$`all`)),  # put it in a list
               label = "Universal")))
    ))

ggplotly(p)

###### **(Please be patient with this visualization as it might take a couple of minutes before showing up. Anyone else having the same issues while using plotly library on R Notebooks? This visulization was supposed to be the show stealer here though. Damnit)**

## Who has the best recent content?  

#We'll compare the Streaming Giants starting from 2010 and observe which Service has been acquiring/producing cocntent with high ratings from both IMDb & Rotten Tomatoes

#Since 2013, both Amazon & Netflix have started producing content under their banners which is catered exclusively to their streaming services only. Netflix's first production was - House of Cards

#![Not a bad start for Netflix](https://media.giphy.com/media/1URYTNvDM2LJoMIdxE/giphy.gif)

## This code piece has been inspired from the Kernel published by ArnabP

animate <- df %>%
  filter(Year >= 2000, Rotten.Tomatoes >= 0.8, IMDb >= 8) %>%
  group_by(Service, Year) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  arrange(Year, Count) %>%
  mutate(Order = 1:n())


animated_gif <- ggplot(data = animate) + 
  geom_bar(mapping = aes(x = Order, y = Count, fill = Service), 
           show.legend = FALSE, 
           stat = "identity", 
           width = 0.50) + 
  transition_states(states = as.integer(Year), transition_length = 20, state_length = 50) +
  scale_x_continuous(breaks = animate$Order, 
                     labels = animate$Service) +
  view_follow(fixed_y = TRUE) + 
  labs(x = "Streaming Platform", 
       y = "No of Shows", 
       title = "Shows by Year: {closest_state}", 
       caption = "TV Shows ")

animated_gif

### ** Top Shows by Streaming Platfrom**

###### Top Shows by IMDb & Rotten Tomatoes Ratings on Netflix

df %>%
  filter(Service == "Netflix", IMDb > 8 & Rotten.Tomatoes > 0.8)%>%
  arrange(-IMDb)%>%
  select(1:6)%>%
  top_n(10,IMDb)

###### Top Shows by IMDb & Rotten Tomatoes Ratings on Hulu

df %>%
  filter(Service == "Hulu", IMDb > 8 & Rotten.Tomatoes > 0.8)%>%
  arrange(-IMDb)%>%
  select(1:6)%>%
  top_n(10,IMDb)

###### Top Shows by IMDb & Rotten Tomatoes Ratings on Amazon Prime

df %>%
  filter(Service == "Prime.Video", IMDb > 8 & Rotten.Tomatoes > 0.8)%>%
  arrange(-IMDb)%>%
  select(1:6)%>%
  top_n(10,IMDb)

###### Top Shows by IMDb & Rotten Tomatoes Ratings on Disney Plus

df %>%
  filter(Service == "Disney.", IMDb > 8 & Rotten.Tomatoes > 0.8)%>%
  arrange(-IMDb)%>%
  select(1:6)%>%
  top_n(10,IMDb)

#I'm open for all sorts of suggestions in the comments and would love to have an upvote if you liked the notebook. 

#Thoughts?

#![Please Upvote this as it'd keep me motivated and excited](https://media.giphy.com/media/mGJAoAqNExqly7NGs0/giphy.gif)
