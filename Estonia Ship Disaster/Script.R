cat("\014")

rm(list = ls())

dev.off()

setwd("C:/Users/aishwarya.sharma/OneDrive - insidemedia.net/Dell Lattitude - 5300/Aishwarya/Kaggle/Estonia")

library(htmltools)

library(tidyr)

library(corrplot)

library(caret)

library(leaflet)

library(ROCR)

library(data.table)

library(dplyr)

library(ggplot2)


##Creating a column with age groups
agebreaks <- c(0,18,34,44,54,65,100)

agelabels <- c("0-18","19-34","35-44","45-54","55-64","65+")

df$agegroups <- as.factor(df$agegroups)

df <- read.csv("estonia-passenger-list.csv")

setDT(df)[ , agegroups := cut(df$Age, 
                              breaks = agebreaks, 
                              right = FALSE, 
                              labels = agelabels)]

### Where did the Passenger and Crew belong to? 
geo_df <-read.csv("Geocode.csv")

colnames(geo_df) <- c("lat","long","Country")

df.join <- left_join(df,geo_df, c("Country"))

temp <- df.join%>%
  group_by(Country,lat,long)%>%
  summarise(Count = length(Country))%>%
  arrange(-Count)

pal <- colorFactor(
  palette = 'Spectral',
  domain = df$Country
)

leaflet(temp)%>%
  addTiles() %>%
  addCircles(~long, ~lat, weight = 1,
             radius = ~(Count)*500,popup = ~Country,color = ~pal(Country), label = ~htmlEscape(Country))%>%
  setView(lng = 20, lat = 55, zoom = 3)


####Creating dummy variables
#After observing a large number of passenger & crew belonged to either Sweden or Estonia 
#we've created a couple of dummy variables

df$Country_Sweden <- ifelse(df$Country == "Sweden",1,0)

df$Country_Estonia <- ifelse(df$Country == "Estonia",1,0)


####Checking correlation between different variables
temp <- df[,c(5:8,10,11)]

temp$Sex <- ifelse(temp$Sex == "M",1,0)

temp$Category <- ifelse(temp$Category == "C",1,0)

corrplot(cor(temp,use="pairwise.complete.obs"),method = "number")


###Passenger vs Crew
temp2 <- as.data.frame(table(df$Survived,df$Category))

colnames(temp2) <- c("Survival","Type","Count")

ggplot(temp2, aes(fill=Survival, y=Count, x=Type)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=Count), position =position_stack(vjust = 0.5))+
  xlab("Passenger Type")+
  ylab("Count of Passenger Type")+
  ggtitle("Passenger vs Crew ",subtitle = "The crew had a relatively higher chance of surviving than the passengers")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


####Age
temp3 <- df %>%
  select(agegroups,Sex,Survived)%>%
  group_by(agegroups,Sex)%>%
  summarise(Total = length(Survived))

ggplot(temp3, aes(fill=Sex, y=Total, x=agegroups)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25)+
  xlab("Different Age Groups")+
  ylab("Count of Passenger by Sex")+
  ggtitle("Passenger Count By Age Groups")+
  theme(plot.title = element_text(hjust = 0.5))

temp4 <- df%>%
  select(Age,Survived)

temp5 <- temp4 %>%
  filter(Survived == 0)%>%
  select(Age)

temp5 <- sapply(temp5, as.numeric)

temp6 <- temp4 %>%
  filter(Survived == 1)%>%
  select(Age)

temp6 <- sapply(temp6, as.numeric)

par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)

hist(temp5, col=rgb(1,0,0,0.5), xlab="Age Group", 
     ylab="Frequency", main="Age Distribution of Non-Survivors vs Survivors" )
abline(v = mean(temp5), col = "blue", lwd = 2)
text(65, 150, "Mean Age of 46.27")


hist(temp6, col=rgb(0,0,1,0.5),xlab="Age Group", 
     ylab="", main="")
abline(v = mean(temp6), col = "red", lwd = 2)
text(45, 25, "Mean Age of 34.01")

legend("topright", c("Survivor", "Non-Survivor"), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),cex = 0.5, pch = 1, bty = "n")
