cat("\014")

rm(list = ls())

install.packages("cluster")

#Loading in all the required libraries
library(stringr)
library(plotly)	
library(reshape2)
library(corrplot)
library(dplyr)
library(lubridate)
library(cluster)

#Setting the work directory
setwd("C:/Users/aishwarya.sharma/OneDrive - insidemedia.net/Dell Lattitude - 5300/Aishwarya/Kaggle/Power Generation India")


##################################Preparing the data

#Reaidng in the file
daily_power_df <- read.csv("file.csv")

#Replacing commas in character strings with blanks
daily_power_df <- sapply(daily_power_df, function(x) str_replace_all(x,"\\,",""))

#Replacing the NaN values with 0
daily_power_df[,3:8] <- sapply(daily_power_df[,3:8], function(x) str_replace_all(x,"NaN","0"))

#Converting dataset to data frame
daily_power_df <- as.data.frame(daily_power_df)

#Converting certain columns to numeric
daily_power_df[,3:8] <- sapply(daily_power_df[,3:8], as.numeric)

#Selecting relevant columns
daily_power_df <- daily_power_df[,c(1,2,3,5,7)]

daily_power_df$Total <- rowSums(daily_power_df[,3:5])

######Visualizations
temp2 <- daily_power_df %>%
  select(2:5)%>%
  group_by(Region) %>%
  summarise_each(funs(sum))

temp2 <- melt(temp2, id.vars = "Region")


ggplot(temp2,aes(x = Region, y = value,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_fill_manual(values = c("grey","seagreen","seagreen1"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = "REGION WISE ELECTRICITY SOURCES - INDIA", 
       subtitle = "India can do more in terms of Electricty Production via Green Energy Sources")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab("Energy Spread by Sources")+
  xlab(label = NULL)

###2nd Visualization
temp3 <- daily_power_df %>%
  select(1,3:6) %>%
  group_by(Date) %>%
  summarise_each(funs(sum))

colnames(temp3)[5] <- "Total Energy Production"

temp3 <- melt(temp3, id.vars = "Date")

ggplot(data=temp3, aes(x=as.Date(Date), y=value, group=variable)) +
  geom_line(aes(color = variable))+
  labs(title = "DAY WISE ELECTRICITY PRODUCTION - INDIA", 
       subtitle = "India's Overall Energy Production is on the upper curve")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab("Energy Production in MU")+
  xlab(label = NULL)

#Hydroelectric production sees a jump in the monsoon Months of Jun - Aug while the
#peak for overall electric energy is usually seen during the summer months of Apr-Jun. All
#findings seem to be intuitive

##Power Generation by Month
temp6 <- daily_power_df

temp6$month <-  months(as.Date(daily_power_df$Date))

temp6 <- temp6 %>%
  select(6,7) %>%
  group_by(month)%>%
  summarise(Total = mean(Total))

ggplot(temp6, aes(month, Total)) +   
  geom_bar(aes(fill = "Spectral"),position = "dodge", stat="identity")+
  labs(title = "MONTH WISE ELECTRICITY PRODUCTION - INDIA", 
       subtitle = "Overall India's Energy Production remained more or less at the same levels each month between 2017-2020")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")+
  ylab("Energy Production in MU")+
  xlab(label = NULL)



######Loading in the statewise data
power_df <- read.csv("State_Region_corrected.csv")

colnames(power_df) <- c("STATE.NAME","Area","Region","Share.Perc")

power_df$STATE.NAME <- toupper(power_df$STATE.NAME)

power_df$Region <- toupper(power_df$Region)

power_df <- power_df[,c(1,3,4)]

temp <- power_df%>%
  group_by(Region)%>%
  summarise(Share.Perc = sum(Share.Perc))

# Create a basic bar
pie = ggplot(temp, aes(x="", y=Share.Perc, fill=Region)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Share.Perc), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Energy Consumption - Region")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))

pie

#########Loading the India census data to gather some more insights about the data
pop_df <- read.csv("elementary_2015_16.csv")

pop_df <- na.omit(pop_df)

pop_df[is.na(pop_df)] <- 0

#Selecting relevant columns
pop_df <- pop_df[,c(3,5,9,10,11,12,15)]

colnames(pop_df)[c(2,4:7)] <- c("TOTAL.POPULATION","SC.POPULATION","ST.POPULATION","OVERALL.LITERACY","AREA")

#Converting % to numbers
pop_df$SC.POPULATION <- round(as.numeric(pop_df$TOTAL.POPULATION) * as.numeric(pop_df$SC.POPULATION)/100,0)

pop_df$ST.POPULATION <- round(as.numeric(pop_df$TOTAL.POPULATION) * as.numeric(pop_df$ST.POPULATION)/100,0)

pop_df$OVERALL.LITERACY <- round(as.numeric(pop_df$TOTAL.POPULATION) * as.numeric(pop_df$OVERALL.LITERACY)/100,0)

df2 <- pop_df%>%
  group_by(STATE.NAME)%>%
  summarise(SEX.RATIO = round(mean(SEX.RATIO),0))

df1 <- pop_df%>%
  group_by(STATE.NAME)%>%
  summarise_each(funs(sum),-SEX.RATIO)

#Joining the data
df <- inner_join(df1,df2, by = "STATE.NAME")

#Joining the two different datasets
df <- left_join(df,power_df,by = "STATE.NAME")

df$Share.Perc[is.na(df$Share.Perc)] <- 0

df$Region[is.na(df$Region)] <- "UNASSIGNED"

#Adding additional information 
df$POWER.PER.PERSON <- (mean(daily_power_df$Total)*365)/df$TOTAL.POPULATION

df$POWER.PER.KM2 <- (mean(daily_power_df$Total)*365)/df$AREA

df$TOTAL.POWER <- (mean(daily_power_df$Total)*365) * df$Share.Perc

###Top 5 states which consume the most energy per person  
temp5 <- df%>%
  filter(Region != "UNASSIGNED")%>%
  select(1,10)%>%
  arrange(-POWER.PER.PERSON)%>%
  head(5)

###Top 5 states with lowest energy per person 
temp7 <- df%>%
  filter(Region != "UNASSIGNED")%>%
  select(1,10)%>%
  arrange(POWER.PER.PERSON)%>%
  head(5)


###Top 5 states which have the most energy by per Area Km2
temp8 <- df%>%
  filter(Region != "UNASSIGNED")%>%
  select(1,11)%>%
  arrange(-POWER.PER.KM2)%>%
  head(5)

###Top 5 with lowest energy per Area Km2
temp9 <- df%>%
  filter(Region != "UNASSIGNED")%>%
  select(1,11)%>%
  arrange(POWER.PER.KM2)%>%
  head(5)



###Creating a correlation matrix from the given data
temp4 <- df[,-c(1,8,9)]

temp4 <- sapply(temp4, as.numeric)

temp4 <- as.matrix(temp4)

corrplot(cor(temp4),method = "number", number.cex = .6, tl.cex = 0.75)


#Area the major factor while for energy share followed by ST Population which seems 
#counter intuitive 

#So with the present metrics we will cluster the States

clustering.data <- df

clustering.data <- clustering.data %>%
  filter(Region  != "UNASSIGNED")

clustering.data <- clustering.data[,-8]

clustering.data[,2:11] <- scale(clustering.data[,2:11])


clustering.data <- column_to_rownames(clustering.data, "STATE.NAME")


fviz_nbclust(clustering.data, kmeans, method = "wss")


k2 <- kmeans(clustering.data, centers = 4, nstart = 25)


fviz_cluster(k2, data = clustering.data, labelsize = 8)

