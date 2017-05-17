#Code written by Alex Katona
#Check out alexkatona.blogspot.com for more info

#load libraries after using install.packages() for each
library(readr) #more efficiently read data into R
library(dplyr) #to manipulate data (i.e. data wrangling)
library(ggplot2) #for visualization
library(ggthemes) #for Tufte theme
library(lubridate) #for working with dates more easily

#load the SF crime dataset exported from https://data.sfgov.org/Public-Safety/Map-Crime-Incidents-from-1-Jan-2003/gxxq-x39z/data. (At the time of this analysis, row count is 2,017,792) 
crime_data <- read_csv("Map__Crime_Incidents_-_from_1_Jan_2003.csv", col_names = TRUE)

#add new date columns to dataset using dplyr's mutate function

crime_data <- crime_data %>% mutate(
  #create cleaned date column
  Date_clean = as.Date(Date,format='%m/%d/%Y'), 
  #create year column
  Year = year(Date_clean),
  #create month column
  Month = month(Date_clean),
  #create weekday column with day name
  Weekday = wday(Date_clean, label = TRUE),
  #add Count column of all 1's to sum for counts more easily
  Count = 1
)

#filter for 2015 onward to limit data size (at the time of this analysis: 311,608 rows)
crime_data_filtered <- filter(crime_data, Year >= 2015)

#What are the top 5 categories of crime?

#group data by category of crime using dplyr
crimes_ranked <- crime_data_filtered %>%
  select(Category, Count) %>%
  group_by(Category) %>% 
  #summarize number of incidents by category
  summarise_each(funs(sum), Count) %>% 
  #sort descending by Count
  arrange(desc(Count))

#find top 5 categories of crime
top_5_crimes <- head(crimes_ranked,5)

#boxplot data engineering
boxplot_data <- crime_data_filtered %>%
  #filter for only the top 5 crimes
  filter(Category %in% top_5_crimes$Category) %>%
  select(Category, Weekday, Count, Date_clean) %>%
  group_by(Category, Weekday, Date_clean) %>% 
  summarise_each(funs(sum), Count)

#ggplot can be thought of as adding layers on top of each other. To create highlighting, filter for the data that you want and then add a boxplot using that data with different aesthetics on top of the boxplot with all of the data when calling ggplot()

#determine days for filtering
days <- c("Friday","Saturday")

#define data for boxplot highlighting
boxplot_highlight_data <- crime_data_filtered %>%
  #filter for the crime we want to highlight
  filter(Category == "LARCENY/THEFT") %>%
  #filter for the days we want to highlight
  filter(DayOfWeek %in% days) %>%
  select(Category, Weekday, Count, Date_clean) %>%
  group_by(Category, Weekday, Date_clean) %>% 
  summarise_each(funs(sum), Count)

#create novel boxplot based on Tufte principles
#inspired by Tufte boxplot on page 125 of the Visual Display of Quantitative Information

#build Tufte boxplot with all the data first, apply Tufte theme, remove axis title and make plot title bigger, create a boxplot for each day by category using facet_grid, add a title using ggtitle, add another boxplot using the data you want to highlight on top of the first boxplot and change the color. Also, change width of line using "size" to make it easier for colorblind users to see (colorblind color from colorbrewer.org (3-class YlOrRd))
ggplot(boxplot_data, aes(x = factor(Weekday),y = Count)) + geom_tufteboxplot(outlier.colour="transparent", size = 0.5) + theme_tufte() + theme(axis.title=element_blank(), plot.title = element_text(size = 14)) + facet_grid(~Category) + ggtitle("Larceny/Theft increases on Fridays and Saturdays whereas other top crimes are more consistent")+ geom_tufteboxplot(data = boxplot_highlight_data, outlier.colour="transparent",aes(x = factor(Weekday),y = Count),color = "#e34a33", size =1)   
