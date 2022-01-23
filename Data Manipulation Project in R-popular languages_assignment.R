knitr::opts_chunk$set(cache=TRUE)
options(scipen = 9999)
rm(list=ls())
#install.packages('ggthemes', dependencies = T)
library('tidyverse')
library('ggthemes')
library('ggplot2')

responses <- read.csv('C:\\Users\\yerran\\Documents\\rcodes\\kagglesurvey.csv')
View(responses)

# Printing the first 10 rows
head(responses, 10)

# Printing the first respondents' tools and languages
responses$WorkToolsSelect[1]

tools <- responses

tools <- tools  %>% 
  mutate(work_tools = strsplit(WorkToolsSelect, ",")) %>%
  unnest(work_tools)
View(tools)

# Creating a new data frame
tool_count <- tools

# 2: Group the data by work_tools, calculate the number of responses in each group
#3: Sort tool_count so that the most popular tools are at the top

tool_count <- tool_count  %>% 
  group_by(work_tools)  %>% 
  summarise(count = n()) %>%
  # Sorting tool_count so that the most popular tools are at the top
  arrange(desc(count))

# 4: Print top 6 records of tool_count
head(tool_count)

#1. Create a bar chart of the work_tools column, most counts on the far right. Arrange the bars so that the tallest are on the far left. And also Rotate the bar labels on x-axis by 90 degrees

ggplot(tool_count, aes(x = reorder(work_tools, count), 
                       y = count)) + 
  geom_bar(stat = "identity",
           aes(fill = work_tools)) +
  
  # Adding non basic-cosmetic theme
  theme_fivethirtyeight() +
  
  # Rotating the bar labels 90 degrees
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1)) +
  labs(title = "The Most Popular Tools in Data Science", 
       x = "Tools", 
       y = "Count") +
  theme(legend.position="none")

# 1. Creating a new data frame called debate_tools
debate_tools <- responses

# Creating a new column called language preference, based on the conditions specified in the Instructions
debate_tools <- debate_tools  %>% 
  mutate(language_preference = case_when(
    grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "R",
    ! grepl("R", WorkToolsSelect) & grepl("Python", WorkToolsSelect) ~ "Python",
    grepl("R", WorkToolsSelect) & grepl("Python", WorkToolsSelect) ~ "both",
    ! grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "neither"))

# Printing the first 6 rows
head(debate_tools)

# Creating a new data frame
debate_plot <- debate_tools

# Grouping by language preference and calculate number of responses
debate_plot <- debate_plot  %>% 
  group_by(language_preference)  %>% 
  summarise(count = n())  %>% 
  
  # Removing the row for users of "neither"
  filter(language_preference != "neither")

# Creating a bar chart
ggplot(debate_plot, aes(x = language_preference, 
                        y = count)) + 
  geom_bar(stat = "identity", 
           aes(fill = language_preference)) +
  
  theme_fivethirtyeight() +
  
  # Rotating the bar labels 90 degrees
  theme(axis.text.x = element_text(vjust = 0.5,
                                   hjust = 0.5)) +
  labs(title = "The Most Popular Tools in Data Science", 
       x = "Tools", y = "Count")

# Creating a new data frame
recommendations <- debate_tools

# Grouping by language_preference and then LanguageRecommendationSelect
recommendations <- recommendations  %>% 
  group_by(language_preference, LanguageRecommendationSelect)  %>% 
  summarise(count = n()) %>%
  
  # Removing empty responses and include the top recommendations
  filter(LanguageRecommendationSelect != "NA") %>%
  arrange(language_preference, desc(count)) %>%
  mutate(row_no = row_number(language_preference)) %>%
  filter(row_no <= 4)

recommendations

# Creating a faceted bar plot
ggplot(recommendations, aes(x = LanguageRecommendationSelect,
                            y = count)) +
  geom_bar(stat = "identity",
           aes(fill = LanguageRecommendationSelect)) +
  facet_wrap(~ language_preference) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5)) +
  labs(title = "The most recommended language by the language used", 
       x = "Tools", y = "Count") 