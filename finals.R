olympic <- read.csv("~/Documents/McGill/Fall Term/MGSC 661/Final/Olympic events data.csv")

library(ggplot2)
library(skimr)
library(tidyr)
library(dplyr)

## Check structure of dataset
str(olympic) #271116 obs

## Check for missing values
skim(olympic)

# Fill missing medal values with 'None'
olympic$Medal[is.na(olympic$Medal)] <- "None"

# Understanding the unique values of select cols
unique(olympic$Team)
# Team does not represent the country, will have to consider NOC column, which is taken from kaggle

# Import NOC dataset
noc <- read.csv("~/Documents/McGill/Fall Term/MGSC 661/Final/noc_regions.csv")

# Merge NOC dataset with olympic set, keeping original olympic set intact
olympic <- merge(olympic, noc, by='NOC', all.x=TRUE)

# Investigate missing values again after merg
skim(olympic)

# Filter rows where region col has missing values
missing_region <- olympic[is.na(olympic$region),]

# Investigate missing region NOC to fill in blanks
unique(missing_region$NOC) # ROT, SGP, TUV, UNK

# Check if the missing region contains any medals
missing_region[missing_region$Medal != "None", ]

# Set up lookup table to fill in missing region values 
country_ref <- data.frame(
  NOC = c('ROT', 'SGP', 'TUV', 'UNK'),
  region = c('Refugee Olympic Team', 'Singapore', 'Tuvalu', 'Kosovo')
)

# Merge dataset again to fill in missing region
olympic <- merge(olympic, country_ref, by='NOC', all.x=TRUE)

# Use the region from noc (region.y) to fill in missing values in olympic (region.x)
olympic$region <- coalesce(olympic$region.x, olympic$region.y)

# Drop the old region.x and region.y columns
olympic <- olympic[, !colnames(olympic) %in% c("region.x", "region.y")]

# Games col = Year + Season. Create vector of cols to drop
cols_drop <- c('ID', 'Team', 'Games', 'City', 'notes', 'NOC')

# Drop cols using -ve index
olympic <- olympic[, !names(olympic) %in% cols_drop]

# Count the number of occurrences of each sport
sport_count <- olympic %>%
  group_by(Sport) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Sort by count in descending order

# Plot the bar graph
ggplot(sport_count, aes(x = reorder(Sport, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Participations in Each Sport", 
       x = "Sport", 
       y = "Number of Participations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#dev.off()

# Count the number of winter vs summer olympic sports
#season_sports_count <- olympic %>%
 # group_by(Season) %>%
  #summarise(unique_sports = n_distinct(Sport))  # Count unique sports for each season

# Print the result
#print(season_sports_count)

unique(olympic[olympic$Season == 'Winter',]$Sport)

# Gymnastics observations
gymnastics <- olympic[olympic$Sport == 'Gymnastics',]
unique(gymnastics$Event)
skim(gymnastics) # 26707 observations in total before dropping NA

ski <- olympic[olympic$Sport == 'Alpine Skiing',] 
unique(ski$Event)
skim(ski) # 8829 observations before dropping NA

#olympic[olympic$Sport == 'Figure Skating',]

# Count medals won by each athlete
#medal_won <- olympic[olympic$Sport %in% c("Gymnastics", "Alpine Skiing",'Figure Skating'), ]
#medals_by_athlete <- medal_won %>% group_by(Name) %>% summarise(medals_won=sum(Medal!='None'),na.rm=TRUE)
#head(medals_by_athlete)

#olympic[olympic$Name=='Larysa Semenivna Latynina (Diriy-)',]

############ Decide on the objectives the project will be based on #############
# Random forest for gymnastics athlete
# PCA/Clustering for gymnastics and ski athletes

### Pre-processing
# Drop null values
gymnastics <- na.omit(gymnastics) # dropped to 18271 (31.6% dataset with na values)
ski <- na.omit(ski) # dropped to 6322 (28.5% dataset with na values)

# Drop dup values
gymnastics <- gymnastics %>%
  distinct() 

ski <- ski %>%
  distinct()

# Check the number of dropped rows
skim(gymnastics) # 2163 unique athletes
skim(ski) # 1753 unique athletes

# Find year distribution of each sport
summary(gymnastics$Year) # started since 1896, latest 2016
summary(ski$Year) # started since 1936, latest 2014

######## Understand distribution of athlete participation over the years
### Gymnastics (count all observations even if same athlete took part in multiple events that year)
gym_athletes_yr <- as.data.frame(table(gymnastics$Year))
colnames(gym_athletes_yr) <- c("Year", "Num_Athletes")

# Plot bar graph to observe trends for gymnastics athletes
ggplot(gym_athletes_yr, aes(x=as.factor(Year), y=Num_Athletes)) +
  geom_bar(stat="identity", fill='skyblue') + # stat = identity indicates to use the actual values in the y aesthetic ie translte directly to height of the bars
  theme_minimal() + 
  labs(title='Number of Gymnastics participations over the Years', 
       x = 'Year',
       y = 'Number of Participations')

# Unique number of athletes each year
unique_gym_year <- gymnastics %>%
  group_by(Year) %>%
  summarise(unique_athletes = n_distinct(Name))

# Now plot again 
ggplot(unique_gym_year, aes(x=as.factor(Year), y=unique_athletes)) + 
  geom_bar(stat='identity', fill='navy') + 
  labs(title = 'Number of unique Gymnastics athletes per year', x = 'Year', y = 'Number of Athletes') + theme_minimal()

# Combine both data frames into one
gym_participations <- merge(gym_athletes_yr, unique_gym_year, by = "Year")

# Reshape data to long format for plotting
gym_participations_long <- gym_participations %>%
  pivot_longer(cols = c(Num_Athletes, unique_athletes), 
               names_to = "Metric", 
               values_to = "Count")

# Plot both metrics on the same plot
ggplot(gym_participations_long, aes(x = as.factor(Year), y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
  scale_fill_manual(values = c("Num_Athletes" = "navy", "unique_athletes" = "lightgrey"), 
                    labels= c("Total Participations", "Number of Athletes")) +
  theme_minimal() +
  labs(title = 'Number of Gymnastics Participations vs Unique Athletes per Year', 
       x = 'Year', y = 'Count', fill=NULL) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis labels


#### Now repeat for ski athletes 
# Count all observations even if same athlete took part in multiple events that year
ski_athletes_yr <- as.data.frame(table(ski$Year))
colnames(ski_athletes_yr) <- c('Year', "Num_Athletes")

ggplot(ski_athletes_yr, aes(x=as.factor(Year), y=Num_Athletes)) +
  geom_bar(stat="identity", fill='skyblue') + 
  theme_minimal() + 
  labs(title='Number of Alpine Ski participations over the Years', 
       x = 'Year',
       y = 'Number of Participations')

# Unique no of ski athletes
unique_ski_year <- ski %>%
  group_by(Year) %>%
  summarise(unique_athletes = n_distinct(Name))

# Now plot again 
ggplot(unique_ski_year, aes(x=as.factor(Year), y=unique_athletes)) + 
  geom_bar(stat='identity', fill='navy') + 
  labs(title = 'Number of unique Alpine Ski athletes per year', x = 'Year', y = 'Number of Athletes') + theme_minimal()

# Combine both data frames into one
ski_participations <- merge(ski_athletes_yr, unique_ski_year, by = "Year")

# Reshape data to long format for plotting
ski_participations_long <- ski_participations %>%
  pivot_longer(cols = c(Num_Athletes, unique_athletes), 
               names_to = "Metric", 
               values_to = "Count")

# Plot both metrics on the same plot
ggplot(ski_participations_long, aes(x = as.factor(Year), y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
  scale_fill_manual(values = c("Num_Athletes" = "navy", "unique_athletes" = "lightgrey"), 
                    labels= c("Total Participations", "Number of Athletes")) +
  theme_minimal() +
  labs(title = 'Number of Alpine Ski Participations vs Unique Athletes per Year', 
       x = 'Year', y = 'Count', fill=NULL) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis labels


################# Understanding the events in each sport
table(ski$Event) # 10 distinct events 
table(gymnastics$Event) 

##### Gymnastic events underwent transformation throughout the years, especially for men's events
subset_gym <- gymnastics %>% 
  filter(Event %in% low_count_events)
# Subsetted events were discontinued, to regroup into the next closest categories

# Create mapping for discontinued events
gym_map <- data.frame(
  Discontinued = c("Gymnastics Men's Horizontal Bar, Teams", "Gymnastics Men's Individual All-Around, 5 Events", "Gymnastics Men's Individual All-Around, Apparatus Work", "Gymnastics Men's Individual All-Around, Field Sports", "Gymnastics Men's Parallel Bars, Teams", "Gymnastics Men's Team All-Around, Swedish System"),
  Cat = c("Gymnastics Men's Horizontal Bar", "Gymnastics Men's Individual All-Around", "Gymnastics Men's Individual All-Around", "Gymnastics Men's Individual All-Around", "Gymnastics Men's Parallel Bars", "Gymnastics Men's Team All-Around")
)

# Merge gymnastics data with the mapped over data
gymnastics <- gymnastics %>% 
  left_join(gym_map, by=c("Event"="Discontinued")) %>% 
  mutate(Event = ifelse(is.na(Cat), Event, Cat)) %>% 
  select(-Cat) # Drop temporary column

# Drop rope climbing event due to inappropriate nature among artistic gymnastic venue. women's portable apparatus as it more closely resembles rhythmic gymnastics
gymnastics <- gymnastics[gymnastics$Event != "Gymnastics Men's Rope Climbing",]
gymnastics[gymnastics$Event=="Gymnastics Women's Team Portable Apparatus",] # 54 rows, but the event is more appropriate under rhythmic gymnastics. drop
gymnastics <- gymnastics[gymnastics$Event != "Gymnastics Women's Team Portable Apparatus",]

# Count the number of participants by Event and Year
gymevent_participation_years <- gymnastics %>%
  group_by(Year, Event, Sex) %>%
  summarise(Num_Participants = n(), .groups = 'drop')

# Visualise changes in participations across the years
ggplot(gymevent_participation_years, aes(x = Year, y = Num_Participants, color = Event, group=Event)) +
  geom_line(size=1) + 
  theme_minimal() +
  labs(title = 'Event Participation in Gymnastics Over the Years',
       x = 'Year', y = 'Number of Participations')

#### Ski participation
# Count the number of participants by Event and Year
skievent_participation_years <- ski %>%
  group_by(Year, Event, Sex) %>%
  summarise(Num_Participants = n(), .groups = 'drop')

# Visualise changes in participation across the years
ggplot(skievent_participation_years, aes(x = Year, y = Num_Participants, color = Event, group=Event)) +
  geom_line(size=1) + 
  theme_minimal() +
  labs(title = 'Event Participation in Alpine Skiing Over the Years',
       x = 'Year', y = 'Number of Participations')

################# Country distribution for each sport & Medal
## Gymnastics event participation
n_distinct(gymnastics$region) # 83 countries participated

# Compute the number of participations from each country
gym_country <- gymnastics %>% 
  group_by(region) %>%
  summarise(Num_Participations = n()) %>%
  arrange(desc(Num_Participations))

# Plot gymnasts country distribution
ggplot(gym_country, aes(x=reorder(as.factor(region), -Num_Participations), y=Num_Participations)) + 
  geom_bar(stat='identity', fill='skyblue') + 
  theme_minimal() + 
  labs(title='Number of Gymnastics Participations by Country', x='Country', y='Number of Participations') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# top 5 countries w participation: Germany, Russia, USA, Japan, France

## Ski event participation
ski_country <- ski %>% 
  group_by(region) %>%
  summarise(Num_Participations = n()) %>%
  arrange(desc(Num_Participations))

# Plot ski athletes country distribution
ggplot(ski_country, aes(x=reorder(as.factor(region), -Num_Participations), y=Num_Participations)) + 
  geom_bar(stat='identity', fill='skyblue') + 
  theme_minimal() + 
  labs(title='Number of Alpine Ski Participations by Country', x='Country', y='Number of Ski Participations') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# top 5 countries w participation: Austria, Switzerland, USA, France, Germany

## Gymnastics total medals 
gym_medals_by_country <- gymnastics %>%
  filter(Medal != "None") %>%  # Exclude rows without medals
  group_by(region, Medal) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(region, Medal)

# Find number of countries with medals
n_distinct(gym_medals_by_country$region) # only 31/83 countries won at events throughout the decades/ century

# Applying order to medal column
gym_medals_by_country$Medal <- factor(gym_medals_by_country$Medal, levels = c("Gold", "Silver", "Bronze"))

# Plot medal accomplishments per country
ggplot(gym_medals_by_country, aes(x = reorder(region, -Count), y = Count, fill = Medal)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Medals Won by Country in Gymnastics",
       x = "Country",
       y = "Number of Medals",
       fill = "Medal Type") +
  scale_fill_manual(values = c("Gold" = "orange", "Silver" = "grey", "Bronze" = "lightblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Russia, Japan, USA, Romania, China with most number of medals

## Understand the number of unique athlete participation throughout the years for the top 5 performing countries
total_medals_by_country <- gym_medals_by_country %>%
  group_by(region) %>%
  summarise(Total_Medals = sum(Count), .groups = 'drop') %>%
  arrange(desc(Total_Medals)) %>%
  head(5)  # Get top 5 countries performing countries

# Filter gymnastics data to include only top 5 countries
gym_top_5 <- gymnastics %>%
  filter(region %in% total_medals_by_country$region)  # Keep only top 5 countries

# Calculate the number of unique athletes per year for the top 5 countries
unique_gym_year_top5 <- gym_top_5 %>%
  group_by(region, Year) %>%
  summarise(Unique_Athletes = n_distinct(Name), .groups = "drop")

# Visualize the result
ggplot(unique_gym_year_top5, aes(x = Year, y = Unique_Athletes, fill = region, group = region)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values = c("USA" = "navy", "China" = "pink", "Russia" = "lightgrey", "Romania" = "purple", "Japan" = "orange")) + 
  theme_minimal() +
  labs(title = "Number of Unique Gymnastics Athletes per Year (Top 5 Countries)",
       x = "Year",
       y = "Unique Athletes",
       fill = "Country")

# Calculate the total number of medals per athlete
gym_athlete_medals_top5 <- gymnastics %>%
  filter(Medal != "None", region %in% gym_top_5$region) %>%  # Exclude non-medal rows and filter for top 5 countries
  group_by(region, Name, Sex) %>%  # Group by region and athlete name
  summarise(Total_Medals = n(), .groups = 'drop')  # Count medals per athlete

# Box plot showing the range of medals per athlete for top 5 countries
ggplot(gym_athlete_medals_top5, aes(x = region, y = Total_Medals, fill = region)) +
  geom_boxplot() +
  facet_wrap(~ Sex, scales = 'free', labeller = labeller(Sex=c("F"="Female", "M"="Male"))) +
  theme_minimal() +
  labs(title = "Number of Medals per medalist in Top 5 Gymnastics Countries split according to gender",
       x = "Country",
       y = "Total Medals per Athlete",
       fill = "Country") + 
  theme(legend.position = "none")

# Number of medals per country over time for top 5 performing countries
medals_gym_country_top5 <- gym_top_5 %>%
  filter(Medal != 'None') %>%
  group_by(region, Year) %>%
  summarise(Medals_per_yr =n(), .groups='drop')

# Plot
ggplot(medals_gym_country_top5, aes(x=Year, y=Medals_per_yr, color=region, group=region)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(title= 'Number of medals of top 5 performing countries in Gymnastics over time',
       x = 'Year',
       y = 'Number of Medals',
       color='Country')


## Ski total medals
ski_medals_by_country <- ski %>%
  filter(Medal != "None") %>%  # Exclude rows without medals
  group_by(region, Medal) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(region, Medal)

# Applying order to medal column
ski_medals_by_country$Medal <- factor(ski_medals_by_country$Medal, levels = c("Gold", "Silver", "Bronze"))

# Plot medal accomplishments per country
ggplot(ski_medals_by_country, aes(x = reorder(region, -Count), y = Count, fill = Medal)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Medals Won by Country in Alpine Skiing",
       x = "Country",
       y = "Number of Medals",
       fill = "Medal Type") +
  scale_fill_manual(values = c("Gold" = "orange", "Silver" = "grey", "Bronze" = "lightblue")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Calculate the total number of medals per athlete
ski_athlete_medals_top5 <- ski %>%
  filter(Medal != "None", region %in% c("Austria", "Switzerland", "USA", "France", "Germany")) %>%  # Exclude non-medal rows and filter for top 5 countries
  group_by(region, Name, Sex) %>%  # Group by region and athlete name
  summarise(Total_Medals = n(), .groups = 'drop')  # Count medals per athlete

# Box plot showing the range of medals per athlete for top 5 countries
ggplot(ski_athlete_medals_top5, aes(x = region, y = Total_Medals, fill = region)) +
  geom_boxplot() +
  facet_wrap(~Sex, scales='free', labeller=labeller(Sex=c("F"="Female", "M"="Male"))) +
  theme_minimal() +
  labs(title = "Number of medals per medalist in Top 5 Alpine Ski Countries according to gender",
       x = "Country",
       y = "Total Medals per Athlete",
       fill = "Country") +
  theme(legend.position = "none")

# Number of medals won by each country for the top 5 countries over time
ski_medals_top5 <- ski %>%
  filter(Medal != "None", region %in% c("Austria", "Switzerland", "USA", "France", "Germany")) %>% 
  group_by(region, Year) %>%  # Group by region and athlete name
  summarise(Total_Medals = n(), .groups = 'drop')  # Count medals per athlete

# Box plot showing the range of medals per athlete for top 5 countries
ggplot(ski_medals_top5, aes(x = Year, y = Total_Medals, color = region, group=region)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Number of medals of top 5 performing countries in Alpine Ski over time",
       x = "Year",
       y = "Number of Medals",
       color = "Country")

skim(gymnastics) # Number of unique gymnasts dropped to 2160 after certain events were removed
# Number of ski athletes remained the same

########### Gender 
table(gymnastics$Sex) # 7521 F, 10693 M
table(ski$Sex) # 2604 F, 3718 M

## Gymnasts participation by gender
gym_gender_yr <- gymnastics %>%
  #filter(Medal != "None") %>%  # Exclude rows with no medals
  group_by(Sex, Year) %>%  # Group by gender
  summarise(Total_Participants = n(), .groups = 'drop')

ggplot(gym_gender_yr, aes(x = Year, y = Total_Participants, color = Sex, group = Sex)) +
  geom_line() +  # Line plot to show participation trend
  geom_point() +  # Add points to highlight individual data points
  theme_minimal() + 
  labs(title = "Gymnastics Participation by Gender Over the Years",
       x = "Year", 
       y = "Total Participants",
       color = "Gender") 

## Ski participation by gender
ski_gender_yr <- ski %>%
  #filter(Medal != "None") %>%  # Exclude rows with no medals
  group_by(Sex, Year) %>%  # Group by gender
  summarise(Total_Participants = n(), .groups = 'drop')

ggplot(ski_gender_yr, aes(x = Year, y = Total_Participants, color = Sex, group = Sex)) +
  geom_line() +  # Line plot to show participation trend
  geom_point() +  # Add points to highlight individual data points
  theme_minimal() + 
  labs(title = "Alpine Ski Participation by Gender Over the Years",
       x = "Year", 
       y = "Total Participations",
       color = "Gender") 

## Medals amassed by athlete gender
# Gymnasts
gym_medals_gender <- gymnastics %>%
  filter(Medal != "None") %>%  # Exclude rows with no medals
  group_by(Sex, Year) %>%           # Group by gender
  summarise(Total_Medals = n(), .groups = 'drop')  # Count total medals per gender

ggplot(gym_medals_gender, aes(x = Year, y = Total_Medals, color = Sex, group = Sex)) +
  geom_line() +  # Line plot to show trend
  geom_point() +  # Points to highlight individual data points
  theme_minimal() +
  labs(title = "Total Medals Amassed by Gymnastics Athletes by Gender Over the Years",
       x = "Year",
       y = "Total Medals",
       color = "Gender") +
  scale_color_manual(values = c("F" = "pink", "M" = "lightblue"))  # Customize line colors

# Ski
ski_medals_gender <- ski %>%
  filter(Medal != "None") %>%  # Exclude rows with no medals
  group_by(Sex, Year) %>%           # Group by gender
  summarise(Total_Medals = n(), .groups = 'drop')  # Count total medals per gender

ggplot(ski_medals_gender, aes(x = Year, y = Total_Medals, color = Sex, group = Sex)) +
  geom_line() +  # Line plot to show trend
  geom_point() +  # Points to highlight individual data points
  theme_minimal() +
  labs(title = "Total Medals Amassed by Alpine Ski Athletes by Gender Over the Years",
       x = "Year",
       y = "Total Medals",
       color = "Gender") +
  scale_color_manual(values = c("F" = "pink", "M" = "lightblue"))  # Customize line colors


######## Age distribution
# Gymnastics average age of participants over the years
gym_avg_age_year <- gymnastics %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age), 
            Total_Participants = n(),              
            .groups = 'drop') 

# Visual
ggplot(gym_avg_age_year, aes(x = Year, y = Average_Age, color = Sex, group = Sex)) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  theme_minimal() +
  labs(title = "Average Age of Gymnastics Participants Over the Years",
       x = "Year",
       y = "Average Age",
       color = 'Gender')

# Average age of gym medalists over the years
gym_age_medalists <- gymnastics %>%
  filter(Medal != "None") %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age), 
            Total_Participants = n(),              
            .groups = 'drop') 

# Visual
ggplot(gym_age_medalists, aes(x = Year, y = Average_Age, color = Sex, group = Sex)) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  theme_minimal() +
  labs(title = "Average Age of Gymnastics Medalists Over the Years",
       x = "Year",
       y = "Average Age",
       color = 'Gender')

# Ski average age of participants over the years
ski_avg_age_year <- ski %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age), 
            Total_Participants = n(),              
            .groups = 'drop') 

# Visual
ggplot(ski_avg_age_year, aes(x = Year, y = Average_Age, color = Sex, group = Sex)) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  theme_minimal() +
  labs(title = "Average Age of Alpine Ski Participants Over the Years",
       x = "Year",
       y = "Average Age",
       color = 'Gender')

# Average age of gym medalists over the years
ski_age_medalists <- ski %>%
  filter(Medal != "None") %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age), 
            Total_Participants = n(),              
            .groups = 'drop') 

# Visual
ggplot(ski_age_medalists, aes(x = Year, y = Average_Age, color = Sex, group = Sex)) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  theme_minimal() +
  labs(title = "Average Age of Alpine Ski Medalists Over the Years",
       x = "Year",
       y = "Average Age",
       color = 'Gender')

######## Height & Weight distribution across the years
### Gymnasts
# Add BMI column
gymnastics <- gymnastics %>%
  mutate(BMI = Weight/(Height/100)^2)

# Avg height/wt/bmi metrics for participants
gym_height_wt <- gymnastics %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age),
            Average_Height = mean(Height), 
            Average_Weight = mean(Weight),
            Average_BMI = mean(BMI),
            .groups = 'drop') 

gym_height_wt_pivot <- gym_height_wt %>%
  pivot_longer(cols=c(Average_Age, Average_Height,Average_Weight,Average_BMI),
               names_to='Metric', values_to='Value')

facet_labels <- c("Average_Age" = "Average Age",
                  "Average_Height" = "Average Height",
                  "Average_Weight" = "Average Weight",
                  "Average_BMI" = "Average BMI")

# Visual
ggplot(gym_height_wt_pivot, aes(x = Year, y = Value, color = Sex, group = interaction(Sex, Metric))) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  facet_wrap(~Metric, scales='free_y', labeller = labeller(Metric = facet_labels)) +
  theme_minimal() +
  labs(title = "Average Age, Height, Weight and BMI of Gymnastics Participants Over the Years",
       x = "Year",
       y = "Value",
       color = 'Gender')

# Avg height/wt/bmi metrics for gym medalists
gym_medalist_height_wt <- gymnastics %>%
  filter(Medal != 'None') %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age),
            Average_Height = mean(Height), 
            Average_Weight = mean(Weight),
            Average_BMI = mean(BMI),
            .groups = 'drop') 

gym_medalist_height_wt_pivot <- gym_medalist_height_wt %>%
  pivot_longer(cols=c(Average_Age, Average_Height,Average_Weight,Average_BMI),
               names_to='Metric', values_to='Value')

facet_labels <- c("Average_Age" = "Average Age",
                  "Average_Height" = "Average Height",
                  "Average_Weight" = "Average Weight",
                  "Average_BMI" = "Average BMI")

# Visual
ggplot(gym_medalist_height_wt_pivot, aes(x = Year, y = Value, color = Sex, group = interaction(Sex, Metric))) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  facet_wrap(~Metric, scales='free_y', labeller = labeller(Metric = facet_labels)) +
  theme_minimal() +
  labs(title = "Average Age, Height, Weight and BMI of Gymnastics Medalists Over the Years",
       x = "Year",
       y = "Value",
       color = 'Gender')

# Reorganise data into age, weight, bmi distribution for all gymnast athletes for histo plot
gym_participants_stat <- gymnastics %>%
  select(Age, Height, Weight, BMI, Sex) %>%
  pivot_longer(cols = -Sex, names_to = "Metric", values_to = "Value")

# Plot
ggplot(gym_participants_stat, aes(x = Value, fill = Sex)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black", position='identity') +  # Histogram with transparency
  facet_wrap(~Metric, scales = "free_x") +  # Separate facets for each metric
  theme_minimal() +
  labs(title = "Frequency Distribution of Age, Height, Weight, and BMI of Gymnasts by Gender",
       x = "Value",
       y = "Frequency",
       fill = "Gender")

# Reorganise data into age, weight, bmi distribution for all gymnast medalists for histo plot
gym_medalists_stat <- gymnastics %>%
  filter(Medal != 'None') %>%
  select(Age, Height, Weight, BMI, Sex) %>%
  pivot_longer(cols = -Sex, names_to = "Metric", values_to = "Value")

# Plot
ggplot(gym_medalists_stat, aes(x = Value, fill = Sex)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black", position='identity') +  # Histogram with transparency
  facet_wrap(~Metric, scales = "free_x") +  # Separate facets for each metric
  theme_minimal() +
  labs(title = "Frequency Distribution of Age, Height, Weight, and BMI of Gymnast Medalists by Gender",
       x = "Value",
       y = "Frequency",
       fill = "Gender")

### Ski athletes
# Add BMI column
ski <- ski %>%
  mutate(BMI = Weight/(Height/100)^2)

# Avg height/wt/bmi metrics for ski participants
ski_height_wt <- ski %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age),
            Average_Height = mean(Height), 
            Average_Weight = mean(Weight),
            Average_BMI = mean(BMI),
            .groups = 'drop') 

ski_height_wt_pivot <- ski_height_wt %>%
  pivot_longer(cols=c(Average_Age, Average_Height,Average_Weight,Average_BMI),
               names_to='Metric', values_to='Value')

facet_labels <- c("Average_Age" = "Average Age",
                  "Average_Height" = "Average Height",
                  "Average_Weight" = "Average Weight",
                  "Average_BMI" = "Average BMI")

# Visual
ggplot(ski_height_wt_pivot, aes(x = Year, y = Value, color = Sex, group = interaction(Sex, Metric))) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  facet_wrap(~Metric, scales='free_y', labeller = labeller(Metric = facet_labels)) +
  theme_minimal() +
  labs(title = "Average Age, Height, Weight and BMI of Alpine Ski Participants Over the Years",
       x = "Year",
       y = "Value",
       color = 'Gender')

# Avg height/wt/bmi metrics for ski medalists
ski_medalist_height_wt <- ski %>%
  filter(Medal != 'None') %>%
  group_by(Year, Sex) %>%
  summarise(Average_Age = mean(Age),
            Average_Height = mean(Height), 
            Average_Weight = mean(Weight),
            Average_BMI = mean(BMI),
            .groups = 'drop') 

ski_medalist_height_wt_pivot <- ski_medalist_height_wt %>%
  pivot_longer(cols=c(Average_Age, Average_Height,Average_Weight,Average_BMI),
               names_to='Metric', values_to='Value')

facet_labels <- c("Average_Age" = 'Average Age',
                  "Average_Height" = "Average Height",
                  "Average_Weight" = "Average Weight",
                  "Average_BMI" = "Average BMI")

# Visual
ggplot(ski_medalist_height_wt_pivot, aes(x = Year, y = Value, color = Sex, group = interaction(Sex, Metric))) +
  geom_line() +  # Line plot for trend
  geom_point() +  # Add points for clarity
  facet_wrap(~Metric, scales='free_y', labeller = labeller(Metric = facet_labels)) +
  theme_minimal() +
  labs(title = "Average Age, Height, Weight and BMI of Alpine Ski Medalists Over the Years",
       x = "Year",
       y = "Value",
       color = 'Gender')

# Reorganise data into age, weight, bmi distribution for all ski athletes for histo plot
ski_participants_stat <- ski %>%
  select(Age, Height, Weight, BMI, Sex) %>%
  pivot_longer(cols = -Sex, names_to = "Metric", values_to = "Value")

# Plot
ggplot(ski_participants_stat, aes(x = Value, fill = Sex)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black", position='identity') +  # Histogram with transparency
  facet_wrap(~Metric, scales = "free_x") +  # Separate facets for each metric
  theme_minimal() +
  labs(title = "Frequency Distribution of Age, Height, Weight, and BMI of Alpine Ski athletes by Gender",
       x = "Value",
       y = "Frequency",
       fill = "Gender")

# Reorganise data into age, weight, bmi distribution for all gymnast medalists for histo plot
ski_medalists_stat <- ski %>%
  filter(Medal != 'None') %>%
  select(Age, Height, Weight, BMI, Sex) %>%
  pivot_longer(cols = -Sex, names_to = "Metric", values_to = "Value")

# Plot
ggplot(ski_medalists_stat, aes(x = Value, fill = Sex)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black", position='identity') +  # Histogram with transparency
  facet_wrap(~Metric, scales = "free_x") +  # Separate facets for each metric
  theme_minimal() +
  labs(title = "Frequency Distribution of Age, Height, Weight, and BMI of Alpine Ski Medalists by Gender",
       x = "Value",
       y = "Frequency",
       fill = "Gender")

############### End of EDA, time to prepare data for model building #############
####### Feature engineering 3 additional predictors: Cum no of participations, years of experience & participation gap (likely correlated)
# Gymnastics
gymnastics_try <- gymnastics %>%
  arrange(Name, Year) %>%  # Sort by Name and Year to ensure correct ordering
  group_by(Name) %>%  # Group by athlete (Name)
  mutate(
    First_year = min(Year),  # Find the first year of participation for each athlete
    Years_exp = Year - First_year,  # Calculate the experience as the difference from the first year
    Previous_Wins = cumsum(ifelse(Medal != "None", 1, 0)) - ifelse(Medal != "None", 1, 0)  # Cumulative wins before the current event
  ) %>%
  select(-First_year) %>%  # Remove the 'First_year' column after calculation
  group_by(Name, Year) %>%  # Group by athlete and year for cumulative participations
  summarise(
    Num_Events = n(),  # Count the number of events per athlete per year
    Years_exp = first(Years_exp),  # Retain Years_exp (it is the same for each year of the same athlete)
    Previous_Wins = first(Previous_Wins),  # Retain the value for "Previous_Wins" in the same year
    .groups = 'drop'
  ) %>%
  arrange(Name, Year) %>%  # Sort by Name and Year after summarising
  group_by(Name) %>%  # Group by athlete to calculate cumulative participations
  mutate(
    Cumulative_Participations = cumsum(Num_Events),  # Calculate cumulative sum of participations
    Participation_Gap = Year - lag(Year)  # Calculate the gap between consecutive participations (Year - lag(Year))
  ) %>%
  ungroup()  # Remove grouping

# Join this data back to the original dataset to retain original columns and add the new features
gymnastics_try <- left_join(gymnastics, gymnastics_try, by = c("Name", "Year"))

# Remove 'Num_Events' and 'Year' 
gymnastics_try <- gymnastics_try %>%
  select(-Num_Events, -Year)

# Replace NA value with 0
gymnastics_try$Participation_Gap <- replace(gymnastics_try$Participation_Gap, is.na(gymnastics_try$Participation_Gap), 0)

# Recategories countries
gymnastics_try <- gymnastics_try %>%
  group_by(region) %>%
  mutate(
    Total_Medals = sum(Medal != "None", na.rm = TRUE),  # Count total medals for each country
    region = ifelse(Total_Medals == 0, "Others", region)  # Categorize countries with no medals as 'Others'
  ) %>%
  ungroup() %>%  # Remove grouping
  select(-Total_Medals)  # Drop the intermediate column if no longer needed

# Ski
ski_try <- ski %>%
  arrange(Name, Year) %>%  # Sort by Name and Year to ensure correct ordering
  group_by(Name) %>%  # Group by athlete (Name)
  mutate(
    First_year = min(Year),  # Find the first year of participation for each athlete
    Years_exp = Year - First_year,  # Calculate the experience as the difference from the first year
    Previous_Wins = cumsum(ifelse(Medal != "None", 1, 0)) - ifelse(Medal != "None", 1, 0)  # Cumulative wins before the current event
  ) %>%
  select(-First_year) %>%  # Remove the 'First_year' column after calculation
  group_by(Name, Year) %>%  # Group by athlete and year for cumulative participations
  summarise(
    Num_Events = n(),  # Count the number of events per athlete per year
    Years_exp = first(Years_exp),  # Retain Years_exp (it is the same for each year of the same athlete)
    Previous_Wins = first(Previous_Wins),  # Retain the value for "Previous_Wins" in the same year
    .groups = 'drop'
  ) %>%
  arrange(Name, Year) %>%  # Sort by Name and Year after summarising
  group_by(Name) %>%  # Group by athlete to calculate cumulative participations
  mutate(
    Cumulative_Participations = cumsum(Num_Events),  # Calculate cumulative sum of participations
    Participation_Gap = Year - lag(Year)  # Calculate the gap between consecutive participations (Year - lag(Year))
  ) %>%
  ungroup()  # Remove grouping

# Now, join this data back to the original dataset to retain original columns and add the new features
ski_try <- left_join(ski, ski_try, by = c("Name", "Year"))

# Remove 'Num_Events' and 'Year' 
ski_try <- ski_try %>%
  select(-Num_Events, -Year)

ski_try$Participation_Gap <- replace(ski_try$Participation_Gap, is.na(ski_try$Participation_Gap), 0)

# Recategories countries
ski_try <- ski_try %>%
  group_by(region) %>%
  mutate(
    Total_Medals = sum(Medal != "None", na.rm = TRUE),  # Count total medals for each country
    region = ifelse(Total_Medals == 0, "Others", region)  # Categorize countries with no medals as 'Others'
  ) %>%
  ungroup() %>%  # Remove grouping
  select(-Total_Medals)  # Drop the intermediate column if no longer needed

### Check for collinearity
# Gymnastics 
cor_gym <- cor(gymnastics_try %>% select_if(is.numeric), use = "complete.obs")
# Print the correlation matrix
print(cor_gym)

library(GGally)
ggpairs(gymnastics_try %>% select_if(is.numeric)) ## potentially need to remove height/weight or both

# Ski
cor_ski <- cor(ski_try %>% select_if(is.numeric), use='complete.obs')
print(cor_ski)

ggpairs(ski_try %>% select_if(is.numeric))
## Some variables are potentially correlated but will keep for now and use pca to confirm

############## PCA
# Remove irrelevant cols for pca analysis
## Gymnasts
gym_pca <- gymnastics_try %>%
  mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  select(-Name, -Season, -Sport)  %>% # Exclude columns not needed for PCA
  select_if(is.numeric) %>% # keep only numeric cols for pca 
  as.data.frame()

# Perform PCA
gym_pca_results <- prcomp(gym_pca, center = TRUE, scale. = TRUE)

# View the PCA components
summary(gym_pca_results) 
gym_pca_results

# View biplot
library(ggfortify)
autoplot(gym_pca_results, data = gym_pca, loadings = TRUE, loadings.label = TRUE, color='lightgray')

#### Ski
ski_pca <- ski_try %>%
  mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  select(-Name, -Season, -Sport)  %>% # Exclude columns not needed for PCA
  select_if(is.numeric) %>% # keep only numeric cols for pca 
  as.data.frame()

# Perform PCA
ski_pca_results <- prcomp(ski_pca, center = TRUE, scale. = TRUE)

# View the PCA components
summary(ski_pca_results)
ski_pca_results

# View biplot
autoplot(ski_pca_results, data = ski_pca, loadings = TRUE, loadings.label = TRUE, color='lightgray')

######## Review of predictors for each dataset before applying classification tree
# Focusing on just countries that have won medals
gymnastics_cluster <- gymnastics_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% gym_medals_by_country$region) %>% # Filter out countries that have won medals
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)
# All athletes from countries that won medals

# Applying factor
gymnastics_cluster$Sex <- as.factor(gymnastics_cluster$Sex)
#gymnastics_cluster$Event <- as.factor(gymnastics_cluster$Event)
gymnastics_cluster$Country <- as.factor(gymnastics_cluster$Country)
gymnastics_cluster$Medal <- as.factor(gymnastics_cluster$Medal)
#gymnastics_cluster <- gymnastics_cluster %>% 
 # select(-Medal)

################ Classification tree
install.packages("rpart")
library(rpart)
library(rpart.plot)

##### Gymnasts data
myoverfittedtree <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster, control=rpart.control(cp=0.0001))
opt_cp<-myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]
opt_cp

classifiedtree <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp, data=gymnastics_cluster,
                        na.action=na.omit)
rpart.plot(classifiedtree, box.palette = "Blues")
summary(classifiedtree)

# Trial 2
gymnastics_cluster2 <- gymnastics_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% gym_medals_by_country$region, # Filter out countries that have won medals
         Medal != 'None') %>% # Athletes that won medals in all countries with medals
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)
# Medalists from countries that won medals

# Applying factor
gymnastics_cluster2$Sex <- as.factor(gymnastics_cluster2$Sex)
#gymnastics_cluster$Event <- as.factor(gymnastics_cluster$Event)
gymnastics_cluster2$Country <- as.factor(gymnastics_cluster2$Country)
gymnastics_cluster2$Medal <- as.factor(gymnastics_cluster2$Medal)
#gymnastics_cluster <- gymnastics_cluster %>% 
# select(-Medal)

# Gymnasts classification tree 2
myoverfittedtree2 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster2, control=rpart.control(cp=0.0001))
opt_cp2<-myoverfittedtree2$cptable[which.min(myoverfittedtree2$cptable[,"xerror"]),"CP"]

classifiedtree2 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp2, data=gymnastics_cluster2,
                        na.action=na.omit)
rpart.plot(classifiedtree2)
summary(classifiedtree2)
opt_cp2

# Trial 3: Gymnasts from top 5 performing countries
gymnastics_cluster3 <- gymnastics_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% total_medals_by_country$region) %>% # Filter out top 5 performing countries 
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)
# all gymnasts from top 5 countries

# Applying factor
gymnastics_cluster3$Sex <- as.factor(gymnastics_cluster3$Sex)
#gymnastics_cluster$Event <- as.factor(gymnastics_cluster$Event)
gymnastics_cluster3$Country <- as.factor(gymnastics_cluster3$Country)
gymnastics_cluster3$Medal <- as.factor(gymnastics_cluster3$Medal)
#gymnastics_cluster <- gymnastics_cluster %>% 
# select(-Medal)

# Gymnasts classification tree 3
myoverfittedtree3 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster3, control=rpart.control(cp=0.0001))
opt_cp3<-myoverfittedtree3$cptable[which.min(myoverfittedtree3$cptable[,"xerror"]),"CP"]

classifiedtree3 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp3, data=gymnastics_cluster3,
                         na.action=na.omit)
rpart.plot(classifiedtree3, box.palette = "Blues")
summary(classifiedtree3)

# Attempt at narrowing the number of predictors but failed to obtain anything meaningful
myoverfittedtree3_2 <- rpart(Medal~Years_exp+Previous_Wins, data=gymnastics_cluster3, control=rpart.control(cp=0.0001))
opt_cp3_2<-myoverfittedtree3_2$cptable[which.min(myoverfittedtree3_2$cptable[,"xerror"]),"CP"]

classifiedtree3_2 <- rpart(Medal~Years_exp+Previous_Wins, cp=opt_cp3_2, data=gymnastics_cluster3,
                         na.action=na.omit)
rpart.plot(classifiedtree3_2, box.palette = "Blues")
summary(classifiedtree3_2)

# Trial 4: Medalists from top 5 performing countries
gymnastics_cluster4 <- gymnastics_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% total_medals_by_country$region, # Filter out top 5 performing countries 
         Medal != 'None') %>% # Athletes that won medals in all countries with medals
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)
# medalists from top 5 countries

# Applying factor
gymnastics_cluster4$Sex <- as.factor(gymnastics_cluster4$Sex)
#gymnastics_cluster$Event <- as.factor(gymnastics_cluster$Event)
gymnastics_cluster4$Country <- as.factor(gymnastics_cluster4$Country)
gymnastics_cluster4$Medal <- as.factor(gymnastics_cluster4$Medal)
#gymnastics_cluster <- gymnastics_cluster %>% 
# select(-Medal)

# Gymnasts classification tree 4
myoverfittedtree4 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster4, control=rpart.control(cp=0.0001))
opt_cp4<-myoverfittedtree4$cptable[which.min(myoverfittedtree4$cptable[,"xerror"]),"CP"]

classifiedtree4 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp4, data=gymnastics_cluster4,
                         na.action=na.omit)
rpart.plot(classifiedtree4)
summary(classifiedtree4)
opt_cp4

# Trial 5: Having all gymnasts to compare difference
gymnastics_cluster5 <- gymnastics_try %>%
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)

# Applying factor
gymnastics_cluster5$Sex <- as.factor(gymnastics_cluster5$Sex)
gymnastics_cluster5$Country <- as.factor(gymnastics_cluster5$Country)
gymnastics_cluster5$Medal <- as.factor(gymnastics_cluster5$Medal)

# Gymnasts classification tree 5
myoverfittedtree5 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster5, control=rpart.control(cp=0.0001))
opt_cp5<-myoverfittedtree5$cptable[which.min(myoverfittedtree5$cptable[,"xerror"]),"CP"]
opt_cp5
classifiedtree5 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp5, data=gymnastics_cluster5,
                         na.action=na.omit)
rpart.plot(classifiedtree5)
summary(classifiedtree5)

##### Ski dataset
### Build tree for all ski athletes in countries that won medals
ski_cluster <- ski_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% ski_medals_by_country$region) %>%
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap, -Cumulative_Participations) %>%
  rename(Country=region)

# Applying factor to ski
ski_cluster$Sex <- as.factor(ski_cluster$Sex)
ski_cluster$Country <- as.factor(ski_cluster$Country)
ski_cluster$Medal <- as.factor(ski_cluster$Medal)

# 1st regression tree for all ski athletes from countries that won medals
myoverfittedtree_ski <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster, control=rpart.control(cp=0.0001))
opt_cp_ski<-myoverfittedtree_ski$cptable[which.min(myoverfittedtree_ski$cptable[,"xerror"]),"CP"]
opt_cp_ski

classifiedtree_ski <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=0.001163467, data=ski_cluster,
                        na.action=na.omit)
rpart.plot(classifiedtree_ski, box.palette = "Blues")
summary(classifiedtree_ski)

# 2nd ski cluster for tree building: only looking at medalists (all)
ski_cluster2 <- ski_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% ski_medals_by_country$region, # Filter out countries that have won medals
         Medal != 'None') %>% # Athletes that won medals in all countries with medals
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)

# Applying factor
ski_cluster2$Sex <- as.factor(ski_cluster2$Sex)
#gymnastics_cluster$Event <- as.factor(gymnastics_cluster$Event)
ski_cluster2$Country <- as.factor(ski_cluster2$Country)
ski_cluster2$Medal <- as.factor(ski_cluster2$Medal)

# Ski classification tree 2
myoverfittedtree_ski2 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster2, control=rpart.control(cp=0.0001))
opt_cp_ski2<-myoverfittedtree_ski2$cptable[which.min(myoverfittedtree_ski2$cptable[,"xerror"]),"CP"]
opt_cp_ski2

classifiedtree_ski2 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp_ski2, data=ski_cluster2,
                         na.action=na.omit)
rpart.plot(classifiedtree_ski2)
summary(classifiedtree_ski2)

# Tree 3: All ski athletes from top 5 performing countries
ski_cluster3 <- ski_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% ski_medals_top5$region) %>% # Filter out top 5 performing countries 
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)

# Applying factor
ski_cluster3$Sex <- as.factor(ski_cluster3$Sex)
ski_cluster3$Country <- as.factor(ski_cluster3$Country)
ski_cluster3$Medal <- as.factor(ski_cluster3$Medal)

# Ski classification tree 3
myoverfittedtree_ski3 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster3, control=rpart.control(cp=0.0001))
opt_cp_ski3<-myoverfittedtree_ski3$cptable[which.min(myoverfittedtree_ski3$cptable[,"xerror"]),"CP"]
opt_cp_ski3
classifiedtree_ski3 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp_ski3, data=ski_cluster3,
                         na.action=na.omit)
rpart.plot(classifiedtree_ski3, box.palette = "Blues")
summary(classifiedtree_ski3)

# Trial 4: Ski Medalists from top 5 performing countries
ski_cluster4 <- ski_try %>%
  #mutate(Win = ifelse(Medal !='None', 1, 0)) %>%
  filter(region %in% ski_medals_top5$region, # Filter out top 5 performing countries 
         Medal != 'None') %>% # Athletes that won medals in all countries with medals
  select(-Name, -Season, -Sport, -BMI, -Participation_Gap) %>%
  rename(Country = region)
# medalists from top 5 countries

# Applying factor
ski_cluster4$Sex <- as.factor(ski_cluster4$Sex)
ski_cluster4$Country <- as.factor(ski_cluster4$Country)
ski_cluster4$Medal <- as.factor(ski_cluster4$Medal)

# Ski classification tree 4
myoverfittedtree_ski4 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster4, control=rpart.control(cp=0.0001))
opt_cp_ski4<-myoverfittedtree_ski4$cptable[which.min(myoverfittedtree_ski4$cptable[,"xerror"]),"CP"]

classifiedtree_ski4 <- rpart(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, cp=opt_cp_ski4, data=ski_cluster4,
                         na.action=na.omit)
rpart.plot(classifiedtree_ski4)
summary(classifiedtree_ski4)
opt_cp_ski4

############# Random forest
install.packages('randomForest')
library(randomForest)
## Gymnasts
gym_forest1 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster,ntree=500, importance=TRUE)
gym_forest1

gym_forest2 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster2,ntree=500, importance=TRUE)
gym_forest2

gym_forest3 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster3,ntree=500, importance=TRUE, do.trace=50)
gym_forest3

gym_forest4 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster4,ntree=500, importance=TRUE)
gym_forest4

gym_forest5 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=gymnastics_cluster5,ntree=500, importance=TRUE)
gym_forest5

# Feature importance
importance(gym_forest5)
importance(gym_forest4)
importance(gym_forest3)
importance(gym_forest2)
importance(gym_forest1)

## Ski
ski_forest1 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster,ntree=500, importance=TRUE)
ski_forest1

ski_forest2 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster2,ntree=500, importance=TRUE)
ski_forest2

ski_forest3 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster3,ntree=500, importance=TRUE, do.trace=50)
ski_forest3

ski_forest4 <- randomForest(Medal~Age+Height+Weight+Years_exp+Previous_Wins+Sex+Country, data=ski_cluster4,ntree=500, importance=TRUE)
ski_forest4

# Feature importance
importance(ski_forest4)
importance(gym_forest4)
importance(ski_forest3)
importance(gym_forest3)
importance(gym_forest2)
importance(ski_forest2)
importance(gym_forest1)
importance(ski_forest1)
