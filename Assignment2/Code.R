library(ggplot2)
library(dplyr)
library(lubridate)
getwd()

squirrels<-read.csv("CyberSquirrel-map_data.csv")
squirrels_original<-read.csv("CyberSquirrel-map_data.csv")
squirrels<- squirrels %>%
  filter(Operative=="Squirrel", Country=="United States")



state_region_mapping <- data.frame(
  State.Provence = c("NE", "KS", "IN", "OH", "IA", "IL", "MO", "WI", "MI", "MN", "SD", "ND",
                     "SC", "ME", "MA", "VA", "TN", "LA", "AL", "MS", "FL", "KY", "NC", "GA",
                     "CA", "CO", "TX", "WA", "MT", "ID", "OR", "WY", "UT", "AZ", "AK", "NV",
                     "NY", "NJ", "VT", "PA", "MD", "NH", "CT", "RI", "DE", "DC", "OK", "AR", "NM"),
  Region = c("Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest",
             "Midwest", "Midwest", "Midwest", "Midwest", "Southeast", "Southeast", "Southeast", "Southeast",
             "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast",
             "Western", "Western", "Western", "Western", "Western", "Western", "Western", "Western",
             "Western", "Western", "Western", "Western", "Northeast", "Northeast", "Northeast", "Northeast",
             "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "South Central", "South Central", "South Central")
)


state_region_mapping <- data.frame(
  State.Provence = c("NE", "KS", "IN", "OH", "IA", "IL", "MO", "WI", "MI", "MN", "SD", "ND",
                     "SC", "ME", "MA", "VA", "TN", "LA", "AL", "MS", "FL", "KY", "NC", "GA",
                     "CA", "CO", "TX", "WA", "MT", "ID", "OR", "WY", "UT", "AZ", "AK", "NV",
                     "NY", "NJ", "VT", "PA", "MD", "NH", "CT", "RI", "DE", "DC", "OK", "AR", "NM"),
  Region = c("Northeast", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest",
             "Midwest", "Midwest", "Midwest", "Midwest", "Southeast", "Northeast", "Northeast", "Southeast",
             "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast",
             "West", "West", "West", "West", "West", "West", "West", "West",
             "West", "West", "West", "West", "Northeast", "Northeast", "Northeast", "Northeast",
             "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast",
             "South", "South", "South")
)


# Add the "Region" column to the dataset
squirrels <- squirrels %>%
  left_join(state_region_mapping, by = "State.Provence")




# Add a new "Season" column based on the "Date" column
squirrels <- squirrels %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert "Date" column to Date class
         Month = month(Date))  # Extract the month

# Create the "Season" column based on the month
squirrels <- squirrels %>%
  mutate(Season = ifelse(Month %in% 3:5, "Spring",
                         ifelse(Month %in% 6:8, "Summer",
                                ifelse(Month %in% 9:11, "Autumn", "Winter"))))

  



# Add a new "Season" column based on the "Date" column
squirrels_original <- squirrels_original %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert "Date" column to Date class
         Month = month(Date))  # Extract the month

# Create the "Season" column based on the month
squirrels_original <- squirrels_original %>%
  mutate(Season = ifelse(Month %in% 3:5, "Spring",
                         ifelse(Month %in% 6:8, "Summer",
                                ifelse(Month %in% 9:11, "Autumn", "Winter"))))


#Spring - March to May.
#Summer - June to August.
#Autumn - September to November.
#Winter - December to February.



# Create a bar plot
ggplot(squirrel_data, aes(x = Region, fill = Season)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Squirrel Attacks by Region and Season",
    x = "Region",
    y = "Number of Attacks"
  ) +
  scale_fill_manual(values = c("Spring" = "green", "Summer" = "blue", "Autumn" = "orange", "Winter" = "purple")) +
  theme_minimal()





ggplot(squirrels, aes(x = Region, fill = Season)) +
  geom_bar(col="black"
           #,position="fill"
           ) +
  labs(
    title = "Squirrel Attacks by Region and Season",
    x = "Region",
    y = "Number of Attacks"
  ) +
  scale_fill_manual(values = c("Spring" = "#8bbd78", "Summer" = "#FFFFBF", "Autumn" = "orange", "Winter" = "blue")) 





ggplot(squirrels, aes(x = State.Provence, fill = Season)) +
  geom_bar(col="black"
           #,position="fill"
  ) +
  labs(
    title = "Squirrel Attacks by Region and Season",
    x = "Region",
    y = "Number of Attacks"
  ) +
  scale_fill_manual(values = c("Spring" = "#8bbd78", "Summer" = "#FFFFBF", "Autumn" = "orange", "Winter" = "#0096FF")) 
