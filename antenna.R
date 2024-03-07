library(dplyr)
library(ggplot2)

# antenna search
data$antenna <- ifelse(grepl("antenna", data$Summary, ignore.case = TRUE) | grepl("antenna", data$Narrative, ignore.case = TRUE), 1, 0)

data_grouped <- data %>%
  group_by(EventDecade) %>%
  summarise(Antennas = sum(antenna), Total = n()) %>%
  mutate(Percentage = (Antennas / Total) * 100)

# plot
ggplot(data_grouped, aes(x = EventDecade, y = Percentage)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Percentage of Antennas Mentioned by Event Decade", x = "Event Decade", y = "Percentage")
