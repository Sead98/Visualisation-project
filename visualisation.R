#1.Calculating win rates for home vs away for teams
library(dplyr)

head(results)
# Add a match result dataframe
results <- results %>%
  mutate(homeOrAwayWin = case_when(
    home_score > away_score ~ "Home Win",
    home_score < away_score ~ "Away Win",
    TRUE ~ "Draw"
  ))

# Calculate win rates for each century
win_rates <- results %>%
  group_by(century, homeOrAwayWin) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Preview the win rates
print(win_rates)

#Visualising the winrates
library(ggplot2)

# Create bar plot

ggplot(win_rates, aes(x = century, y = percent, fill = homeOrAwayWin)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Win Rates by Century",
       x = "Century",
       y = "Percentage",
       fill = "Home or Away win") +
  theme_minimal() +
  scale_fill_manual(values = c("Home Win" = "darkviolet", "Away Win" = "darkgreen", "Draw" = "mediumseagreen"))



#2.Determining the proportion of high scoring game(total no of goals >3) played at home/away

high_scoring <- results %>%
  mutate(high_scoring = (home_score + away_score) > 3) %>%
  group_by(century, neutral, high_scoring) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) #calculation of high scoring games

#plotting it

ggplot(high_scoring %>% filter(high_scoring == TRUE), aes(x = century, y = percent, color = neutral, group = neutral)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Trend of High-Scoring Games by Venue and Century", x = "Century", y = "Percentage", color = "Neutral Venue") +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_minimal()
head(results)



#3.How does the home advantage vary between neutral and non-neutral venues across different centuries?"


library(tidyr)

home_advantage_by_venue <- results %>%
  group_by(century, neutral) %>%
  summarize(
    home_win_percentage = mean(homeOrAwayWin == "Home Win") * 100,
    away_win_percentage = mean(homeOrAwayWin == "Away Win") * 100,
    draw_percentage = mean(homeOrAwayWin == "Draw") * 100,
    .groups = "drop" # Important to avoid dplyr warnings
  )

# Shape the data for heatmap 
home_advantage_long <- home_advantage_by_venue %>%
  pivot_longer(cols = c(home_win_percentage, away_win_percentage, draw_percentage),
               names_to = "outcome", values_to = "percentage") %>%
  mutate(outcome = factor(outcome, levels = c("home_win_percentage", "draw_percentage", "away_win_percentage"),
                          labels = c("Home Win", "Draw", "Away Win")))

# Create heatmap
ggplot(home_advantage_long, aes(x = century, y = outcome, fill = percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(percentage, 1)), color = "black", size = 4) +
  scale_fill_gradient(low = "lightgray", high = "darkblue", name = "Percentage") +
  labs(title = "Win/Draw/Loss Percentages by Century and Venue Neutrality",
       x = "Century", y = "Outcome") +
  theme_minimal() +
  facet_wrap(~ neutral, labeller = as_labeller(c("FALSE" = "Non-Neutral Venue", "TRUE" = "Neutral Venue"))) + # Facet by Neutrality
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid = element_blank(),
        axis.ticks = element_blank())


