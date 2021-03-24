library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggthemes)
library(gt)

seasons <- 2020:2020
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

#——————————————————————————————————————————————————————————————————————————#
pbp <- pbp %>%
  filter(week <= 17)


season_stats <- nflfastR::calculate_player_stats(pbp = pbp)

roster <- fast_scraper_roster(2020)

decoded_stats <- season_stats %>%
  decode_player_ids()

joined_stats <- decoded_stats %>%
  filter(!is.na(player_id)) %>%
  left_join(roster, by = c("player_id" = "gsis_id"))

filtered_joined_stats <- joined_stats %>%
  filter(targets >= 50) %>%
  arrange(desc(receiving_yards)) %>%
  head(n = 25)

filtered_joined_stats %>%
  ggplot(aes(x = receiving_yards,
             y = receiving_air_yards)) +
  geom_image(aes(image = headshot_url), size = filtered_joined_stats$fantasy_points_ppr/4000, asp = 16/9) +
  theme_fivethirtyeight() +
  labs(title = "NFL Receiving Yards vs. Air Yards 2020",
       subtitle = "Using nflfastR",
       x = "Receiving Yards",
       y = "Air Yards",
       caption = "@_pranavrajaram") +
  theme(axis.title = element_text()) 
