library("tidyverse")
library("ggdensity")

data <- tidytuesdayR::tt_load('2022-01-25')

df <- data$ratings |>
  left_join(data$details, by = c("id" = "id"))

top_categories <- df |>
  group_by(boardgamecategory) |>
  summarize(n = n()) |>
  filter(!is.na(boardgamecategory)) |>
  mutate(category = str_extract_all(boardgamecategory, "(?<=')[^,]*(?=')")) |>
  unnest(category) |>
  select(-boardgamecategory) |>
  group_by(category) |>
  summarize(n = sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 5)

p <- df |> 
  filter(maxplayers < 20) |>
  filter(playingtime < 1000) |> # Wargame category has a ton of very high values...
  mutate(category = str_extract_all(boardgamecategory, "(?<=')[^,]*(?=')")) |>
  unnest(category) |>
  select(-boardgamecategory) |>
  filter(category %in% top_categories$category) |>
  left_join(top_categories, by = "category") |>
  # mutate(label = paste0(category, " (", scales::comma(n), ")"),
  #        label = fct_reorder(label, n, .desc = TRUE)) |>
  mutate(label = paste0(category),
         label = fct_reorder(label, playingtime, mean, .desc = TRUE)) |>
  mutate(playingtime = playingtime / 60) |>
  mutate(avg_players = (minplayers + maxplayers)/2) |>
  ggplot(aes(x = avg_players, y = playingtime, fill = label)) +
  geom_hdr(adjust = 2.5) +
  scale_fill_brewer(type = "qual", palette = 2, guide = NULL) +
  # geom_jitter(height = 30, width = 1, size = .1, alpha = .5) +
  facet_wrap(vars(label), ncol = 5) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_y_continuous(breaks = 0:8 * 2) +
  coord_cartesian(ylim = c(0, 14), expand = FALSE) +
  # theme_bw(18) +
  theme_bw(5) +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    # legend.key.size = unit(.75, "cm")
    legend.key.size = unit(.25, "cm"),
    plot.caption = element_text(size = 3),
    plot.margin = margin(c(.25, .25, .1, .25), unit = "cm")
  ) +
  labs(
    x = "Average no. of players",
    y = "Average play time (Hours)",
    title = "Top 5 Board Game Types: Play Time vs. Number of Players",
    caption = "Source: BoardGameGeek | Graphic created by @jamesotto852 using ggdensity"
  )

ggsave(here::here("2022-01-25-boardgames/boardgames.png"), p, width = 1600, height = 900, units = "px")
