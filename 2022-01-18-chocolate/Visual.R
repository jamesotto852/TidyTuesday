library("tidyverse") 
library("ggdensity")

library("sysfonts")
library("showtext")
# font_add("Palatino", "pala.ttf")

showtext_auto() 
font_add_google("Roboto Condensed")
font_add_google("Roboto Slab")
font_add_google("Roboto")


theme_set(theme_bw())
theme_update(panel.grid = element_blank(),
             text = element_text(family = "Roboto"),
             plot.caption = element_text(family = "Roboto Condensed"))

chocolate <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

chocolate <- chocolate |>
  mutate(cocoa_percent = str_extract(cocoa_percent, "^.*(?=\\%)"),
         cocoa_percent = as.numeric(cocoa_percent) / 100)
 
chocolate |>
  ggplot(aes(x = rating, y = cocoa_percent)) +
  # geom_hdr(method = "kde", h = c(.5, .05)) +
  geom_point() +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2))


# Do custom jittering for point layer
# (want to respect hard cutoffs in support)
set.seed(1)
chocolate <- chocolate |>
  mutate(rating = rating + runif(n(), -.125, .125),
         rating = if_else(rating > 4, 4, rating),
         rating = if_else(rating < 1, 1, rating)) |>
  mutate(cocoa_percent = cocoa_percent + runif(n(), -.025, .025),
         cocoa_percent = if_else(cocoa_percent > 1, 1, cocoa_percent),
         cocoa_percent = if_else(cocoa_percent < 0, 0, cocoa_percent)) 

chocolate |>
  ggplot(aes(x = rating, y = cocoa_percent)) +
  # geom_hdr(method = "kde", h = c(.5, .05)) +
  geom_point() +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2))

chocolate |>
  ggplot(aes(x = rating, y = cocoa_percent)) +
  geom_hdr(method = "mvnorm", xlim = c(0, 5)) +
  geom_point(size = .3) +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2))


# By year:
chocolate |>
  mutate(year = as_factor(review_date),
         year = fct_collapse(year,
                             "2006 - 2010" = as.character(2006:2010),
                             "2011 - 2015" = as.character(2011:2015),
                             "2016 - 2021" = as.character(2016:2021))) |>
  ggplot(aes(x = rating, y = cocoa_percent, fill = year)) +
  geom_hdr(method = "mvnorm", xlim = c(0, 5)) +
  geom_point(shape = 21) +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2)) +
  facet_wrap(vars(year))

library("countrycode")
chocolate <- chocolate |> 
  mutate(continent = countrycode(chocolate$country_of_bean_origin, "country.name", "continent")) |>
  mutate(continent = if_else(country_of_bean_origin == "Blend", "Blend", continent)) |>
  filter(!is.na(continent)) |>
  mutate(continent = fct_reorder(continent, rating, .desc = TRUE)) |>
  mutate(year = as_factor(review_date),
         year = fct_collapse(year,
                             "2006 - 2010" = as.character(2006:2010),
                             "2011 - 2015" = as.character(2011:2015),
                             "2016 - 2021" = as.character(2016:2021)))

ggplot(chocolate, aes(x = rating, y = cocoa_percent, fill = continent)) +
  geom_hdr(xlim = c(0, 5)) +
  geom_point(shape = 21, size = .85, stroke = .4) +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2)) +
  scale_fill_brewer(name = "Continent", type = "qual", palette = 2) +
  facet_grid(continent ~ year) +
  ggtitle("Visualizing the relationship between chocolate bars' ratings and cocoa percentage")

chocolate |> 
  ggplot(aes(x = rating, y = cocoa_percent, fill = continent)) +
  geom_hdr(method = "mvnorm", xlim = c(0, 5)) +
  geom_point(shape = 21, size = .85, stroke = .4) +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2)) +
  scale_fill_brewer(name = "Continent", type = "qual", palette = 2) +
  facet_grid(continent ~ year) +
  ggtitle("Visualizing the relationship between chocolate bars' ratings and cocoa percentage")


# Plot to save w/ Twitter resolution
p <- chocolate |> 
  ggplot(aes(x = rating, y = cocoa_percent, fill = continent)) +
  geom_hdr(method = "mvnorm", xlim = c(0, 5)) +
  geom_point(shape = 21, size = .2, stroke = .1) +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2)) +
  scale_fill_brewer(name = "Continent of Origin", type = "qual", palette = 2) +
  facet_grid(continent ~ year) +
  labs(
    title = "Chocolate Bars' Ratings and Cocoa Percentage",
    caption = "Source: Flavors of Cacao | Graphic created by @jamesotto852 using ggdensity"
  ) +
  theme(
    plot.title = element_text(size = 18, margin = margin(b = .06, unit = "cm")),
    plot.caption = element_text(size = 10, margin = margin(t = .15, b = .03, unit = "cm")),
    legend.margin = margin(l = .01, t = .15, b = .15, unit = "cm"),
    legend.key.size = unit(.25, "cm"),
    legend.title = element_text(margin = margin(b = -.075, unit = "cm")),
    legend.text = element_text(margin = margin(l = -.125, r = 0, unit = "cm"), hjust = 0),
    strip.text.x = element_text(margin = margin(t = .075, b = .075, unit = "cm")),
    strip.text.y = element_text(margin = margin(r = .075, l = .075, unit = "cm")),
    axis.ticks = element_line(size = .3),
    axis.ticks.length = unit(.04, "cm"),
    axis.text.x = element_text(margin = margin(t = .04, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = .03, unit = "cm")),
    panel.spacing = unit(.1, "lines")
  )

ggsave(here::here("2022-01-18-chocolate/chocolate.png"), p, width = 1600, height = 900, units = "px")

# Thoughts:
# The mvnorm estimator makes the relationships much easier to see (neg -> pos corr over time, dispersion on cocoa pct. across continents)

# Meta thought: the use of mvnorm hdr doesn't visually imply causality the same way regression lines do (+ is for informative) 



library("gganimate")
p <- chocolate |> 
  # filter(!continent %in% c("Asia", "Oceania")) |>
  ggplot(aes(x = rating, y = cocoa_percent, fill = continent, group = continent)) +
  geom_hdr(method = "mvnorm", xlim = c(0, 5)) +
  geom_point(shape = 21, size = .2, stroke = .1) +
  scale_x_continuous(name = "Rating") +
  scale_y_continuous(name = "Cocoa Percentage", labels = scales::percent_format(accuracy = 2)) +
  scale_fill_brewer(name = "Continent of Origin", type = "qual", palette = 2) +
  # facet_grid(1 ~ continent) +
  facet_wrap(vars(continent), ncol = 3) +
  labs(
    title = "Chocolate Bars' Ratings and Cocoa Percentage",
    caption = "Source: Flavors of Cacao | Graphic created by @jamesotto852 using ggdensity"
  ) +
  theme(
    plot.title = element_text(size = 18, margin = margin(b = .06, unit = "cm")),
    plot.caption = element_text(size = 10, margin = margin(t = .15, b = .03, unit = "cm")),
    legend.margin = margin(l = .01, t = .15, b = .15, unit = "cm"),
    legend.key.size = unit(.25, "cm"),
    legend.title = element_text(margin = margin(b = -.075, unit = "cm")),
    legend.text = element_text(margin = margin(l = -.125, r = 0, unit = "cm"), hjust = 0),
    strip.text.x = element_text(margin = margin(t = .075, b = .075, unit = "cm")),
    strip.text.y = element_text(margin = margin(r = .075, l = .075, unit = "cm")),
    axis.ticks = element_line(size = .3),
    axis.ticks.length = unit(.04, "cm"),
    axis.text.x = element_text(margin = margin(t = .04, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = .03, unit = "cm")),
    panel.spacing = unit(.1, "lines")
  ) +
  transition_states(year)
  




ggsave(here::here("2022-01-18-chocolate/chocolate.png"), p, width = 1200, height = 600, units = "px")


  


