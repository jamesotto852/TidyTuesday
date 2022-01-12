library("here")
library("tidyverse")
library("gganimate")

# For plotting spatial data
library("maps")
library("sf")
library("ggspatial")

# For manual tweening
library("zoo")

# Load in data
colony <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

# Fixing small data issue
colony <- filter(colony, months != "2019")

# Loading in sf for plot
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Fix implicitly missing data (not all states included in bee colony data)
states_in_data <- colony |>
  pluck("state") |>
  unique() |>
  tolower()

states_missing <- states |>
  filter(!ID %in% states_in_data) |>
  pluck("ID") |>
  unique()

colony <- colony |>
  mutate(state = tolower(state)) |>
  add_row(state = states_missing, year = 2015, months = "January-March", .before = 1) |>
  complete(state, nesting(year, months))

# Combine year and date into single numeric identifier for animation
f_find_quarter <- function(x) {
  if (length(x) > 1) return(map_dbl(x, f_find_quarter))
  
  if (x == "January-March") return(0/4)
  if (x == "April-June") return(1/4)
  if (x == "July-September") return(2/4)
  if (x == "October-December") return(3/4)
}


colony <- colony |>
  mutate(time = year + f_find_quarter(months))

# Function converting encoded time -> title for animation
time_to_title <- function(x) {
  if (length(x) > 1) return(map_chr(x, quarter_to_title))
  
  x <- as.numeric(x)
  x <- floor(x * 4) / 4
  
  y <- floor(x)
  
  if (x %% 1 == 0/4) q <- "January - March"
  if (x %% 1 == 1/4) q <- "April - June"
  if (x %% 1 == 2/4) q <- "July - September"
  if (x %% 1 == 3/4) q <- "October - December"
  
  str_c(y, ", ", q)
}

# Helper function, custom tweening for transition_manual
source(here("2022-01-11-colony/tween_group.R"))

anim <- colony |>
  mutate(state = tolower(state)) |>
  select(state, time, colony_lost_pct) |>
  nest(data = c(time, colony_lost_pct)) |>
  rowwise() |>
  mutate(interp = tween_group(data, k = 15, p = 14)) |>
  unnest(cols = c(interp)) |>
  select(-data) |>
  group_by(state, time) |>
  summarize(colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE) / 100) |>
  left_join(states, by = c("state" = "ID")) |>
  ungroup() |>
  ggplot(aes(geometry = geom, group = state)) +
  geom_sf(aes(fill = colony_lost_pct)) +
  coord_sf() +
  scale_fill_continuous(high = "#3A3637", low = "#FCD615", na.value="grey40",
                        type = "gradient", labels = scales::percent_format(accuracy = 1), 
                        name = NULL) +
  theme_void() +
  theme(plot.margin = margin(.5, .7, .5, .7, unit = "cm"),
        plot.title = element_text(size = 20, hjust = 0, margin = margin(b = .1, l = .25, unit = "cm")),
        strip.text = element_text(size = 16),
        legend.margin = margin(t = .5, unit = "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.key.width = unit(2, "cm"),
        legend.position = "bottom") +
  ggtitle("Percentage of honey bee colonies lost: {time_to_title(current_frame)}") +
  transition_manual(time)
  

# Warning: this takes a while
animate(anim, fps = 30, duration = 26, width = 1500, height = 650) 
# animate(anim, fps = 30, duration = 26, width = 1500, height = 650, start_pause = 15, end_pause = 30) 
anim_save(here::here("2022-01-11-colony/colony.gif"))

## Converting from gif to .mp4
# need ffmpeg (sudo apt install ffmpeg)
system(
  'cd 2022-01-11-colony
  ffmpeg -i colony.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" colony.mp4'
)

  
  