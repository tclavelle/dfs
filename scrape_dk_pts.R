################################################################################
## Project: DFS 
## Script purpose:
## By:
## Date:
################################################################################


# Setup -------------------------------------------------------------------
library(tidyverse)
library(fishwatchr)
library(nflfastR)
library(rvest)
library(glue)
library(janitor)

# Advanced Sports Analytics raw data
asa <- read_csv("~/data/nfl/asa_nfl_raw_data.csv")

# define which seasons shall be loaded
# seasons <- 2020
# pbp <- purrr::map_df(seasons, function(x) {
#   readRDS(
#     url(
#       glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
#     )
#   )
# })

# Scrape DK scores and points ---------------------------------------------

# Empty list for data
dk <- list()

for(w in c(1:6)){
  # DK points website
  dk_site <- glue("http://rotoguru1.com/cgi-bin/fyday.pl?week={w}&game=dk&scsv=1")

  # Read websits
  dk_pts <- read_html(dk_site) %>% 
    html_nodes('pre') %>% 
    html_text() %>% 
    read.table(text = ., sep = ";", header = T, stringsAsFactors = F, quote = "")
  
  dk[[w]] <- dk_pts
}

# Flatten list
dk <- bind_rows(dk)

# Convert to tibble
dk_df <- tibble(dk) %>% 
  clean_names()

# Correlations ------------------------------------------------------------

roi <- dk_df %>% 
  select(name, pos, dk_points, dk_salary) %>% 
  mutate(roi = dk_points * 1000 / dk_salary) %>%
  group_by(name, pos) %>% 
  summarize(avg_roi = mean(roi, na.rm = T)) %>% 
  mutate(avg_roi = ifelse(is.infinite(avg_roi), 0, avg_roi),
         pos = toupper(pos)) 
  
# Raincloud plot  
plot_raincloud(roi,
               group_var = pos,
               value_var = avg_roi,
               label_list = list(
                 ylab = 'Average ROI',
                 xlab = 'Position',
                 plot_title = 'Average ROI on DraftKings salary by position'
                 )
)

# Barplots
roi %>% 
  ungroup() %>% 
  filter(pos == 'WR') %>% 
  arrange(desc(avg_roi)) %>% 
  mutate(rank = dense_rank(desc(avg_roi))) %>% 
  filter(rank <= 20) %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(name, avg_roi), y = avg_roi),
           fill = gfw_palette('secondary',1)) +
  coord_flip() +
  labs(y = 'Average ROI',
       x = 'Player',
       title = 'Top 20 Wide Receivers by ROI on DraftKings Salary',
       subtitle = '2020') +
  theme_gfw()

roi %>% 
  group_by(pos) %>% 
  # filter(Pos == 'RB') %>% 
  # arrange(desc(avg_roi)) %>% 
  mutate(rank = dense_rank(desc(avg_roi))) %>% 
  filter(rank <= 10) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(name, avg_roi), y = avg_roi, fill = pos)) +
  coord_flip() +
  labs(y = 'Avgerage ROI',
       x = 'Player',
       title = 'Top 20 players by ROI on DraftKings Salary',
       subtitle = '2020') +
  # facet_grid(~Pos, scales = 'free') +
  theme_gfw()  
  









