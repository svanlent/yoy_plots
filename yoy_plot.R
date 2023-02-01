# Load packages
library(tidyverse)
library(stringr)
library(mlbplotR)
library(ggrepel)
library(prismatic)

# set custom theme for plot, but this time change up our color scheme

theme_scott <- function () {

    theme_minimal(base_size=9) %+replace%
    theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
            fill = "#F9F9F9", color = "#F9F9F9"
        ),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
       plot.margin = margin(10, 10, 20, 10) 
    )

}

# read in data

mlb = read_csv("https://raw.githubusercontent.com/svanlent/yoy_plots/main/pitching_mlb_90_22.csv")

df_sum_mlb = mlb %>% 
  group_by(player_id, player_full_name, season) %>% 
  summarize(
    bf = sum(batters_faced, na.rm = TRUE),
    k_p = sum(strike_outs, na.rm = TRUE) / sum(batters_faced, na.rm = TRUE),
    bb_p = sum(base_on_balls, na.rm = TRUE) / sum(batters_faced, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(k_minus_bb = k_p - bb_p)

df_sum_mlb = df_sum_mlb
  group_by(player_id) %>%
  arrange(season) %>% 
  mutate(
    lag_k_bb_p = lag(k_minus_bb), 
    lag_bf = lag(bf), 
    diff_lag = k_minus_bb - lag_k_bb_p  
    ) %>% 
  ungroup() %>%
  arrange(desc(diff_lag)) %>% 
  filter(bf >= 300 & lag_bf >= 300) %>%
  mutate(player_year = paste0(player_full_name, " (", season, ")"))

players_to_plot = c(
    "Lucas Giolito (2019)", 
    "Jake Arrieta (2014)", 
    "Justin Verlander (2018)", 
    "James Kaprielian (2022)", 
    "Andrew Heaney (2022)",
    "Pedro Martinez (1999)",
    "Eric Gagne (2003)",
    "Mariano Rivera (1996)",
    "Brad Lidge (2004)",
    "Kerry Wood (2000)",
    "Rich Harden (2010)",
    "Roy Halladay (2001)",
    "Trevor Rosenthal (2014)",
    "Randy Johnson (2001)",
    "Max Scherzer (2019)",
    "Pedro Martinez (2000)",
    "Chris Sale (2019)"
)

p = df_sum_mlb %>% 
   ggplot(aes(x = lag_k_bb_p, y = k_minus_bb, 
      label = ifelse(
         player_year %in% players_to_plot, player_year, "")
      )
   ) + 
   geom_point(aes(fill = diff_lag, 
      color = after_scale(clr_darken(fill, 0.3))
      ), 
      shape = 21, 
      alpha = .75,
      size = 2
   ) +
   geom_text_repel(size = 3, 
      alpha = .7, 
      point.padding = 0.1, 
      nudge_y = .013, 
      nudge_x = -.006, 
      max.overlaps = 40
   ) +
   scale_x_continuous(limits = c(0, .35), 
      labels = scales::percent, breaks = seq(0, .35, .05)) + 
   scale_y_continuous(limits = c(0, .35), 
      labels = scales::percent, breaks = seq(0, .35, .05)) +
   theme_scott() +
   theme(legend.position = 'top', 
      plot.title.position = 'plot', 
      plot.title = element_text(face = 'bold', size = 12, color = "#CB4C4E", hjust = 0.5), 
      plot.subtitle = element_text(size = 9, hjust = 0.5),        
      legend.title=element_text(size=7), 
      plot.margin = margin(25, 25, 10, 25)) +
   scale_fill_distiller(
       palette = 7, 
       type = "div", 
       direction = -1, 
       breaks = 0, 
       labels = ""
   ) +
   guides(fill = guide_colorbar(
    barwidth = 3,
    barheight = .15,
    default.unit="inch", 
    title.position = 'bottom',
    title.hjust = .5,
    title.vjust = 7.5)) +
   labs(x = "K-BB% in t-1", 
      y = "K-BB% in t", 
      fill = "K-BB% Decreased YoY                        K-BB% Increased YoY",
      title = "Largest Increases & Decreases in Strikeout Minus Walk Percentage (Pitchers)", 
      subtitle = "Year-over-Year Change in K-BB Percentage (1990-2022) | Minimum 300 BF In Both Seasons")