# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md
# https://stackoverflow.com/questions/40484090/rotate-switched-facet-labels-in-ggplot2-facet-grid
# https://stackoverflow.com/questions/3261597/can-i-change-the-position-of-the-strip-label-in-ggplot-from-the-top-to-the-botto
# https://ggrepel.slowkow.com/reference/geom_text_repel.html
library(dplyr)
library(ggplot2)
library(readr)

records <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

player_codes <- tibble(player = unique(records$player)) %>%
  mutate(
    player_code = c("SA", "BO", "GG", "RG", "LA", "PQ", "PV", "PE", "ZW", "LY", "MJ", "MR", "DA", "MK", "TY", "ZO", "VA", "PH", "WO", "IV", "JO", "KA", "AG", "AN", "DL", "WE", "JW", "MB", "BM", "HA", "TJ", "MY", "MC", "MS", "PO", "SG", "SI", "ET", "SM", "JD", "KY", "JY", "IM", "JE", "DB", "LU", "AC", "LP", "GI", "AB", "LB", "LO", "GL", "MP", "MA", "EF", "RA", "ML", "JA", "DK", "MF", "TE", "TR", "SK", "JB")
  )

records <- left_join(records, player_codes)

records_p <- records %>%
  group_by(track, type, system_played, shortcut) %>%
  mutate(
    original_record = first(time),
    previous_record = lag(time),
    improvement = abs(time - original_record),
    p_improvement = 100*(improvement/previous_record),
    o_improvement = 100*(improvement/original_record)
  )

records_p %>%
  filter(type == "Three Lap", system_played == "PAL") %>%
  ggplot() +
  geom_step(aes(date, o_improvement, colour = track), size = 1, alpha = .3) +
  #geom_point(aes(date, o_improvement, colour = player), alpha = 0.5) +
  facet_wrap(~shortcut, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text.y.right = element_text(angle = 0),
    legend.position = "right"
  ) +
  xlim(as.Date("1995-01-01"), as.Date("2022-01-01")) +
 # ylim(0, 100) +
  guides(colour = guide_legend(ncol = 1))

ggsave("mario-kart.png", width = 10, height = 10)
