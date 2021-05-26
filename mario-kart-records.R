# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md
# https://stackoverflow.com/questions/40484090/rotate-switched-facet-labels-in-ggplot2-facet-grid
# https://stackoverflow.com/questions/3261597/can-i-change-the-position-of-the-strip-label-in-ggplot-from-the-top-to-the-botto
# https://ggrepel.slowkow.com/reference/geom_text_repel.html
# https://stackoverflow.com/questions/64167737/highlight-a-line-in-ggplot-with-multiple-lines
# https://stackoverflow.com/questions/25685185/limit-ggplot2-axes-without-removing-data-outside-limits-zoom
library(dplyr)
library(ggplot2)
library(readr)

records <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

records_p <- records %>%
  group_by(track, type, system_played, shortcut) %>%
  mutate(
    original_record = first(time),
    previous_record = lag(time),
    improvement = abs(time - original_record),
    p_improvement = 100*(improvement/previous_record),
    o_improvement = 100*(improvement/original_record)
  )

colour1 <- "grey80"
colour2 <- "red"
colour3 <- "green"
colour4 <- "blue"
colour5 <- "yellow"

colours_ <- c("Luigi Raceway" = colour1,
             "Moo Moo Farm" = colour1,
             "Koopa Troopa Beach" = colour1,
             "Kalimari Desert" = colour1,
             "Toad's Turnpike" = colour3,
             "Frappe Snowland" = colour1,
             "Choco Mountain" = colour4,
             "Mario Raceway" = colour1,
             "Wario Stadium" = colour5,
             "Sherbet Land" = colour1,
             "Royal Raceway" = colour1,
             "Bowser's Castle" = colour1,
             "D.K.'s Jungle Parkway" = colour1,
             "Yoshi Valley" = colour1,
             "Banshee Boardwalk" = colour1,
             "Rainbow Road" = colour2)

records_p %>%
  filter(system_played == "PAL") %>%
  filter(type == "Three Lap") %>%
  ggplot() +
  geom_step(aes(date, o_improvement, colour = track), size = .5) +
  facet_wrap(~shortcut) +
  theme_minimal() +
  scale_color_manual(values = colours_) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    legend.position = "right"
  ) +
  xlim(as.Date("1995-01-01"), as.Date("2022-01-01")) +
  coord_cartesian(ylim = c(0, 40))
  guides(colour = guide_legend(ncol = 1))

ggsave("mario-kart.png", width = 10, height = 10)
