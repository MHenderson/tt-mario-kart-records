# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md
# https://stackoverflow.com/questions/40484090/rotate-switched-facet-labels-in-ggplot2-facet-grid
# https://stackoverflow.com/questions/3261597/can-i-change-the-position-of-the-strip-label-in-ggplot-from-the-top-to-the-botto
# https://ggrepel.slowkow.com/reference/geom_text_repel.html
# https://stackoverflow.com/questions/64167737/highlight-a-line-in-ggplot-with-multiple-lines
# https://stackoverflow.com/questions/25685185/limit-ggplot2-axes-without-removing-data-outside-limits-zoom
# https://thomasadventure.blog/posts/ggplot2-percentage-scale/
# https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/
# https://stackoverflow.com/questions/9131916/increase-space-between-axis-title-and-axis-text-in-ggplot2-version-0-9-0
# https://statisticsglobe.com/change-legend-title-ggplot-r
# https://stackoverflow.com/questions/28872875/ggplot2-applying-width-of-line-to-the-legend-key#28889508
# https://stackoverflow.com/questions/41105759/aligning-title-subtitle-and-caption-for-horizontal-ggplot-barchart#57792599
library(dplyr)
library(ggplot2)
library(here)
library(ragg)
library(readr)
library(scales)

if(!file.exists('records.csv')) {
  download.file('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv', destfile = 'records.csv')
}

if(!file.exists('drivers.csv')) {
  download.file('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv', destfile = 'drivers.csv')
}

records <- read_csv('records.csv')
drivers <- read_csv('drivers.csv')

records_p <- records %>%
  group_by(track, type, system_played, shortcut) %>%
  mutate(
    original_record = first(time),
    previous_record = lag(time),
    improvement = abs(time - original_record),
    p_improvement = improvement/previous_record,
    o_improvement = improvement/original_record
  )

colour1 <- "grey"
colour2 <- "#1B9E77"
colour3 <- "#D95F02"
colour4 <- "#7570B3"
colour5 <- "#E7298A"

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

shortcut_labs <- c("Shortcut", "No Shortcut")
names(shortcut_labs) <- c("Yes", "No")

base_size <- 12
font1 <- "Roboto Condensed"
background_colour <- "#e4e2e0"
text_colour <- "#212121"

p <- records_p %>%
  filter(system_played == "PAL") %>%
  filter(type == "Three Lap") %>%
  ggplot() +
  geom_step(aes(date, o_improvement, colour = track), size = .8, alpha = .5) +
  facet_wrap(~shortcut, labeller = labeller(shortcut = shortcut_labs)) +
  theme_minimal() +
  scale_color_manual(values = colours_) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    legend.position = "right"
  ) +
  xlim(as.Date("1995-01-01"), as.Date("2022-01-01")) +
  coord_cartesian(ylim = c(0, .4)) +
  guides(
    colour = guide_legend(ncol = 1, override.aes = list(size = 2))
  ) +
  labs(
    title = "Mario Kart World Records",
    subtitle = "1997 - 2021 (PAL systems, 3 laps)",
    caption = "Data: https://mkwrs.com/ | Graphic: Matthew Henderson",
    x = "",
    y = "Percentage improvement",
    colour = ""
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(
    plot.margin       = margin(20, 10, 20, 10),
    panel.background  = element_rect(fill = background_colour, colour = NA),
    plot.background   = element_rect(fill = background_colour, colour = NA),
    panel.grid.major  = element_line(colour = "grey"),
    panel.grid.minor  = element_line(colour = "grey"),
    plot.title        = element_text(colour = text_colour, size = 30, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
    plot.caption      = element_text(colour = text_colour, size = 10, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
    plot.subtitle     = element_text(colour = text_colour, size = 20, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
    legend.title      = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1),
    legend.text       = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1),
    strip.text        = element_text(colour = text_colour, size = base_size, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
    axis.title.y      = element_text(colour = text_colour, size = base_size, hjust = 1, family = font1, margin = unit(c(0, 5, 0, 0), "mm")),
    axis.text         = element_text(colour = text_colour, size = base_size, hjust = 1, family = font1, margin = margin(5, 0, 5, 0))
  )

agg_png(here("mario-kart.png"), res = 300, height = 9, width = 9, units = "in")
print(p)

dev.off()
