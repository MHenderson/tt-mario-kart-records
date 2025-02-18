library(targets)

tar_option_set(
  packages = c("dplyr", "ggplot2", "ragg", "readr", "scales", "tibble") 
)

list(
  tar_target(
       name = records,
    command = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
  ),
  tar_target(
       name = drivers,
    command = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')
  ),
  tar_target(
       name = records_p,
    command = {
      records |>
        group_by(track, type, system_played, shortcut) |>
        mutate(
          original_record = first(time),
          previous_record = lag(time),
              improvement = abs(time - original_record),
            p_improvement = improvement/previous_record,
            o_improvement = improvement/original_record
        ) |>
        filter(system_played == "PAL") |>
        filter(type == "Three Lap")
    }
  ),
  tar_target(
       name = main_plot,
    command = {

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

      records_p |>
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
                  plot.margin = margin(20, 10, 20, 10),
             panel.background = element_rect(fill = background_colour, colour = NA),
              plot.background = element_rect(fill = background_colour, colour = NA),
             panel.grid.major = element_line(colour = "grey"),
             panel.grid.minor = element_line(colour = "grey"),
                   plot.title = element_text(colour = text_colour, size = 30, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
                 plot.caption = element_text(colour = text_colour, size = 10, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
                plot.subtitle = element_text(colour = text_colour, size = 20, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
                 legend.title = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1),
                  legend.text = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1),
                   strip.text = element_text(colour = text_colour, size = base_size, hjust = 0, family = font1, margin = margin(5, 0, 5, 0)),
                 axis.title.y = element_text(colour = text_colour, size = base_size, hjust = 1, family = font1, margin = unit(c(0, 5, 0, 0), "mm")),
                    axis.text = element_text(colour = text_colour, size = base_size, hjust = 1, family = font1, margin = margin(5, 0, 5, 0))
      )
    } 
  ),
  tar_target(
       name = save_main_plot,
    command = {
      agg_png("plot/mario-kart.png", res = 300, height = 9, width = 9, units = "in")
      print(main_plot)
      dev.off()
      "plot/mario-kart.png"
    }, 
     format = "file"
  )
)
