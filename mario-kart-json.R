library(readr)

colour1 <- "grey"
colour2 <- "#1B9E77"
colour3 <- "#D95F02"
colour4 <- "#7570B3"
colour5 <- "#E7298A"

colors_df <- tribble(
  ~track, ~color,
  "Luigi Raceway", colour1,
  "Moo Moo Farm", colour1,
  "Koopa Troopa Beach", colour1,
  "Kalimari Desert", colour1,
  "Toad's Turnpike", colour3,
  "Frappe Snowland", colour1,
  "Choco Mountain", colour4,
  "Mario Raceway", colour1,
  "Wario Stadium", colour5,
  "Sherbet Land", colour1,
  "Royal Raceway", colour1,
  "Bowser's Castle", colour1,
  "D.K.'s Jungle Parkway", colour1,
  "Yoshi Valley", colour1,
  "Banshee Boardwalk", colour1,
  "Rainbow Road", colour2
)

X <- records_p %>%
  ungroup %>%
  select(date, o_improvement, track, shortcut) %>%
  mutate(
    shortcut = case_when(
      shortcut == "No" ~ "No Shortcut",
      shortcut == "Yes" ~ "Shortcut"
    )
  )

X <- left_join(X, colors_df)

X %>%
  write_csv("mario-kart.csv")

# https://help.data.world/hc/en-us/articles/360026625353-Using-the-Vega-Lite-editor-in-Chart-Builder
# https://stackoverflow.com/questions/55134128/setting-a-maximum-axis-value-in-vega-lite#55134190
# https://stackoverflow.com/questions/57244390/has-anyone-figured-out-a-workaround-to-add-a-subtitle-to-an-altair-generated-cha#59782387
# https://vega.github.io/vega-lite/docs/scale.html#example-clipping-or-removing-unwanted-data-points
# https://vega.github.io/vega-lite/docs/size.html
# http://vda-lab.github.io/2019/12/vegalite
# https://vega.github.io/vega-lite/docs/predicate.html#one-of-predicate
