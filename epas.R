library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(fontawesome)
#——————————————————————————————————————#

roster <- fast_scraper_roster(2020)


joined <- load_pbp(2020, qs = TRUE) %>%
  filter(!is.na(receiver_id)) %>%
  left_join(roster, by = c("receiver_id" = "gsis_id"))
  
table_data <- joined %>%
  group_by(receiver, receiver_id, posteam) %>%
  mutate(tgt = sum(complete_pass + incomplete_pass)) %>%
  filter(tgt >= 50) %>%
  filter(position == "TE") %>%
  filter(complete_pass == 1, air_yards < yardline_100, !is.na(xyac_epa)) %>%
  summarize(
    epa_oe = mean(yac_epa - xyac_epa),
    epa = mean(epa),
    rec = dplyr::n(),
    headshot_url = unique(headshot_url)
  ) %>% 
  ungroup() %>%
  select(receiver, headshot_url, posteam, epa, epa_oe) %>% 
  arrange(-epa) %>%
  head(n = 15)

table_data %>%
  gt() %>%
  cols_align(align = "center",
             columns = vars(epa)) %>%
  tab_options(
    data_row.padding = px(1)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(TRUE)
  ) %>%
  tab_header(
    title = md("NFL TEs by EPA/Play"),
    subtitle = "Using nflfastR"
  ) %>%
  cols_label(
    receiver = "Name",
    headshot_url = "",
    posteam = "TM",
    epa = "EPA",
    epa_oe = "EPA over expectation"
  ) %>%
  opt_all_caps() %>%
  tab_options(
    table.background.color = "white",
    column_labels.background.color = "purple",
    #row.striping.background_color = "#e0e0e0"
  ) %>%
  opt_row_striping() %>%
  opt_table_font(
    font = list(
      google_font("Calibri"),
      default_fonts()
    )
  ) %>%
  tab_source_note(
    source_note = gt::html(
      htmltools::tags$a(
        href = "https://twitter.com/_pranavrajaram", 
        target = "_blank", 
        "@_pranavrajaram"
      ) %>% 
        as.character()
    )
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(headshot_url)),
    fn = function(x){
      gt::web_image(x)
    }
  ) %>%
  data_color(
    columns = vars(epa),
    colors = scales::col_numeric(
      palette = c("white", "#00b200"),
      domain = NULL
    )
  )
