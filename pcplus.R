library(baseballr)
library(dplyr)
library(gt)
library(gtExtras)
library(mlbplotR)
library(ggtext)

pitch_leaders <- fg_pitcher_leaders(startseason = "2023", endseason = "2023") 

game_logs <- data.frame()

for (id in unique(pitch_leaders$playerid)) {
  game_logs_indiv <- fg_pitcher_game_logs(id, 2023) %>% mutate(num = row_number()) %>% select(name = PlayerName, id = playerid, team = Team, num, IP, WPA)
  game_logs <- rbind(game_logs, game_logs_indiv)
}

stats <- game_logs %>%
  filter(!(IP == 0)) %>%
  group_by(team, id) %>%
  summarize(name = first(name), games = n(), ip = sum(IP), avg_wpa = mean(WPA), stdev = sd(WPA)) %>%
  filter(games != 1)

team_stats <- stats %>%
  group_by(team) %>%
  summarize(team_mean = weighted.mean(avg_wpa, ip), team_stdev = sqrt(sum(ip * (avg_wpa - weighted.mean(avg_wpa, ip))^2)))

league_pc <- mean(team_stats$team_stdev)

team_stats <- team_stats %>%
  mutate(WPAscaled = round(scale(team_mean) * 100), PCplus = round(league_pc/team_stdev * 100)) %>%
  select(-team_mean, -team_stdev) %>%
  arrange(-PCplus)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>baseballR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

nice_table <- team_stats %>% gt() %>%
  gt_fmt_mlb_logo(columns = "team", height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(team, WPAscaled, PCplus)
  ) %>%
  gt_hulk_col_numeric(c(WPAscaled, PCplus)) %>%
  cols_label(
    team = md("**TEAM**"),
    WPAscaled = md("**zWPA**"),
    PCplus = md("**PC+**")
  ) %>%
  tab_header(
    title = "2023 MLB PC+ (Pitching Consistency+) By Team",
    subtitle = md("*zWPA: Scaled Weighted Average **Win Probability Added***")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = PCplus
    )
  )

gtsave(nice_table, "nice_table.png", vwidth = 1000, vheight = 2500, zoom = 1)

plot <- team_stats %>%
  ggplot(aes(x = WPAscaled, y = PCplus)) + 
  geom_hline(yintercept = 100, color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  geom_mlb_logos(aes(team_abbr = team), width = 0.05) + 
  labs(x = "zWPA", 
       y = "PC+",
       title = "2023 MLB Pitching Performance vs Consistency",
       caption = "Data from **baseballR** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme(plot.title = element_markdown(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"), plot.caption = element_markdown(hjust = 0.5))

ggsave("plot.png", plot, width = 10, height = 6)