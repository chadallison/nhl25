NHL 2025
================

------------------------------------------------------------------------

``` r
tic()

start_date = as.Date("2024-10-01")
end_date = as.Date("2025-04-15")
games = list()
current_date = start_date

while (current_date <= end_date) {
  url = paste0("https://api-web.nhle.com/v1/schedule/", format(current_date, "%Y-%m-%d"))
  response = GET(url)
  data = content(response, "parsed")

  if (status_code(response) == 200) {
    game_week = data$gameWeek
    for (game_day in game_week) {
      for (game in game_day$games) {
        if (game$gameType == 2) {
          utc_time = game$startTimeUTC
          if (!is.null(utc_time)) {
            utc_time_obj = with_tz(ymd_hms(utc_time), tzone = "UTC")
            game_date = format(with_tz(utc_time_obj, tzone = "US/Eastern"), "%Y-%m-%d")
          } else {
            game_date = NA
          }

          home_score = as.integer(game$homeTeam$score)
          away_score = as.integer(game$awayTeam$score)

          games = append(games, list(list(
            game_id = game$id,
            game_date = game_date,
            home_team = game$homeTeam$abbrev,
            away_team = game$awayTeam$abbrev,
            home_score = home_score,
            away_score = away_score,
            total_score = home_score + away_score,
            venue = game$venue$default
          )))
  }}}}
  current_date = current_date + 1
}

end_games_raw = bind_rows(games)

end_games = end_games_raw |>
  filter(total_score > 0) |>
  distinct() |>
  mutate(game_date = as_date(game_date))

toc()
```

    ## 151.06 sec elapsed

``` r
end_games = end_games |>
  mutate(win_team = if_else(home_score > away_score, home_team, away_team),
         lose_team = if_else(home_score > away_score, away_team, home_team))

win_counts = end_games |>
  count(win_team, name = "wins")

lose_counts = end_games |>
  count(lose_team, name = "losses")

team_records = left_join(win_counts, lose_counts, by = c("win_team" = "lose_team"))

team_records = team_records |>
  select(win_team, wins, losses) |>
  rename(team = win_team) |>
  arrange(team) |>
  mutate(win_pct = round(wins / (wins + losses) * 100, 2))

all_teams = team_records$team
```

``` r
get_team_scores = function(tm) {
  home = end_games |>
    filter(home_team == tm) |>
    select(game_date, team = home_team, score = home_score, other_score = away_score)
  
  away = end_games |>
    filter(away_team == tm) |>
    select(game_date, team = away_team, score = away_score, other_score = home_score)
  
  res = rbind(home, away) |>
    arrange(game_date)
  
  return(res)
}

all_team_scores = data.frame()

for (team in all_teams) {
  all_team_scores = rbind(all_team_scores, get_team_scores(tm = team))
}
```

``` r
team_color_mapping = hockeyR::team_logos_colors |>
  select(team_abbr, team_color = team_color1)

team_ppg = all_team_scores |>
  group_by(team) |>
  summarise(gspg = mean(score),
            gapg = mean(other_score)) |>
  left_join(team_color_mapping, by = c("team" = "team_abbr")) |>
  mutate(team_color = ifelse(team == "UTA", "#71AFE5", team_color))

team_ppg |>
  ggplot(aes(gspg, gapg)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  geom_vline(xintercept = mean(team_ppg$gspg), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(team_ppg$gapg), linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_ppg$team_color) +
  scale_x_continuous(breaks = seq(2, 5, by = 0.1)) +
  scale_y_continuous(breaks = seq(2, 5, by = 0.1)) +
  labs(x = "Goals Scored per Game", y = "Goals Allowed per Game",
       title = "Scatterplot of goals scored/allowed per game by team")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
team_ppg |>
  mutate(ss = scale(gspg),
         as = scale(gapg),
         ovr = round(ss - as, 2),
         poslab = ifelse(ovr >= 0, ovr, ""),
         neglab = ifelse(ovr < 0, ovr, ""),
         rk = rank(-ovr),
         team = as.character(glue("{team} ({rk})"))) |>
  ggplot(aes(reorder(team, ovr), ovr)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = poslab), size = 3, hjust = -0.2) +
  geom_text(aes(label = neglab), size = 3, hjust = 1.2) +
  scale_fill_manual(values = team_ppg$team_color) +
  scale_y_continuous(breaks = seq(-5, 5, by = 0.5)) +
  coord_flip() +
  labs(x = NULL, y = "Scaled Power Rating",
       title = "NHL Scaled Power Rankings")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
end_npr = end_games |>
  inner_join(team_ppg, by = c("home_team" = "team")) |>
  rename(home_gspg = gspg, home_gapg = gapg) |>
  select(-team_color) |>
  inner_join(team_ppg, by = c("away_team" = "team")) |>
  rename(away_gspg = gspg, away_gapg = gapg) |>
  select(-team_color) |>
  mutate(home_exp = (home_gspg + away_gapg) / 2,
         away_exp = (away_gspg + home_gapg) / 2,
         home_off_npr = home_score - home_exp,
         home_def_npr = away_exp - away_score,
         away_off_npr = away_score - away_exp,
         away_def_npr = home_exp - home_score)

get_team_off_npr = function(tm) {
  home = end_npr |> filter(home_team == tm) |> pull(home_off_npr)
  away = end_npr |> filter(away_team == tm) |> pull(away_off_npr)
  return(round(mean(c(home, away)), 3))
}

get_team_def_npr = function(tm) {
  home = end_npr |> filter(home_team == tm) |> pull(home_def_npr)
  away = end_npr |> filter(away_team == tm) |> pull(away_def_npr)
  return(round(mean(c(home, away)), 3))
}

team_npr = data.frame(team = all_teams) |>
  mutate(off_npr = sapply(team, get_team_off_npr),
         def_npr = sapply(team, get_team_def_npr),
         ovr_npr = off_npr + def_npr)

team_npr |>
  ggplot(aes(off_npr, def_npr)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = team), size = 3) +
  scale_color_manual(values = team_ppg$team_color) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  labs(x = "Offensive NPR", y = "Defensive NPR",
       title = "Naive Performance Rating by Team, 2025 NHL Season")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
