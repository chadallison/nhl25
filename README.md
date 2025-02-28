NHL 2025
================

------------------------------------------------------------------------

``` r
team_hex = data.frame(
  team_abbr = c("ANA", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL",
                "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NJD",
                "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "SEA", "SJS",
                "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WPG", "WSH"),
  team_hex = c("#F47A38", "#FFB81C", "#002F87", "#CC0000",
               "#041E42", "#C8102E", "#CF0A2C", "#6F263D",
               "#006847", "#CE1126", "#041E42", "#C8102E",
               "#A2AAAD", "#154734", "#AF1E2D", "#CE1126",
               "#FFB81C", "#00539B", "#0038A8", "#C8102E",
               "#F74902", "#FCB514", "#99D9D9", "#006D75",
               "#002F87", "#002868", "#00205B", "#71AFE5",
               "#00843D", "#B4975A", "#041E42", "#041E42")
)
```

``` r
url = "https://api-web.nhle.com/v1/standings/now"
response = GET(url)
data = content(response, as = "text", encoding = "UTF-8") |> fromJSON()

standings = data$standings |>
  as.data.frame() |>
  clean_names() |>
  mutate(city_name = place_name$default,
         team_name = team_common_name$default,
         team_abbr = team_abbrev$default) |>
  inner_join(team_hex, by = "team_abbr") |>
  select(city_name, team_name, team_abbr, team_logo, team_hex,
         conference_name, division_name,
         games_played, goal_differential, goal_against, goal_for,
         home_wins, home_losses, home_ot_losses,
         home_goal_differential, home_goals_against, home_goals_for,
         road_wins, road_losses, road_ot_losses,
         road_goal_differential, road_goals_against, road_goals_for,
         wins, losses, ot_losses, points) |>
  mutate(gfpg = goal_for / games_played,
         gapg = goal_against / games_played)
```

``` r
standings |>
  ggplot(aes(gfpg, gapg)) +
  geom_point(aes(col = team_abbr), shape = "square", size = 4, show.legend = F) +
  scale_color_manual(values = team_hex$team_hex) +
  ggrepel::geom_text_repel(aes(label = team_abbr), size = 3) +
  geom_vline(xintercept = mean(standings$gfpg), linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = mean(standings$gapg), linetype = "dashed", alpha = 0.25) +
  labs(x = "Goals Scored per Game", y = "Goals Allowed per Game", title = "Season-long goals scored/allowed per game") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.1))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
standings |>
  mutate(py = (goal_for ^ 2) / (goal_for ^ 2 + goal_against ^ 2)) |>
  ggplot(aes(points, py)) +
  geom_point(aes(col = team_abbr), shape = "square", size = 4, show.legend = F) +
  geom_line(stat = "smooth", formula = y ~ x, method = "loess", linetype = "dashed", alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = team_abbr), size = 3, max.overlaps = 32) +
  scale_color_manual(values = team_hex$team_hex) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  labs(x = "Points", y = "Pythagorean win percentage",
       title = "Team points vs. pythagorean win percentage",
       subtitle = "Teams above/below dashed line are better/worse than their record per PWP")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### new work

``` r
start_date = as.Date("2024-10-01")
end_date = as.Date("2025-04-15")
games = list()
current_date = start_date

while (current_date <= end_date) {
    url = paste0("https://api-web.nhle.com/v1/schedule/", format(current_date, "%Y-%m-%d"))
    response = GET(url)
    
    if (status_code(response) == 200) {
        data = content(response, as = "parsed", type = "application/json")
        game_week = data$gameWeek
        
        if (!is.null(game_week)) {
            for (game_day in game_week) {
                for (game in game_day$games) {
                    if (game$gameType == 2) {
                        utc_time = game$startTimeUTC
                        
                        if (!is.null(utc_time)) {
                            utc_time_obj = ymd_hms(utc_time, tz = "UTC")
                            game_date = format(with_tz(utc_time_obj, "America/New_York"), "%Y-%m-%d")
                        } else {
                            game_date = NA
                        }
                        
                        home_score = as.integer(game$homeTeam$score %||% 0)
                        away_score = as.integer(game$awayTeam$score %||% 0)
                        
                        game_outcome = game$gameOutcome
                        overtime = ifelse(!is.null(game_outcome) && game_outcome$lastPeriodType %in% c("OT", "SO"), TRUE, FALSE)
                        
                        games = append(games, list(data.frame(
                            game_id = game$id,
                            game_date = game_date,
                            home_team = game$homeTeam$abbrev,
                            away_team = game$awayTeam$abbrev,
                            home_score = home_score,
                            away_score = away_score,
                            total_score = home_score + away_score,
                            overtime = overtime,
                            venue = game$venue$default
                        )))
                    }
                }
            }
        }
    }
    
    current_date = current_date + days(1)
}

games_df = bind_rows(games) |>
    filter(total_score > 0) |>
    distinct() |>
    mutate(game_date = as.Date(game_date, format = "%Y-%m-%d"))
```

``` r
all_teams = sort(unique(c(games_df$home_team, games_df$away_team)))

get_team_gspg = function(tm) {
  home = games_df |> filter(home_team == tm) |> pull(home_score)
  away = games_df |> filter(away_team == tm) |> pull(away_score)
  return(round(mean(c(home, away)), 3))
}

get_team_gapg = function(tm) {
  home = games_df |> filter(home_team == tm) |> pull(away_score)
  away = games_df |> filter(away_team == tm) |> pull(home_score)
  return(round(mean(c(home, away)), 3))
}

team_gpg = data.frame(team = all_teams) |>
  mutate(gspg = sapply(team, get_team_gspg),
         gapg = sapply(team, get_team_gapg))

end_npr = games_df |>
  inner_join(team_gpg, by = c("home_team" = "team")) |>
  rename(home_gspg = gspg, home_gapg = gapg) |>
  inner_join(team_gpg, by = c("away_team" = "team")) |>
  rename(away_gspg = gspg, away_gapg = gapg) |>
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
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_abline(linetype = "dashed", alpha = 0.25) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_color_manual(values = team_hex$team_hex) +
  labs(x = "Offensive NPR", y = "Defensive NPR",
       title = "Team Offensive/Defensive NPR",
       subtitle = "Teams above/below diagonal line are better defensively/offensively")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
team_npr |>
  inner_join(standings, by = c("team" = "team_abbr")) |>
  ggplot(aes(points, ovr_npr)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  geom_line(stat = "smooth", formula = y ~ x, method = "loess", linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex$team_hex) +
  labs(x = "Points", y = "NPR", title = "Team Points vs. NPR",
       subtitle = "Teams above/below line are better/worse than their record suggests") +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
get_team_off_npr_on_date = function(tm, dt) {
  home = end_npr |> filter(game_date <= dt & home_team == tm) |> pull(home_off_npr)
  away = end_npr |> filter(game_date <= dt & away_team == tm) |> pull(away_off_npr)
  return(round(mean(c(home, away)), 3))
}

get_team_def_npr_on_date = function(tm, dt) {
  home = end_npr |> filter(game_date <= dt & home_team == tm) |> pull(home_def_npr)
  away = end_npr |> filter(game_date <= dt & away_team == tm) |> pull(away_def_npr)
  return(round(mean(c(home, away)), 3))
}

all_szn_dates = sort(unique(end_npr$game_date))

npr_on_dates = crossing(date = all_szn_dates, team = all_teams) |>
  rowwise() |>
  mutate(off_npr_on_date = get_team_off_npr_on_date(team, date),
         def_npr_on_date = get_team_def_npr_on_date(team, date)) |>
  ungroup() |>
  filter(!is.na(off_npr_on_date) & !is.na(def_npr_on_date))

npr_on_dates |>
  mutate(roll_off = rollapply(off_npr_on_date, FUN = "mean", width = 5, align = "right", fill = NA),
         roll_def = rollapply(def_npr_on_date, FUN = "mean", width = 5, align = "right", fill = NA)) |>
  filter(!is.na(roll_off) & !is.na(roll_def)) |>
  ggplot(aes(date, roll_off)) +
  geom_line(aes(col = team), show.legend = F) +
  scale_color_manual(values = team_hex$team_hex) +
  labs(title = "ignore this one but it looks kinda cool i guess")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
roll_width = 10

games_df |>
  select(game_date, team = home_team, team_score = home_score, opp_score = away_score) |>
  bind_rows(games_df |>
  select(game_date, team = away_team, team_score = away_score, opp_score = home_score)) |>
  arrange(team, game_date) |>
  mutate(roll_score = rollapply(team_score, width = roll_width, FUN = "mean", align = "right", fill = NA),
         roll_allow = rollapply(opp_score, width = roll_width, FUN = "mean", align = "right", fill = NA)) |>
  filter(!is.na(roll_score) & !is.na(roll_allow)) |>
  mutate(py = (roll_score ^ 2) / (roll_score ^ 2 + roll_allow ^ 2)) |>
  filter(team %in% c("CHI", "CAR", "COL", "WPG", "WSH", "TBL")) |>
  ggplot(aes(game_date, py)) +
  geom_line(aes(col = team), linewidth = 2, show.legend = T) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("red", "gold", "maroon", "lightblue", "navy", "pink")) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.05)) +
  labs(x = NULL, y = "Pythagorean win percentage", col = NULL,
       title = glue("Season-long pythagorean win percentage in {roll_width}-game rolling windows"))
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
diff_df = games_df |>
  select(game_date, team = home_team, team_score = home_score, opp_score = away_score) |>
  bind_rows(games_df |>
  select(game_date, team = away_team, team_score = away_score, opp_score = home_score)) |>
  arrange(team, game_date) |>
  group_by(team) |>
  mutate(game_num = row_number()) |>
  ungroup() |>
  inner_join(npr_on_dates, by = c("game_date" = "date", "team")) |>
  mutate(ovr_npr_on_date = off_npr_on_date + def_npr_on_date) |>
  group_by(team) |>
  mutate(lag10 = lag(ovr_npr_on_date, n = 10, default = NA)) |>
  ungroup() |>
  filter(!is.na(lag10)) |>
  group_by(team) |>
  slice_max(game_num, n = 1) |>
  ungroup() |>
  mutate(diff = ovr_npr_on_date - lag10) |>
  arrange(desc(diff))

diff_df |>
  ggplot(aes(x = reorder(paste0(team, " (", round(diff, 2), ")"), ovr_npr_on_date), 
             xend = reorder(paste0(team, " (", round(diff, 2), ")"), ovr_npr_on_date), 
             y = lag10, yend = ovr_npr_on_date, col = diff)) + 
  geom_segment(linewidth = 3, show.legend = F) +
  geom_text(aes(y = ovr_npr_on_date, 
                label = ifelse(diff >= 0, "→", "")), 
            size = 4, hjust = -0.2, vjust = 0.25, color = "black", alpha = 0.75) +  
  geom_text(aes(y = ovr_npr_on_date, 
                label = ifelse(diff <= 0, "←", "")), 
            size = 4, hjust = 1.2, vjust = 0.25, color = "black", alpha = 0.75) +  
  coord_flip() +
  labs(x = NULL, y = "Change in NPR",
       title = "Team NPR Trends in Past Ten Games") +
  scale_y_continuous(breaks = seq(-5, 5, by = 0.1)) +
  scale_color_gradient(low = "indianred3", high = "springgreen4")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
games_df |>
  transmute(team = home_team, home_margin = home_score - away_score) |>
  group_by(team) |>
  summarise(avg_home_margin = mean(home_margin)) |>
  inner_join(games_df |>
  transmute(team = away_team, away_margin = away_score - home_score) |>
  group_by(team) |>
  summarise(avg_away_margin = mean(away_margin)), by = "team") |>
  ggplot(aes(avg_home_margin, avg_away_margin)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex$team_hex) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  labs(x = "Avg. Home Margin", y = "Avg. Road Margin",
       title = "Home Ice Adantage") +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.25)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.25))
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
end_npr |>
  transmute(team = home_team, home_npr = home_off_npr + home_def_npr) |>
  group_by(team) |>
  summarise(avg_home_margin = mean(home_npr)) |>
  inner_join(end_npr |>
  transmute(team = away_team, away_npr = away_off_npr + away_def_npr) |>
  group_by(team) |>
  summarise(avg_away_margin = mean(away_npr)), by = "team") |>
  ggplot(aes(avg_home_margin, avg_away_margin)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex$team_hex) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  labs(x = "Avg. Home NPR", y = "Avg. Road NPR",
       title = "Home Ice Merchants") +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.25)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.25))
```

![](README_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
end_npr = end_npr |>
  mutate(home_npr = home_off_npr + home_def_npr,
         away_npr = away_off_npr + away_def_npr)

sl_npr_trends = end_npr |>
  select(game_date, team = home_team, npr = home_npr) |>
  bind_rows(end_npr |>
  select(game_date, team = away_team, npr = away_npr)) |>
  arrange(team, game_date) |>
  group_by(team) |>
  mutate(game_num = row_number(),
         roll_npr = rollapply(npr, width = 10, align = "right", FUN = "mean", fill = NA)) |>
  ungroup() |>
  filter(!is.na(roll_npr))

sl_npr_trends |>
  ggplot(aes(game_num, roll_npr)) +
  geom_line(aes(col = team), linewidth = 1.25, show.legend = F) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex$team_hex) +
  facet_wrap(vars(team), scales = "free_x", nrow = 4) +
  theme(axis.text = element_blank()) +
  scale_y_continuous(breaks = seq(-2, 2, by = 2)) +
  labs(x = NULL, y = "NPR in 10-game rolling windows", title = "Season-long NPR trends",
       caption = paste0(month(Sys.Date(), label = T, abbr = F), " ", day(Sys.Date()), ", ", year(Sys.Date())))
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
team_gpg |>
  mutate(gspg_scaled = scale(gspg),
         gapg_scaled = scale(gapg),
         full = gspg_scaled - gapg_scaled) |>
  arrange(desc(full))
```

    ##    team  gspg  gapg gspg_scaled  gapg_scaled       full
    ## 1   WPG 3.550 2.350  1.72773162 -2.003327046  3.7310587
    ## 2   WSH 3.627 2.610  1.97902321 -1.225008997  3.2040322
    ## 3   TBL 3.586 2.672  1.84521860 -1.039410077  2.8846287
    ## 4   DAL 3.345 2.586  1.05870858 -1.296853740  2.3555623
    ## 5   VGK 3.339 2.814  1.03912741 -0.614328681  1.6534561
    ## 6   NJD 3.050 2.517  0.09596809 -1.503407376  1.5993755
    ## 7   FLA 3.350 2.850  1.07502621 -0.506561567  1.5815878
    ## 8   CAR 3.254 2.814  0.76172761 -0.614328681  1.3760563
    ## 9   TOR 3.190 2.845  0.55286188 -0.521529222  1.0743911
    ## 10  EDM 3.254 2.932  0.76172761 -0.261092028  1.0228196
    ## 11  LAK 2.911 2.571 -0.35766217 -1.341756704  0.9840945
    ## 12  COL 3.217 3.000  0.64097711 -0.057531923  0.6985090
    ## 13  CBJ 3.390 3.271  1.20556729  0.753714966  0.4518523
    ## 14  MIN 2.881 2.898 -0.45556798 -0.362872081 -0.0926959
    ## 15  NYR 3.052 3.103  0.10249515  0.250801766 -0.1483066
    ## 16  OTT 2.776 2.845 -0.79823832 -0.521529222 -0.2767091
    ## 17  UTA 2.850 2.967 -0.55673732 -0.156318445 -0.4004189
    ## 18  STL 2.867 3.000 -0.50125736 -0.057531923 -0.4437254
    ## 19  BUF 3.228 3.404  0.67687591  1.151854584 -0.4749787
    ## 20  DET 2.932 3.153 -0.28912810  0.400478313 -0.6896064
    ## 21  NYI 2.724 2.966 -0.96794173 -0.159311976 -0.8086298
    ## 22  VAN 2.746 3.017 -0.89614413 -0.006641897 -0.8895022
    ## 23  CGY 2.655 2.948 -1.19312510 -0.213195533 -0.9799296
    ## 24  SEA 2.900 3.250 -0.39356097  0.690850816 -1.0844118
    ## 25  PHI 2.967 3.350 -0.17490465  0.990203912 -1.1651086
    ## 26  MTL 2.983 3.373 -0.12268822  1.059055124 -1.1817433
    ## 27  ANA 2.603 3.052 -1.36282850  0.098131687 -1.4609602
    ## 28  BOS 2.733 3.200 -0.93856999  0.541174268 -1.4797443
    ## 29  NSH 2.569 3.276 -1.47378842  0.768682621 -2.2424710
    ## 30  PIT 2.885 3.672 -0.44251387  1.954120881 -2.3966348
    ## 31  CHI 2.678 3.559 -1.11806397  1.615851882 -2.7339159
    ## 32  SJS 2.567 3.750 -1.48031548  2.187616295 -3.6679318

``` r
team_win_pct = standings |>
  transmute(team_abbr, win_pct = round(wins / (wins + losses) * 100, 2))

team_results = games_df |>
  select(team = home_team, opp = away_team, score = home_score, opp_score = away_score) |>
  bind_rows(games_df |>
  select(team = away_team, opp = home_team, score = away_score, opp_score = home_score)) |>
  inner_join(team_win_pct, by = c("team" = "team_abbr")) |>
  rename(team_win_pct = win_pct) |>
  inner_join(team_win_pct, by = c("opp" = "team_abbr")) |>
  rename(opp_win_pct = win_pct)

get_team_above_500_win_pct = function(tm) {
  f_games = team_results |> filter(team == tm & opp_win_pct >= 50)
  wins = f_games |> filter(score > opp_score)
  return(round(nrow(wins) / nrow(f_games) * 100, 2))
}

get_team_below_500_win_pct = function(tm) {
  f_games = team_results |> filter(team == tm & opp_win_pct < 50)
  wins = f_games |> filter(score > opp_score)
  return(round(nrow(wins) / nrow(f_games) * 100, 2))
}

team_500_wp = data.frame(team = all_teams) |>
  mutate(above500 = sapply(team, get_team_above_500_win_pct),
         below500 = sapply(team, get_team_below_500_win_pct))

team_500_wp |>
  ggplot(aes(above500, below500)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  geom_vline(xintercept = 50, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5) +
  geom_abline(linetype = "dashed", alpha = 0.25) +
  scale_color_manual(values = team_hex$team_hex) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  labs(x = "Win Percentage vs. Teams with Above .500 Record",
       y = "Win Percentage vs. Teams with Below .500 Record",
       title = "Records vs. Above/Below .500 Opponents") +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5))
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
