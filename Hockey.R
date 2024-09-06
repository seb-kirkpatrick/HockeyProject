library(tidyverse)
library(rvest)

url <- "https://www.spotrac.com/nhl/rankings/player/_/year/2024/sort/cap_total"

webpage <- read_html(url)

players <- webpage %>%
  html_nodes("ul.list-group li.list-group-item") %>%
  map_df(~ {
    data.frame(
      name = .x %>%
        html_node("a div.link") %>%
        html_text(trim = TRUE),
      team = .x %>%
        html_node("small") %>%
        html_text(trim = TRUE) %>%
        gsub("^(\\S{3}).*", "\\1", .),
      cap_hit = .x %>%
        html_node(".medium") %>%
        html_text(trim = TRUE) %>%
        gsub("[\\$\\,]", "", .) %>%
        as.numeric(),
      position = .x %>%
        html_node("small") %>%
        html_text(trim = TRUE) %>%
        str_extract("(?<=, )[A-Z/]+")
    )
  })

colnames(players)[1] <- "Player"
colnames(players)[2] <- "Team"
colnames(players)[3] <- "Cap Hit"
colnames(players)[4] <- "Pos"

cleaned_players <- players |>
  na.omit() |>
  filter(`Cap Hit` > 7000000)

cleaned_players |>
  mutate(Pos = ifelse(Pos == "C/RW", "C", Pos)) |>
  mutate(Pos = ifelse(Pos == "LW" | Pos == "RW", "W", Pos)) |>
  group_by(Pos) |>
  summarize(meanSal = median(`Cap Hit`))
  
  
  
  
  
  


url <- "https://www.hockey-reference.com/teams/CBJ/2024.html"

table <- read_html(url) |>
  html_element(css = "#skaters") |>
  html_table(header = TRUE, fill = TRUE)

colnames(table) <- table[1, ]
table <- table[-1, -1]
colnames(table)[10] <- "EV - G"
colnames(table)[11] <- "PP - G"
colnames(table)[12] <- "SH - G"
colnames(table)[14] <- "EV - A"
colnames(table)[15] <- "PP - A"
colnames(table)[16] <- "SH - A"

table <- table |>
  slice(1:(n() - 1))

print(table)




base_url <- "https://www.hockey-reference.com/teams/"
teams <- c(
  "ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL",
  "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR",
  "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "VEG", "WPG"
)
season <- "2024"


all_team_data <- list()

for (team in teams) {
  
  url <- paste0(base_url, team, "/", season, ".html")
  
  table <- read_html(url) |>
    html_element(css = "#skaters") |>
    html_table(header = TRUE, fill = TRUE)
  
  colnames(table) <- table[1, ]
  table <- table[-1,-1 ]
  
  colnames(table)[10] <- "EV - G"
  colnames(table)[11] <- "PP - G"
  colnames(table)[12] <- "SH - G"
  colnames(table)[14] <- "EV - A"
  colnames(table)[15] <- "PP - A"
  colnames(table)[16] <- "SH - A"
  
  table <- table |>
    slice(1:(n() - 1)) |>
    mutate(Team = team)
  
  all_team_data[[team]] <- table
  
  Sys.sleep(20)
}

combined_data <- bind_rows(all_team_data)
