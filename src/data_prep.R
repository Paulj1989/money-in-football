library(worldfootballR)
library(dplyr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Squad Values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get player values from transfermarkt
player_values_raw <- 
  tm_player_market_values(
    country_name = 
      c(
        "England", 
        "Spain",
        "Germany",
        "Italy",
        "France",
        "Netherlands",
        "Portugal",
        "Austria"
      ),
    start_year = c(2012:2021)
  )

# wrangle to team total values per season
squad_values <- 
  player_values_raw %>%
  mutate(
    comp_name = case_when(
      country == "England" ~ "Premier League",
      country == "Spain" ~ "La Liga",
      country == "Germany" ~ "Bundesliga",
      country == "Italy" ~ "Serie A",
      country == "France" ~ "Ligue 1",
      country == "Netherlands" ~ "Eredivisie",
      country == "Portugal" ~ "Primeira Liga", 
      country == "Austria" ~ "Austrian Bundesliga"
      )
    ) %>%
  select(
    comp_name, squad, season_start_year, player_market_value_euro
    ) %>%
  rename(
    season = season_start_year,
    league = comp_name
    ) %>%
  na.omit() %>%
  group_by(squad, league, season) %>%
  summarise(
    value = sum(player_market_value_euro)
    ) %>%
  arrange(desc(value))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# League Tables ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get league tables from fb ref
fb_league_table_raw <- 
  fb_season_team_stats(
    country = c(
      "ENG",
      "ESP",
      "GER",
      "ITA",
      "FRA",
      "NED",
      "POR",
      "AUT"
    ),
    gender = "M",
    season_end_year = c(2013:2022),
    tier = "1st",
    stat_type = "league_table",
    time_pause = 5
  )

# wrangle league tables to merge with squad values
league_tables <-
  fb_league_table_raw %>%
  janitor::clean_names() %>%
  rename(league = competition_name) %>%
  mutate(season = season_end_year - 1,
         league =
           recode(
             league,
             "Fußball-Bundesliga" = "Bundesliga",
             "Austrian Football Bundesliga" = "Austrian Bundesliga"
           )
         ) %>%
  select(league, season, squad, pts) %>%
  na.omit()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transfer Balances ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

country_list <-
  c(
    "England", 
    "Spain",
    "Germany",
    "Italy",
    "France",
    "Netherlands",
    "Portugal",
    "Austria"
  )

season_list <-
  c(2012:2021)

crossed <- tidyr::crossing(country_list, season_list)

# specify function for transfer balances
balance_fun <- 
  function(countries, years){
    tm_team_transfer_balances(
      country_name = countries,
      start_year = years
    ) %>%
    mutate(season = years)
  }

# get transfer balances 
transfer_balance_raw <- 
  purrr::map2_dfr(
    crossed$country_list,
    crossed$season_list,
    .f = balance_fun
  )

transfer_balances <-
  transfer_balance_raw %>%  
  mutate(
    league = case_when(
      country == "England" ~ "Premier League",
      country == "Spain" ~ "La Liga",
      country == "Germany" ~ "Bundesliga",
      country == "Italy" ~ "Serie A",
      country == "France" ~ "Ligue 1",
      country == "Netherlands" ~ "Eredivisie",
      country == "Portugal" ~ "Primeira Liga", 
      country == "Austria" ~ "Austrian Bundesliga"
    ),
    net_spend = expenditure_euros - income_euros
  ) %>%
  select(league, season, squad, net_spend)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of Players ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fb_std_raw <- 
  fb_season_team_stats(
  country = c(
    "ENG",
    "ESP",
    "GER",
    "ITA",
    "FRA",
    "NED",
    "POR",
    "AUT"
  ),
  gender = "M",
  season_end_year = c(2013:2022),
  tier = "1st",
  stat_type = "standard",
  time_pause = 3)
  
num_players <-
  fb_std_raw %>%
  janitor::clean_names() %>%
  filter(team_or_opponent == "team") %>%
  rename(league = competition_name) %>%
  mutate(season = season_end_year - 1,
         league =
           recode(
             league,
             "Fußball-Bundesliga" = "Bundesliga",
             "Austrian Football Bundesliga" = "Austrian Bundesliga"
             )
         ) %>%
  select(league, season, squad, num_players) %>%
  na.omit()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Injuries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

injury_history_raw <- 
  tm_player_injury_history(
    player_urls = player_values_raw$player_url
    )

# fix inconsistencies in club names for summary functions
injury_history_raw <-
  injury_history_raw %>%
  mutate(
    club =
      recode(
        club,
        "Liverpool FC" = "Liverpool",
        "Chelsea FC" = "Chelsea",
        "FC Barcelona" = "Barcelona",
        "Atlético de Madrid" = "Atlético Madrid",
        "Tottenham Hotspur" = "Tottenham",
        "Juventus FC" = "Juventus",
        "Manchester United" = "Manchester Utd",
        "Arsenal FC" = "Arsenal",
        "SSC Napoli" = "Napoli",
        "AS Monaco" = "Monaco",
        "Everton FC" = "Everton",
        "Valencia CF" = "Valencia",
        "AS Roma" = "Roma",
        "Olympique Lyon" = "Lyon",
        "Bayer 04 Leverkusen" = "Bayer Leverkusen",
        "Wolverhampton Wanderers" = "Wolves",
        "Atalanta BC" = "Atalanta",
        "Sevilla FC" = "Sevilla",
        "Villarreal CF" = "Villarreal",
        "SS Lazio" = "Lazio",
        "West Ham United" = "West Ham",
        "TSG 1899 Hoffenheim" = "Hoffenheim",
        "Newcastle United" = "Newcastle",
        "ACF Fiorentina" = "Fiorentina",
        "LOSC Lille" = "Lille",
        "Brighton & Hove Albion" = "Brighton",
        "Eintracht Frankfurt" = "Frankfurt",
        "AFC Bournemouth" = "Bournemouth",
        "Southampton FC" = "Southampton",
        "Stade Rennais FC" = "Rennes",
        "Brentford FC" = "Brentford",
        "Real Betis Balompié" = "Real Betis",
        "Olympique Marseille" = "Marseille",
        "FC Schalke 04" = "Schalke",
        "VfL Wolfsburg" = "Wolfsburg",
        "OGC Nice" = "Nice",
        "Fulham FC" = "Fulham",
        "US Sassuolo" = "Sassuolo",
        "Torino FC" = "Torino",
        "Watford FC" = "Watford",
        "Celta de Vigo" = "Celta Vigo",
        "UC Sampdoria" = "Sampdoria",
        "Getafe CF" = "Getafe",
        "Udinese Calcio" = "Udinese",
        "Burnley FC" = "Burnley",
        "Genoa CFC" = "Genoa",
        "VfB Stuttgart" = "Stuttgart",
        "Cagliari Calcio" = "Cagliari",
        "Bologna FC 1909" = "Bologna",
        "FC Girondins Bordeaux" = "Bordeaux",
        "SC Freiburg" = "Freiburg",
        "RCD Espanyol Barcelona" = "Espanyol",
        "SV Werder Bremen" = "Werder Bremen",
        "West Bromwich Albion" = "West Brom",
        "1.FSV Mainz 05" = "Mainz",
        "AS Saint-Étienne" = "Saint-Étienne",
        "RC Lens" = "Lens",
        "Sheffield United" = "Sheffield Utd",
        "Montpellier HSC" = "Montpellier",
        "FC Augsburg" = "Augsburg",
        "Huddersfield Town" = "Huddersfield",
        "Sunderland AFC" = "Sunderland",
        "Middlesbrough FC" = "Middlesbrough",
        "Parma FC" = "Parma",
        "Empoli FC " = "Empoli",
        "Granada CF" = "Granada",
        "RC Strasbourg Alsace" = "Strasbourg",
        "FC Nantes" = "Nantes",
        "1. FC Köln" = "Köln",
        "1.FC Union Berlin" = "Union Berlin",
        "AC Monza" = "Monza",
        "CA Osasuna" = "Osasuna",
        "FC Toulouse" = "Toulouse",
        "Levante UD" = "Levante",
        "CD Leganés" = "Leganés",
        "Angers SCO" = "Angers",
        "FC Metz" = "Metz",
        "Stade Brestois 29" = "Stade Brest",
        "Girona FC" = "Girona",
        "Fortuna Düsseldorf" = "Düsseldorf",
        "SD Eibar" = "Eibar",
        "UD Las Palmas" = "Las Palmas",
        "Nîmes Olympique" = "Nîmes",
        "Real Valladolid CF" = "Real Valladolid",
        "Málaga CF" = "Málaga",
        "Brescia Calcio" = "Brescia",
        "US Salernitana 1919" = "Salernitana",
        "Venezia FC" = "Venezia",
        "UD Almería" = "Almería",
        "Spezia Calcio" = "Spezia",
        "Elche CF" = "Elche",
        "RCD Mallorca" = "Mallorca",
        "ESTAC Troyes" = "Troyes",
        "FC Lorient" = "Lorient",
        "EA Guingamp" = "Guingamp",
        "Amiens SC" = "Amiens",
        "Dijon FCO" = "Dijon",
        "US Palermo" = "Palermo",
        "Palermo SSD" = "Palermo",
        "Cádiz CF" = "Cádiz",
        "SM Caen" = "Caen",
        "SD Huesca" = "Huesca",
        "Benevento Calcio" = "Benevento",
        "Chievo Verona" = "Chievo",
        "Clermont Foot 63" = "Clermont Foot",
        "US Cremonese" = "Cremonese",
        "Frosinone Calcio" = "Frosinone",
        "1.FC Nuremberg" = "Nürnberg",
        "US Lecce" = "Lecce",
        "FC Crotone" = "Crotone",
        "VfL Bochum" = "Bochum",
        "Carpi FC 1909" = "Carpi",
        "Delfino Pescara 1936" = "Pescara",
        "FC Ingolstadt 04" = "Ingolstadt",
        "SpVgg Greuther Fürth" = "Greuther Fürth",
        "AJ Auxerre" = "Auxerre",
        "SC Bastia" = "Bastia",
        "AS Nancy-Lorraine" = "Nancy",
        "SC Paderborn 07" = "Paderborn",
        "SV Darmstadt 98" = "Darmstadt",
        "Willem II Tilburg" = "Willem II",
        "Vitória Setúbal FC" = "Vitória Setúbal",
        "Vitória Guimarães SC" = "Vitória Guimarães",
        "Vitesse Arnhem" = "Vitesse",
        "Valenciennes FC" = "Valenciennes",
        "AS Livorno" = "Livorno",
        "Twente Enschede FC" = "Twente",
        "TSV Hartberg" = "Hartberg",
        "FC Évian Thonon Gaillard" = "Evian",
        "SV Ried" = "Ried",
        "SV Mattersburg" = "Mattersburg",
        "SV Grödig" = "Grödig",
        "SL Benfica" = "Benfica",
        "SKN St. Pölten" = "St. Pölten",
        "SK Sturm Graz" = "Sturm Graz",
        "SC Olhanense" = "Olhanense",
        "SC Heerenveen" = "Heerenveen",
        "SC Cambuur-Leeuwarden" = "Cambuur",
        "SC Braga" = "Braga",
        "SC Beira-Mar" = "Beira-Mar",
        "Roda JC Kerkrade" = "Roda",
        "Rio Ave FC" = "Rio Ave",
        "Red Bull Salzburg" = "RB Salzburg",
        "Reading FC" = "Reading",
        "Rapid Vienna" = "Rapid Wien",
        "Queens Park Rangers" = "QPR",
        "Portimonense SC" = "Portimonense",
        "PEC Zwolle" = "Zwolle",
        "Moreirense FC" = "Moreirense",
        "Gil Vicente FC" = "Gil Vicente",
        "GD Estoril Praia" = "Estoril",
        "GD Chaves" = "Chaves",
        "Feyenoord Rotterdam" = "Feyenoord",
        "FC Wacker Innsbruck" = "Wacker Innsbruck",
        "FC Utrecht" = "Utrecht",
        "FC Sochaux-Montbéliard" = "Sochaux",
        "FC Porto" = "Porto",
        "FC Paços de Ferreira" = "Paços",
        "FC Penafiel" = "Penafiel",
        "FC Groningen" = "Groningen",
        "FC Famalicão" = "Famalicão",
        "FC Emmen" = "Emmen",
        "FC Dordrecht" = "Dordrecht",
        "FC Admira Wacker Mödling" = "Admira",
        "Excelsior Rotterdam" = "Excelsior",
        "Eintracht Braunschweig" = "Braunschweig",
        "Desportivo Aves (- 2020)" = "Aves",
        "De Graafschap Doetinchem" = "De Graafschap",
        "Córdoba CF" = "Córdoba",
        "CF União Madeira" = "União",
        "CF Os Belenenses" = "Belenenses",
        "AC Cesena" = "Cesena",
        "CD Tondela" = "Tondela",
        "CD Santa Clara" = "Santa Clara",
        "CD Feirense" = "Feirense",
        "Calcio Catania" = "Catania",
        "Boavista FC" = "Boavista",
        "Belenenses SAD" = "Belenenses",
        "Austria Vienna" = "Austria Wien",
        "Ajax Amsterdam" = "Ajax",
        "ACR Siena 1904" = "Siena",
        "ACN Siena 1904" = "Siena",
        "AC Siena" = "Siena",
        "Robur Siena" = "Siena",
        "SC Wiener Neustadt" = "Wiener Neustadt",
        "SC Magna Wiener Neustadt" = "Wiener Neustadt"
      ))

injuries <-
  injury_history_raw %>%
  select(club, season_injured, duration) %>%
  na.omit() %>%
  filter(duration != "? days") %>%
  mutate(
    duration = as.numeric(stringr::str_remove(duration, " days"))
  ) %>%
  distinct() %>%
  group_by(club, season_injured) %>%
  summarise(duration = sum(as.numeric(duration))) %>%
  rename(squad = club,
         season = season_injured,
         days_injured = duration
         )

readr::write_rds(injuries, here::here("data", "injuries.rds"))

injuries <- 
  injuries %>%
  filter(
    season %in% 
      c(
        "12/13", "13/14", "14/15", "15/16",
        "16/17", "17/18", "18/19", "19/20",
        "20/21", "21/22", "22/23"
      )
    ) %>%
  mutate(
    season = as.numeric(
      paste0("20", stringr::str_remove(season, "/..")
             )
      )
    )
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total Number of Games Played ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CURRENTLY HELD OUT FROM DATASET DUE TO ISSUES WITH THIS DATA

# specify function for squad stats
# squad_fun <- 
#   function(countries, years){
#     tm_squad_stats(
#       team_url =
#         tm_league_team_urls(
#           country_name = countries,
#           start_year = years
#         )
#     ) %>%
#       mutate(season = years)
#   }

# get squad stats
# squad_stats_raw <- 
#   purrr::map2_dfr(
#     crossed$country_list,
#     crossed$season_list,
#     .f = squad_fun
#   )

# squad_stats <-
#   squad_stats_raw %>%  
#   mutate(
#     league = case_when(
#       country == "England" ~ "Premier League",
#       country == "Spain" ~ "La Liga",
#       country == "Germany" ~ "Bundesliga",
#       country == "Italy" ~ "Serie A",
#       country == "France" ~ "Ligue 1",
#       country == "Netherlands" ~ "Eredivisie",
#       country == "Portugal" ~ "Primeira Liga", 
#       country == "Austria" ~ "Austrian Bundesliga"
#     ),
#   ) %>%
#   rename(squad = team_name) %>%
#   select(league, season, squad, in_squad) %>%
#   group_by(league, season, squad) %>%
#   summarise(total_games = max(in_squad))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fix Team Names ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

league_tables <- 
  league_tables %>%
  mutate(
    squad =
      recode(
        squad,
        "Paris S-G" = "Paris Saint-Germain",
        "Inter" = "Inter Milan",
        "Dortmund" = "Borussia Dortmund",
        "Milan" = "AC Milan",
        "Leverkusen" = "Bayer Leverkusen",
        "M'Gladbach" = "Borussia Mönchengladbach",
        "Betis" = "Real Betis",
        "Athletic Club" = "Athletic Bilbao",
        "Nott'ham Forest" = "Nottingham Forest",
        "Reims" = "Stade Reims",
        "Alavés" = "Deportivo Alavés",
        "Brest" = "Stade Brest",
        "La Coruña" = "Deportivo de La Coruña",
        "Valladolid" = "Real Valladolid",
        "Arminia" = "Arminia Bielefeld",
        "Ajaccio" = "AC Ajaccio",
        "Gazélec Ajaccio" = "GFC Ajaccio",
        "Newcastle Utd" = "Newcastle",
        "Eint Frankfurt" = "Frankfurt",
        "Schalke 04" = "Schalke",
        "Mainz 05" = "Mainz",
        "Ingolstadt 04" = "Ingolstadt",
        "Darmstadt 98" = "Darmstadt",
        "Paderborn 07" = "Paderborn",
        "Vitória" = "Vitória Guimarães",
        "Roda JC" = "Roda",
        "Zaragoza" = "Real Zaragoza",
        "Go Ahead Eag" = "Go Ahead Eagles",
        "Wacker Innsbr" = "Wacker Innsbruck",
        "Sparta R'dam" = "Sparta Rotterdam",
        "Gil Vicente FC" = "Gil Vicente"
      )
  )

num_players <- 
  num_players %>%
  mutate(
    squad =
      recode(
        squad,
        "Paris S-G" = "Paris Saint-Germain",
        "Inter" = "Inter Milan",
        "Dortmund" = "Borussia Dortmund",
        "Milan" = "AC Milan",
        "Leverkusen" = "Bayer Leverkusen",
        "M'Gladbach" = "Borussia Mönchengladbach",
        "Betis" = "Real Betis",
        "Athletic Club" = "Athletic Bilbao",
        "Nott'ham Forest" = "Nottingham Forest",
        "Reims" = "Stade Reims",
        "Alavés" = "Deportivo Alavés",
        "Brest" = "Stade Brest",
        "La Coruña" = "Deportivo de La Coruña",
        "Valladolid" = "Real Valladolid",
        "Arminia" = "Arminia Bielefeld",
        "Ajaccio" = "AC Ajaccio",
        "Gazélec Ajaccio" = "GFC Ajaccio",
        "Newcastle Utd" = "Newcastle",
        "Eint Frankfurt" = "Frankfurt",
        "Schalke 04" = "Schalke",
        "Mainz 05" = "Mainz",
        "Ingolstadt 04" = "Ingolstadt",
        "Darmstadt 98" = "Darmstadt",
        "Paderborn 07" = "Paderborn",
        "Vitória" = "Vitória Guimarães",
        "Roda JC" = "Roda",
        "Zaragoza" = "Real Zaragoza",
        "Go Ahead Eag" = "Go Ahead Eagles",
        "Wacker Innsbr" = "Wacker Innsbruck",
        "Sparta R'dam" = "Sparta Rotterdam",
        "Gil Vicente FC" = "Gil Vicente"
      )
  )

squad_values <-
  squad_values %>%
  mutate(
    squad =
      recode(
        squad,
        "Liverpool FC" = "Liverpool",
        "Chelsea FC" = "Chelsea",
        "FC Barcelona" = "Barcelona",
        "Atlético de Madrid" = "Atlético Madrid",
        "Tottenham Hotspur" = "Tottenham",
        "Juventus FC" = "Juventus",
        "Manchester United" = "Manchester Utd",
        "Arsenal FC" = "Arsenal",
        "SSC Napoli" = "Napoli",
        "AS Monaco" = "Monaco",
        "Everton FC" = "Everton",
        "Valencia CF" = "Valencia",
        "AS Roma" = "Roma",
        "Olympique Lyon" = "Lyon",
        "Bayer 04 Leverkusen" = "Bayer Leverkusen",
        "Wolverhampton Wanderers" = "Wolves",
        "Atalanta BC" = "Atalanta",
        "Sevilla FC" = "Sevilla",
        "Villarreal CF" = "Villarreal",
        "SS Lazio" = "Lazio",
        "West Ham United" = "West Ham",
        "TSG 1899 Hoffenheim" = "Hoffenheim",
        "Newcastle United" = "Newcastle",
        "ACF Fiorentina" = "Fiorentina",
        "LOSC Lille" = "Lille",
        "Brighton & Hove Albion" = "Brighton",
        "Eintracht Frankfurt" = "Frankfurt",
        "AFC Bournemouth" = "Bournemouth",
        "Southampton FC" = "Southampton",
        "Stade Rennais FC" = "Rennes",
        "Brentford FC" = "Brentford",
        "Real Betis Balompié" = "Real Betis",
        "Olympique Marseille" = "Marseille",
        "FC Schalke 04" = "Schalke",
        "VfL Wolfsburg" = "Wolfsburg",
        "OGC Nice" = "Nice",
        "Fulham FC" = "Fulham",
        "US Sassuolo" = "Sassuolo",
        "Torino FC" = "Torino",
        "Watford FC" = "Watford",
        "Celta de Vigo" = "Celta Vigo",
        "UC Sampdoria" = "Sampdoria",
        "Getafe CF" = "Getafe",
        "Udinese Calcio" = "Udinese",
        "Burnley FC" = "Burnley",
        "Genoa CFC" = "Genoa",
        "VfB Stuttgart" = "Stuttgart",
        "Cagliari Calcio" = "Cagliari",
        "Bologna FC 1909" = "Bologna",
        "FC Girondins Bordeaux" = "Bordeaux",
        "SC Freiburg" = "Freiburg",
        "RCD Espanyol Barcelona" = "Espanyol",
        "SV Werder Bremen" = "Werder Bremen",
        "West Bromwich Albion" = "West Brom",
        "1.FSV Mainz 05" = "Mainz",
        "AS Saint-Étienne" = "Saint-Étienne",
        "RC Lens" = "Lens",
        "Sheffield United" = "Sheffield Utd",
        "Montpellier HSC" = "Montpellier",
        "FC Augsburg" = "Augsburg",
        "Huddersfield Town" = "Huddersfield",
        "Sunderland AFC" = "Sunderland",
        "Middlesbrough FC" = "Middlesbrough",
        "Parma Calcio 1913" = "Parma",
        "FC Empoli" = "Empoli",
        "Granada CF" = "Granada",
        "RC Strasbourg Alsace" = "Strasbourg",
        "FC Nantes" = "Nantes",
        "1. FC Köln" = "Köln",
        "1.FC Union Berlin" = "Union Berlin",
        "AC Monza" = "Monza",
        "CA Osasuna" = "Osasuna",
        "FC Toulouse" = "Toulouse",
        "Levante UD" = "Levante",
        "CD Leganés" = "Leganés",
        "Angers SCO" = "Angers",
        "FC Metz" = "Metz",
        "Stade Brestois 29" = "Stade Brest",
        "Girona FC" = "Girona",
        "Fortuna Düsseldorf" = "Düsseldorf",
        "SD Eibar" = "Eibar",
        "UD Las Palmas" = "Las Palmas",
        "Nîmes Olympique" = "Nîmes",
        "Real Valladolid CF" = "Real Valladolid",
        "Málaga CF" = "Málaga",
        "Brescia Calcio" = "Brescia",
        "US Salernitana 1919" = "Salernitana",
        "Venezia FC" = "Venezia",
        "UD Almería" = "Almería",
        "Spezia Calcio" = "Spezia",
        "Elche CF" = "Elche",
        "RCD Mallorca" = "Mallorca",
        "ESTAC Troyes" = "Troyes",
        "FC Lorient" = "Lorient",
        "EA Guingamp" = "Guingamp",
        "Amiens SC" = "Amiens",
        "Dijon FCO" = "Dijon",
        "Palermo FC" = "Palermo",
        "Cádiz CF" = "Cádiz",
        "SM Caen" = "Caen",
        "SD Huesca" = "Huesca",
        "Benevento Calcio" = "Benevento",
        "Chievo Verona" = "Chievo",
        "Clermont Foot 63" = "Clermont Foot",
        "US Cremonese" = "Cremonese",
        "Frosinone Calcio" = "Frosinone",
        "1.FC Nuremberg" = "Nürnberg",
        "US Lecce" = "Lecce",
        "FC Crotone" = "Crotone",
        "VfL Bochum" = "Bochum",
        "Athletic Carpi 2021" = "Carpi",
        "Delfino Pescara 1936" = "Pescara",
        "FC Ingolstadt 04" = "Ingolstadt",
        "SpVgg Greuther Fürth" = "Greuther Fürth",
        "AJ Auxerre" = "Auxerre",
        "SC Bastia" = "Bastia",
        "AS Nancy-Lorraine" = "Nancy",
        "SC Paderborn 07" = "Paderborn",
        "SV Darmstadt 98" = "Darmstadt",
        "Willem II Tilburg" = "Willem II",
        "Vitória Setúbal FC" = "Vitória Setúbal",
        "Vitória Guimarães SC" = "Vitória Guimarães",
        "Vitesse Arnhem" = "Vitesse",
        "Valenciennes FC" = "Valenciennes",
        "US Livorno 1915" = "Livorno",
        "Twente Enschede FC" = "Twente",
        "TSV Hartberg" = "Hartberg",
        "Thonon Évian Grand Genève FC" = "Evian",
        "SV Ried" = "Ried",
        "SV Mattersburg (-2020)" = "Mattersburg",
        "SV Grödig" = "Grödig",
        "SL Benfica" = "Benfica",
        "SKN St. Pölten" = "St. Pölten",
        "SK Sturm Graz" = "Sturm Graz",
        "SC Olhanense" = "Olhanense",
        "SC Heerenveen" = "Heerenveen",
        "SC Cambuur-Leeuwarden" = "Cambuur",
        "SC Braga" = "Braga",
        "SC Beira-Mar" = "Beira-Mar",
        "Roda JC Kerkrade" = "Roda",
        "Rio Ave FC" = "Rio Ave",
        "Red Bull Salzburg" = "RB Salzburg",
        "Reading FC" = "Reading",
        "Rapid Vienna" = "Rapid Wien",
        "Queens Park Rangers" = "QPR",
        "Portimonense SC" = "Portimonense",
        "PEC Zwolle" = "Zwolle",
        "Moreirense FC" = "Moreirense",
        "Gil Vicente FC" = "Gil Vicente",
        "GD Estoril Praia" = "Estoril",
        "GD Chaves" = "Chaves",
        "Feyenoord Rotterdam" = "Feyenoord",
        "FC Wacker Innsbruck" = "Wacker Innsbruck",
        "FC Utrecht" = "Utrecht",
        "FC Sochaux-Montbéliard" = "Sochaux",
        "FC Porto" = "Porto",
        "FC Paços de Ferreira" = "Paços",
        "FC Penafiel" = "Penafiel",
        "FC Groningen" = "Groningen",
        "FC Famalicão" = "Famalicão",
        "FC Emmen" = "Emmen",
        "FC Dordrecht" = "Dordrecht",
        "FC Arouca" = "Arouca",
        "FC Admira Wacker Mödling" = "Admira",
        "Excelsior Rotterdam" = "Excelsior",
        "Eintracht Braunschweig" = "Braunschweig",
        "Desportivo Aves (- 2020)" = "Aves",
        "De Graafschap Doetinchem" = "De Graafschap",
        "CS Marítimo" = "Marítimo",
        "Córdoba CF" = "Córdoba",
        "CF União Madeira" = "União",
        "CF Os Belenenses" = "Belenenses",
        "Cesena FC" = "Cesena",
        "CD Tondela" = "Tondela",
        "CD Santa Clara" = "Santa Clara",
        "CD Nacional" = "Nacional",
        "CD Feirense" = "Feirense",
        "Catania SSD" = "Catania",
        "Boavista FC" = "Boavista",
        "Belenenses SAD" = "Belenenses",
        "Austria Vienna" = "Austria Wien",
        "Ajax Amsterdam" = "Ajax",
        "ACR Siena 1904" = "Siena",
        "Académica Coimbra" = "Académica",
        "AC Carpi" = "Carpi",
        "1. Wiener Neustädter SC" = "Wiener Neustadt"
      ))

transfer_balances <-
  transfer_balances %>%
  mutate(
    squad =
      recode(
        squad,
        "Liverpool FC" = "Liverpool",
        "Chelsea FC" = "Chelsea",
        "FC Barcelona" = "Barcelona",
        "Atlético de Madrid" = "Atlético Madrid",
        "Tottenham Hotspur" = "Tottenham",
        "Juventus FC" = "Juventus",
        "Manchester United" = "Manchester Utd",
        "Arsenal FC" = "Arsenal",
        "SSC Napoli" = "Napoli",
        "AS Monaco" = "Monaco",
        "Everton FC" = "Everton",
        "Valencia CF" = "Valencia",
        "AS Roma" = "Roma",
        "Olympique Lyon" = "Lyon",
        "Bayer 04 Leverkusen" = "Bayer Leverkusen",
        "Wolverhampton Wanderers" = "Wolves",
        "Atalanta BC" = "Atalanta",
        "Sevilla FC" = "Sevilla",
        "Villarreal CF" = "Villarreal",
        "SS Lazio" = "Lazio",
        "West Ham United" = "West Ham",
        "TSG 1899 Hoffenheim" = "Hoffenheim",
        "Newcastle United" = "Newcastle",
        "ACF Fiorentina" = "Fiorentina",
        "LOSC Lille" = "Lille",
        "Brighton & Hove Albion" = "Brighton",
        "Eintracht Frankfurt" = "Frankfurt",
        "AFC Bournemouth" = "Bournemouth",
        "Southampton FC" = "Southampton",
        "Stade Rennais FC" = "Rennes",
        "Brentford FC" = "Brentford",
        "Real Betis Balompié" = "Real Betis",
        "Olympique Marseille" = "Marseille",
        "FC Schalke 04" = "Schalke",
        "VfL Wolfsburg" = "Wolfsburg",
        "OGC Nice" = "Nice",
        "Fulham FC" = "Fulham",
        "US Sassuolo" = "Sassuolo",
        "Torino FC" = "Torino",
        "Watford FC" = "Watford",
        "Celta de Vigo" = "Celta Vigo",
        "UC Sampdoria" = "Sampdoria",
        "Getafe CF" = "Getafe",
        "Udinese Calcio" = "Udinese",
        "Burnley FC" = "Burnley",
        "Genoa CFC" = "Genoa",
        "VfB Stuttgart" = "Stuttgart",
        "Cagliari Calcio" = "Cagliari",
        "Bologna FC 1909" = "Bologna",
        "FC Girondins Bordeaux" = "Bordeaux",
        "SC Freiburg" = "Freiburg",
        "RCD Espanyol Barcelona" = "Espanyol",
        "SV Werder Bremen" = "Werder Bremen",
        "West Bromwich Albion" = "West Brom",
        "1.FSV Mainz 05" = "Mainz",
        "AS Saint-Étienne" = "Saint-Étienne",
        "RC Lens" = "Lens",
        "Sheffield United" = "Sheffield Utd",
        "Montpellier HSC" = "Montpellier",
        "FC Augsburg" = "Augsburg",
        "Huddersfield Town" = "Huddersfield",
        "Sunderland AFC" = "Sunderland",
        "Middlesbrough FC" = "Middlesbrough",
        "Parma FC" = "Parma",
        "Empoli FC" = "Empoli",
        "Empoli FC " = "Empoli",
        "Granada CF" = "Granada",
        "RC Strasbourg Alsace" = "Strasbourg",
        "FC Nantes" = "Nantes",
        "1. FC Köln" = "Köln",
        "1.FC Union Berlin" = "Union Berlin",
        "AC Monza" = "Monza",
        "CA Osasuna" = "Osasuna",
        "FC Toulouse" = "Toulouse",
        "Levante UD" = "Levante",
        "CD Leganés" = "Leganés",
        "Angers SCO" = "Angers",
        "FC Metz" = "Metz",
        "Stade Brestois 29" = "Stade Brest",
        "Girona FC" = "Girona",
        "Fortuna Düsseldorf" = "Düsseldorf",
        "SD Eibar" = "Eibar",
        "UD Las Palmas" = "Las Palmas",
        "Nîmes Olympique" = "Nîmes",
        "Real Valladolid CF" = "Real Valladolid",
        "Málaga CF" = "Málaga",
        "Brescia Calcio" = "Brescia",
        "US Salernitana 1919" = "Salernitana",
        "Venezia FC" = "Venezia",
        "UD Almería" = "Almería",
        "Spezia Calcio" = "Spezia",
        "Elche CF" = "Elche",
        "RCD Mallorca" = "Mallorca",
        "ESTAC Troyes" = "Troyes",
        "FC Lorient" = "Lorient",
        "EA Guingamp" = "Guingamp",
        "Amiens SC" = "Amiens",
        "Dijon FCO" = "Dijon",
        "US Palermo" = "Palermo",
        "Cádiz CF" = "Cádiz",
        "SM Caen" = "Caen",
        "SD Huesca" = "Huesca",
        "Benevento Calcio" = "Benevento",
        "Chievo Verona" = "Chievo",
        "Clermont Foot 63" = "Clermont Foot",
        "US Cremonese" = "Cremonese",
        "Frosinone Calcio" = "Frosinone",
        "1.FC Nuremberg" = "Nürnberg",
        "US Lecce" = "Lecce",
        "FC Crotone" = "Crotone",
        "VfL Bochum" = "Bochum",
        "Athletic Carpi 2021" = "Carpi",
        "Delfino Pescara 1936" = "Pescara",
        "FC Ingolstadt 04" = "Ingolstadt",
        "SpVgg Greuther Fürth" = "Greuther Fürth",
        "AJ Auxerre" = "Auxerre",
        "SC Bastia" = "Bastia",
        "AS Nancy-Lorraine" = "Nancy",
        "SC Paderborn 07" = "Paderborn",
        "SV Darmstadt 98" = "Darmstadt",
        "Willem II Tilburg" = "Willem II",
        "Vitória Setúbal FC" = "Vitória Setúbal",
        "Vitória Guimarães SC" = "Vitória Guimarães",
        "Vitesse Arnhem" = "Vitesse",
        "Valenciennes FC" = "Valenciennes",
        "AS Livorno" = "Livorno",
        "Twente Enschede FC" = "Twente",
        "TSV Hartberg" = "Hartberg",
        "FC Évian Thonon Gaillard" = "Evian",
        "SV Ried" = "Ried",
        "SV Mattersburg" = "Mattersburg",
        "SV Grödig" = "Grödig",
        "SL Benfica" = "Benfica",
        "SKN St. Pölten" = "St. Pölten",
        "SK Sturm Graz" = "Sturm Graz",
        "SC Olhanense" = "Olhanense",
        "SC Heerenveen" = "Heerenveen",
        "SC Cambuur-Leeuwarden" = "Cambuur",
        "SC Braga" = "Braga",
        "SC Beira-Mar" = "Beira-Mar",
        "Roda JC Kerkrade" = "Roda",
        "Rio Ave FC" = "Rio Ave",
        "Red Bull Salzburg" = "RB Salzburg",
        "Reading FC" = "Reading",
        "Rapid Vienna" = "Rapid Wien",
        "Queens Park Rangers" = "QPR",
        "Portimonense SC" = "Portimonense",
        "PEC Zwolle" = "Zwolle",
        "Moreirense FC" = "Moreirense",
        "Gil Vicente FC" = "Gil Vicente",
        "GD Estoril Praia" = "Estoril",
        "GD Chaves" = "Chaves",
        "Feyenoord Rotterdam" = "Feyenoord",
        "FC Wacker Innsbruck" = "Wacker Innsbruck",
        "FC Utrecht" = "Utrecht",
        "FC Sochaux-Montbéliard" = "Sochaux",
        "FC Porto" = "Porto",
        "FC Paços de Ferreira" = "Paços",
        "FC Penafiel" = "Penafiel",
        "FC Groningen" = "Groningen",
        "FC Famalicão" = "Famalicão",
        "FC Emmen" = "Emmen",
        "FC Dordrecht" = "Dordrecht",
        "FC Arouca" = "Arouca",
        "FC Admira Wacker Mödling" = "Admira",
        "Excelsior Rotterdam" = "Excelsior",
        "Eintracht Braunschweig" = "Braunschweig",
        "Desportivo Aves (- 2020)" = "Aves",
        "De Graafschap Doetinchem" = "De Graafschap",
        "CS Marítimo" = "Marítimo",
        "Córdoba CF" = "Córdoba",
        "CF União Madeira" = "União",
        "CF Os Belenenses" = "Belenenses",
        "AC Cesena" = "Cesena",
        "CD Tondela" = "Tondela",
        "CD Santa Clara" = "Santa Clara",
        "CD Nacional" = "Nacional",
        "CD Feirense" = "Feirense",
        "Calcio Catania" = "Catania",
        "Boavista FC" = "Boavista",
        "Belenenses SAD" = "Belenenses",
        "Austria Vienna" = "Austria Wien",
        "Ajax Amsterdam" = "Ajax",
        "AC Siena" = "Siena",
        "Académica Coimbra" = "Académica",
        "Carpi FC 1909" = "Carpi",
        "SC Wiener Neustadt" = "Wiener Neustadt",
        "SPAL 2013" = "SPAL",
        "FC Internazionale" = "Inter Milan",
        "FC Évian Thonon Gaillard" = "Evian"
      ))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine Datasets ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tm_combined <-
  full_join(squad_values, transfer_balances)

tm_combined <- 
  left_join(tm_combined, injuries) %>%
  mutate(days_injured = tidyr::replace_na(days_injured, 0))

fb_combined <-
  full_join(league_tables, num_players)

club_resources <- full_join(fb_combined, tm_combined)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save Final Dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

readr::write_rds(club_resources, file = here::here("data", "club_resources.rds"))
