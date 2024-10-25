# Script for pulling and combining raw data from FB Ref and Transfermarkt

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fix_team_names <- 
  function(data) {
    dplyr::case_when(
      data == "1.FC Heidenheim 1846" ~ "Heidenheim",
      data == "1.FC Kaiserslautern" ~ "Kaiserslautern",
      data == "1.FC Köln" ~ "Köln",
      data == "1.FC Nuremberg" ~ "Nürnberg",
      data == "1.FC Union Berlin" ~	"Union Berlin",
      data == "1.FSV Mainz 05" ~ "Mainz",
      data == "Académica Coimbra" ~ "Académica",
      data == "AC Carpi" ~ "Carpi",
      data == "AC Cesena" ~ "Cesena",
      data == "AC Monza" ~ "Monza",
      data == "AC Siena" ~ "Siena",
      data == "ACF Fiorentina" ~ "Fiorentina",
      data == "ACR Siena 1904" ~ "Siena",
      data == "AFC Bournemouth" ~ "Bournemouth",
      data == "AJ Auxerre" ~ "Auxerre",
      data == "AC Ajaccio" ~ "Ajaccio",
      data == "Ajaccio AC" ~ "Ajaccio",
      data == "Ajax Amsterdam" ~ "Ajax",
      data == "Alavés" ~ "Deportivo Alavés",
      data == "Almere City FC" ~ "Almere City",
      data == "Amiens SC" ~ "Amiens",
      data == "Angers SCO" ~ "Angers",
      data == "Athlétic Club Arlésien" ~ "Arlésien",
      data == "Arles-Avignon" ~ "Arlésien",
      data == "Arminia" ~ "Arminia Bielefeld",
      data == "FC Arouca" ~ "Arouca",
      data == "Arsenal FC" ~ "Arsenal",
      data == "AS Livorno" ~ "Livorno",
      data == "AS Monaco" ~ "Monaco",
      data == "AS Nancy-Lorraine" ~ "Nancy",
      data == "AS Roma" ~ "Roma",
      data == "AS Saint-Étienne" ~ "Saint-Étienne",
      data == "Associação Naval 1893" ~ "Naval",
      data == "Atalanta BC" ~ "Atalanta",
      data == "Athletic Carpi 2021" ~ "Carpi",
      data == "Athletic Club" ~ "Athletic Bilbao",
      data == "Atlético de Madrid" ~ "Atlético Madrid",
      data == "B SAD" ~ "B-SAD",
      data == "Bayer 04 Leverkusen" ~ "Bayer Leverkusen",
      data == "Benevento Calcio" ~ "Benevento",
      data == "Betis" ~ "Real Betis",
      data == "Blackburn" ~ "Blackburn Rovers",
      data == "Blackpool FC" ~ "Blackpool",
      data == "Boavista FC" ~ "Boavista",
      data == "Bologna FC 1909" ~ "Bologna",
      data == "Bolton" ~ "Bolton Wanderers",
      data == "US Boulogne" ~ "Boulogne",
      data == "Brentford FC" ~ "Brentford",
      data == "Brescia Calcio" ~ "Brescia",
      data == "Brest" ~ "Stade Brest",
      data == "Brighton & Hove Albion" ~ "Brighton",
      data == "Burnley FC" ~ "Burnley",
      data == "CA Osasuna" ~ "Osasuna",
      data == "Cádiz CF" ~ "Cádiz",
      data == "Cagliari Calcio" ~	"Cagliari",
      data == "Calcio Catania" ~ "Catania",
      data == "Carpi FC 1909" ~ "Carpi",
      data == "Casa Pia AC" ~ "Casa Pia",
      data == "Catania FC" ~ "Catania",
      data == "Catania SSD" ~ "Catania",
      data == "CD Feirense" ~ "Feirense",
      data == "CD Leganés" ~ "Leganés",
      data == "CD Nacional" ~ "Nacional",
      data == "CD Numancia" ~ "Numancia",
      data == "CD Santa Clara" ~ "Santa Clara",
      data == "CD Tenerife" ~ "Tenerife",
      data == "CD Tondela" ~ "Tondela",
      data == "CD Trofense" ~ "Trofense",
      data == "CF Estrela Amadora" ~ "Estrela",
      data == "CF União Madeira (-2021)" ~ "União",
      data == "Celta de Vigo" ~ "Celta Vigo",
      data == "Cesena FC" ~ "Cesena",
      data == "CF Os Belenenses" ~ "Belenenses",
      data == "Chelsea FC" ~ "Chelsea",
      data == "Chievo Verona" ~ "Chievo",
      data == "Clermont Foot 63" ~ "Clermont Foot",
      data == "Córdoba CF" ~ "Córdoba",
      data == "CS Marítimo" ~ "Marítimo",
      data == "Darmstadt 98" ~ "Darmstadt",
      data == "De Graafschap Doetinchem" ~ "De Graafschap",
      data == "Delfino Pescara 1936" ~ "Pescara",
      data == "Desportivo Aves (- 2020)" ~ "Aves",
      data == "Dijon FCO" ~ "Dijon",
      data == "Dortmund" ~ "Borussia Dortmund",
      data == "EA Guingamp" ~ "Guingamp",
      data == "Eint Frankfurt" ~ "Frankfurt",
      data == "Eintracht Braunschweig" ~ "Braunschweig",
      data == "Eintracht Frankfurt" ~ "Frankfurt",
      data == "Elche CF" ~ "Elche",
      data == "Empoli FC" ~ "Empoli",
      data == "ESTAC Troyes" ~ "Troyes",
      data == "Everton FC" ~ "Everton",
      data == "FC Augsburg" ~ "Augsburg",
      data == "FC Barcelona" ~ "Barcelona",
      data == "FC Crotone" ~ "Crotone",
      data == "FC Dordrecht" ~ "Dordrecht",
      data == "FC Emmen" ~ "Emmen",
      data == "FC Empoli" ~ "Empoli",
      data == "FC Energie Cottbus" ~ "Energie Cottbus",
      data == "Estoril Praia" ~ "Estoril", 
      data == "Excelsior Rotterdam" ~ "Excelsior",
      data == "FC Évian Thonon Gaillard" ~ "Evian",
      data == "FC Girondins Bordeaux" ~ "Bordeaux",
      data == "FC Famalicão" ~ "Famalicão",
      data == "FC Groningen" ~ "Groningen",
      data == "FC Paços de Ferreira" ~ "Paços",
      data == "FC Penafiel" ~ "Penafiel",
      data == "FC Porto" ~ "Porto",
      data == "FC St. Pauli" ~ "St. Pauli",
      data == "FC Utrecht" ~ "Utrecht",
      data == "FC Vizela" ~ "Vizela",
      data == "FC Volendam" ~ "Volendam",
      data == "FC Ingolstadt 04" ~ "Ingolstadt",
      data == "FC Internazionale" ~ "Inter Milan",
      data == "FC Lorient" ~ "Lorient",
      data == "FC Metz" ~ "Metz",
      data == "FC Nantes" ~ "Nantes",
      data == "FC Schalke 04" ~ "Schalke",
      data == "FC Sochaux-Montbéliard" ~ "Sochaux",
      data == "FC Toulouse" ~ "Toulouse",
      data == "Feyenoord Rotterdam" ~ "Feyenoord",
      data == "Fortuna Düsseldorf" ~ "Düsseldorf",
      data == "Frosinone Calcio" ~ "Frosinone",
      data == "Fulham FC" ~ "Fulham",
      data == "Gazélec Ajaccio" ~ "Ajaccio",
      data == "Gazélec Ajaccio GFC" ~ "Ajaccio",
      data == "Genoa CFC" ~ "Genoa",
      data == "Getafe CF" ~ "Getafe",
      data == "GD Chaves" ~ "Chaves",
      data == "GFC Ajaccio" ~ "Ajaccio",
      data == "Gil Vicente FC" ~ "Gil Vicente",
      data == "Girona FC" ~ "Girona",
      data == "Go Ahead Eag" ~ "Go Ahead Eagles",
      data == "Grenoble Foot 38" ~ "Grenoble",
      data == "Granada CF" ~ "Granada",
      data == "Hércules CF" ~ "Hércules",
      data == "Huddersfield Town" ~ "Huddersfield",
      data == "Ingolstadt 04" ~ "Ingolstadt",
      data == "Inter" ~ "Inter Milan",
      data == "Juventus FC" ~ "Juventus",
      data == "Karlsruher SC" ~ "Karlsruher",
      data == "La Coruña" ~ "Deportivo de La Coruña",
      data == "Le Havre AC" ~ "Le Havre",
      data == "Le Mans FC" ~ "Le Mans",
      data == "Leixões SC" ~ "Leixões",
      data == "Levante UD" ~ "Levante",
      data == "Leverkusen" ~ "Bayer Leverkusen",
      data == "Liverpool FC" ~ "Liverpool",
      data == "LOSC Lille" ~ "Lille",
      data == "Mainz 05" ~ "Mainz",
      data == "Málaga CF" ~ "Málaga",
      data == "Manchester United" ~ "Manchester Utd",
      data == "M'Gladbach" ~ "Borussia Mönchengladbach",
      data == "Gladbach" ~ "Borussia Mönchengladbach",
      data == "Middlesbrough FC" ~ "Middlesbrough",
      data == "Milan" ~ "AC Milan",
      data == "Moreirense FC" ~ "Moreirense",
      data == "Montpellier HSC" ~ "Montpellier",
      data == "Newcastle United" ~ "Newcastle",
      data == "Newcastle Utd" ~ "Newcastle",
      data == "Nîmes Olympique" ~ "Nîmes",
      data == "Nott'ham Forest" ~ "Nottingham Forest",
      data == "Novara FC" ~ "Novara",
      data == "OGC Nice" ~ "Nice",
      data == "Olympique Lyon" ~ "Lyon",
      data == "Olympique Marseille" ~ "Marseille",
      data == "PEC Zwolle" ~ "Zwolle",
      data == "Paços de Ferreira" ~ "Pacos",
      data == "Paderborn 07" ~ "Paderborn",
      data == "Palermo FC" ~ "Palermo",
      data == "Paris S-G" ~ "Paris Saint-Germain",
      data == "Parma Calcio 1913" ~ "Parma",
      data == "Parma FC" ~ "Parma",
      data == "Portimonense SC" ~ "Portimonense",
      data == "Portsmouth FC" ~ "Portsmouth",
      data == "Queens Park Rangers" ~ "QPR",
      data == "Racing Sant" ~ "Racing Santander",
      data == "RC Lens" ~ "Lens",
      data == "RC Strasbourg Alsace" ~ "Strasbourg",
      data == "RCD Espanyol Barcelona" ~ "Espanyol",
      data == "RCD Mallorca" ~ "Mallorca",
      data == "Reading FC" ~ "Reading",
      data == "Real Betis Balompié" ~ "Real Betis",
      data == "Real Valladolid CF" ~ "Real Valladolid",
      data == "Recreativo Huelva" ~ "Recreativo",
      data == "Reims" ~ "Stade Reims",
      data == "Reggina 1914" ~ "Reggina",
      data == "Rio Ave FC" ~ "Rio Ave",
      data == "Roda JC Kerkrade" ~ "Roda JC",
      data == "SC Bastia" ~ "Bastia",
      data == "SC Beira-Mar" ~ "Beira-Mar",
      data == "SC Braga" ~ "Braga",
      data == "SC Cambuur Leeuwarden" ~ "Cambuur",
      data == "SC Farense" ~ "Farense",
      data == "SC Freiburg" ~ "Freiburg",
      data == "SC Heerenveen" ~ "Heerenveen",
      data == "SC Olhanense" ~ "Olhanense",
      data == "SC Paderborn 07" ~ "Paderborn",
      data == "Schalke 04" ~ "Schalke",
      data == "SD Eibar" ~ "Eibar",
      data == "SD Huesca" ~ "Huesca",
      data == "Sevilla FC" ~ "Sevilla",
      data == "Sheffield United" ~ "Sheffield Utd",
      data == "Siena FC" ~ "Siena",
      data == "SL Benfica" ~ "Benfica",
      data == "SM Caen" ~ "Caen",
      data == "Southampton FC" ~ "Southampton",
      data == "SPAL 2013" ~ "SPAL",
      data == "Sparta R'dam" ~ "Sparta Rotterdam",
      data == "Spezia Calcio" ~ "Spezia",
      data == "SpVgg Greuther Fürth" ~ "Greuther Fürth",
      data == "SS Lazio" ~ "Lazio",
      data == "SSC Bari" ~ "Bari",
      data == "SSC Napoli" ~ "Napoli",
      data == "Stade Brestois 29" ~ "Stade Brest",
      data == "Stade Rennais FC" ~ "Rennes",
      data == "Sunderland AFC" ~ "Sunderland",
      data == "SV Darmstadt 98" ~ "Darmstadt",
      data == "SV Werder Bremen" ~ "Werder Bremen",
      data == "Thonon Évian Grand Genève FC" ~ "Evian",
      data == "Torino FC" ~ "Torino",
      data == "Tottenham Hotspur" ~ "Tottenham",
      data == "TSG 1899 Hoffenheim" ~ "Hoffenheim",
      data == "Twente Enschede FC" ~ "Twente",
      data == "UC Sampdoria" ~ "Sampdoria",
      data == "UD Almería" ~ "Almería",
      data == "UD Las Palmas" ~ "Las Palmas",
      data == "Udinese Calcio" ~ "Udinese",
      data == "US Cremonese" ~ "Cremonese",
      data == "US Lecce" ~ "Lecce",
      data == "US Livorno 1915" ~ "Livorno",
      data == "US Palermo" ~ "Palermo",
      data == "US Salernitana 1919" ~ "Salernitana",
      data == "US Sassuolo" ~ "Sassuolo",
      data == "Valencia CF" ~ "Valencia",
      data == "Valenciennes FC" ~ "Valenciennes",
      data == "Valladolid" ~ "Real Valladolid",
      data == "Venezia FC" ~ "Venezia",
      data == "VfB Stuttgart" ~ "Stuttgart",
      data == "VfL Bochum" ~ "Bochum",
      data == "VfL Wolfsburg" ~ "Wolfsburg",
      data == "Villarreal CF" ~ "Villarreal",
      data == "Vitesse Arnhem" ~ "Vitesse",
      data == "Vitória Guimarães SC" ~ "Vitória",
      data == "Vitória Setúbal FC" ~ "Vitória Setúbal",
      data == "Watford FC" ~ "Watford",
      data == "West Bromwich Albion" ~ "West Brom",
      data == "West Ham United" ~ "West Ham",
      data == "Willem II Tilburg" ~ "Willem II",
      data == "Wolverhampton Wanderers" ~ "Wolves",
      data == "Xerez CD" ~ "Xerez",
      data == "Zaragoza" ~ "Real Zaragoza",
      .default = data
    )
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Squad Values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get player values from transfermarkt
player_values_raw <- 
  worldfootballR::tm_player_market_values(
    country_name = c("England", "Spain", "Germany", "Italy", "France", "Portugal", "Netherlands"),
    start_year = c(2008:2023)
  )

readr::write_csv(
  player_values_raw, 
  here::here("data", "player_values.csv")
)

# wrangle to team total values per season?
squad_values <- 
  player_values_raw |>
  dplyr::mutate(
    league = comp_name,
    league = dplyr::case_when(
      league == "LaLiga" ~ "La Liga",
      league == "Liga Portugal" ~ "Primeira Liga",
      .default = league
    ),
    season = season_start_year
  ) |> 
  tidyr::drop_na(player_market_value_euro) |>
  dplyr::summarise(
    squad_value = sum(player_market_value_euro),
    .by = c(squad, league, season)
  ) |> 
  dplyr::mutate(
    squad = fix_team_names(squad)
    )

readr::write_csv(
  squad_values, 
  here::here("data", "squad_values.csv")
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# League Tables ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get league tables from fb ref
league_table_raw <- 
  worldfootballR::fb_season_team_stats(
    country = c("ENG", "ESP", "GER", "ITA", "FRA", "POR", "NED"),
    gender = "M",
    season_end_year = c(2009:2024),
    tier = "1st",
    stat_type = "league_table",
    time_pause = 5
  )

# wrangle league tables to merge with squad values
league_tables <-
  league_table_raw |>
  janitor::clean_names(
    replace = c(
      "xGD" = "xgd",
      "xGA"= "xga",
      "xG" = "xg"
    )
  ) |>
  dplyr::mutate(
    season = season_end_year - 1,
    league =
      dplyr::case_when(
        competition_name == "Fußball-Bundesliga" ~ "Bundesliga",
        .default = competition_name
      ),
    squad = fix_team_names(squad)
    ) |> 
  dplyr::select(league, squad, season, rk, mp, pts)  # gf, ga, gd, xg, xga, xgd)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine Datasets ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

club_resources <- 
  dplyr::full_join(squad_values, league_tables) |> 
  dplyr::mutate(
    season =
      forcats::as_factor(
        glue::glue(
          "{season}/{as.numeric(stringr::str_sub(season, start = -2)) + 1}"
        )
      )
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save Final Dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

readr::write_rds(
  club_resources, 
  file = here::here(
    "data", "club_resources.rds"
  )
)
