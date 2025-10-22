library(dplyr)
library(tidyr)
library(stringr)

path <- here::here()
path_lib <- paste0(path,"/lib/")

# 0) Prepping --------------------------------
# State 
state_map <- list(
  NSW = c(
    "Blue Mountains", "Central Coast", "Central NSW",
    "Hunter", "New England North West", "North Coast NSW", "Outback NSW",
    "Riverina", "Snowy Mountains", "South Coast", "Sydney",
    "Capital Country", "The Murray",
    "Canberra" # ACT is included in NSW for simplicity
  ),
  VIC = c(
    "Ballarat", "Bendigo Loddon", "Central Murray", "Geelong and the Bellarine",
    "Gippsland", "Great Ocean Road", "High Country", "Macedon", "Mallee",
    "Melbourne", "Melbourne East", "Murray East", "Peninsula", "Phillip Island",
    "Spa Country", "Upper Yarra", "Western Grampians", "Wimmera",
    "Central Highlands", "Lakes", "Goulburn"
  ),
  QLD = c(
    "Brisbane", "Bundaberg", "Capricorn", "Fraser Coast", "Gladstone",
    "Gold Coast", "Mackay", "Outback Queensland", "Southern Queensland Country",
    "Sunshine Coast", "Townsville", "Tropical North Queensland",
    "Whitsundays"
  ),
  SA = c(
    "Adelaide", "Adelaide Hills", "Barossa", "Clare Valley", "Eyre Peninsula",
    "Fleurieu Peninsula", "Flinders Ranges and Outback", "Kangaroo Island",
    "Limestone Coast", "Murray River, Lakes and Coorong", "Riverland",
    "Yorke Peninsula"
  ),
  WA = c(
    "Australia's Coral Coast", "Australia's Golden Outback",
    "Australia's North West", "Australia's South West", "Destination Perth"
  ),
  TAS = c(
    "East Coast", "Hobart and the South",
    "Launceston and the North", "North West", "West Coast"
  ),
  NT = c(
    "Alice Springs", "Barkly", "Darwin", "Katherine Daly",
    "Litchfield Kakadu Arnhem", "Lasseter", "MacDonnell"
  )
)
region_state_lookup <- tibble(
  State  = rep(names(state_map), lengths(state_map)),
  Region = unlist(state_map)
)
# Zone 
zone_map <- list(
  # NSW
  `Metro NSW`        = c("Sydney", "Central Coast"),
  `Nth Coast NSW`    = c("Hunter", "North Coast NSW"),
  `Sth Coast NSW`    = "South Coast",
  `Sth NSW`          = c("Snowy Mountains", "Capital Country", "The Murray", "Riverina"),
  `Nth NSW`          = c("Central NSW", "New England North West", "Outback NSW", "Blue Mountains"),
  ACT                = "Canberra",
  # VIC
  `Metro VIC`        = c("Melbourne", "Peninsula", "Geelong and the Bellarine"),
  `West Coast VIC`   = "Great Ocean Road",
  `East Coast VIC`   = c("Lakes", "Gippsland", "Phillip Island"),
  `Nth East VIC`     = c("Central Murray", "Goulburn", "High Country",
                        "Melbourne East", "Upper Yarra", "Murray East"),
  `Nth West VIC`     = c("Wimmera", "Western Grampians", "Bendigo Loddon",
                        "Macedon", "Spa Country", "Ballarat", "Mallee",
                        "Central Highlands"),
  # QLD
  `Metro QLD`        = c("Gold Coast", "Brisbane", "Sunshine Coast"),
  `Central Coast QLD`= c("Bundaberg", "Fraser Coast", "Mackay", 
                        "Southern Queensland Country", "Capricorn", "Gladstone"),
  `Nth Coast QLD`    = c("Whitsundays", "Tropical North Queensland", "Townsville"),
  `Inland QLD`       = c("Outback Queensland"),
  # SA
  `Metro SA`         = c("Adelaide", "Barossa", "Adelaide Hills"),
  `Sth Coast SA`     = c("Limestone Coast", "Fleurieu Peninsula", "Kangaroo Island"),
  `Inland SA`        = c("Riverland", "Clare Valley", "Flinders Ranges and Outback",
                        "Murray River, Lakes and Coorong"),
  `West Coast SA`    = c("Eyre Peninsula", "Yorke Peninsula"),
  # WA
  `West Coast WA`    = c("Destination Perth", "Australia's South West", "Australia's Coral Coast"),
  `Nth WA`           = "Australia's North West",
  `Sth WA`           = "Australia's Golden Outback",
  # TAS
  `Sth TAS`          = "Hobart and the South",
  `Nth East TAS`     = c("East Coast", "Launceston and the North"),
  `Nth West TAS`     = c("North West", "West Coast"),
  # NT
  `Nth Coast NT`     = c("Darwin", "Litchfield Kakadu Arnhem", "Katherine Daly"),
  `Central NT`       = c("Barkly", "Lasseter", "Alice Springs", "MacDonnell")
)
region_zone_lookup <- tibble(
  Zone   = rep(names(zone_map), lengths(zone_map)),
  Region = unlist(zone_map)
)

level_map <- readRDS(paste0(path_lib, "level_info.rds"))$map
# only geo
table_geo <- level_map[str_sub(level_map, -7, -1) != "Purpose"]
table_geo <- tibble(
  Series = as.character(1:length(table_geo)),
  Name = names(table_geo),
  Level = table_geo
)
table_geo[1,2:3] <- table_geo[1,3:2]


# 1) Derive Zone -> State map -----------------------------

zone_state_lookup <- region_zone_lookup %>%
  left_join(region_state_lookup, by = "Region") %>%
  distinct(Zone, State) %>%
  group_by(Zone) %>%
  summarise(State = dplyr::n_distinct(State) %>% 
              {if (. != 1) stop("A zone maps to multiple states — check your maps.") ; 
                first(region_state_lookup$State[match(Zone, region_zone_lookup$Zone)])},
            .groups = "drop") %>%
  # The summarise above returns same column name "State" (the first() result).
  # Clean up to keep (Zone, State) unique:
  transmute(Zone, State = State)

# 2) State letters (A, B, C, …) -----------------------------
#    Use the order of states as shown in table_geo

state_letters <- table_geo %>%
  filter(Level == "State") %>%
  distinct(State = Name) %>%
  arrange(State) %>%  # change to your preferred ordering; 
                      # use `arrange(match(State, c("NSW","VIC",...)))` if needed
  mutate(state_letter = LETTERS[row_number()])

# 3) Zone letters within State (A, B, C, …) -----------------------------
#    Assign alphabetically by Zone name within each State

zone_letters <- table_geo %>%
  filter(Level == "Zone") %>%
  transmute(Zone = Name) %>%
  left_join(zone_state_lookup, by = "Zone") %>%
  left_join(state_letters, by = "State") %>%
  group_by(State) %>%
  arrange(Zone, .by_group = TRUE) %>%
  mutate(zone_letter = LETTERS[row_number()],
          zone_code   = paste0(state_letter, zone_letter)) %>%
  ungroup() %>%
  select(Zone, State, state_letter, zone_letter, zone_code)

# 4) Region letters within Zone (A, B, C, …) -----------------------------
#    Assign alphabetically by Region name within each Zone

region_letters <- table_geo %>%
  filter(Level == "Region") %>%
  transmute(Region = Name) %>%
  left_join(region_zone_lookup, by = "Region") %>%
  left_join(zone_letters, by = "Zone") %>%
  group_by(Zone) %>%
  arrange(Region, .by_group = TRUE) %>%
  mutate(region_letter = LETTERS[row_number()],
          region_code   = paste0(state_letter, zone_letter, region_letter)) %>%
  ungroup() %>%
  select(Region, Zone, State, region_code)

create_table_geo <- function() {

  table_geo_labeled <- table_geo %>%
    # state codes
    left_join(state_letters %>% transmute(Name = State, state_code = state_letter), by = "Name") %>%
    # zone codes
    left_join(zone_letters  %>% transmute(Name = Zone,  zone_code),  by = "Name") %>%
    # region codes
    left_join(region_letters %>% transmute(Name = Region, region_code), by = "Name") %>%
    mutate(
      Label = dplyr::case_when(
        Level == "State"  ~ state_code,
        Level == "Zone"   ~ zone_code,
        Level == "Region" ~ region_code,
        TRUE              ~ NA_character_   # e.g., "Total"
      )
    ) %>%
    select(Label, Series, Name, Level)
  
  table_geo_labeled
}

