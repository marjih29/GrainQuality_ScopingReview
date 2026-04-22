# Load packages
pkgs <- c(
  "httr", "jsonlite", "dplyr", "purrr", "readr", "writexl",
  "stringr", "tibble", "tidyr", "ontologyIndex",
  "fuzzyjoin", "stringdist", "hunspell", "rlang", "rdflib", "furrr", "future", "data.tree"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


# Set up

## Functions

load_dict <- function(path) read.csv(path, stringsAsFactors = FALSE)

clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_squish()
}

apply_dict_one <- function(text, dict_tbl,
                           pattern_col = "pattern",
                           value_col = "canonical",
                           ignore_case = TRUE) {
  if (is.na(text) || str_squish(text) == "") {
    return(NA_character_)
  }

  x <- str_squish(as.character(text))

  hits <- dict_tbl[[value_col]][
    str_detect(
      string = x,
      pattern = regex(dict_tbl[[pattern_col]], ignore_case = ignore_case)
    )
  ]

  hits <- unique(hits[!is.na(hits) & hits != ""])

  if (length(hits) == 0) {
    return(x)
  } else {
    return(hits[1])
  }
}

apply_dict <- function(text, dict_tbl, col = "canonical") {
  if (is.na(text) || str_squish(text) == "") {
    return(NA_character_)
  }

  x <- str_squish(as.character(text))

  hits <- dict_tbl[[col]][
    str_detect(
      string = x,
      pattern = regex(dict_tbl$pattern, ignore_case = TRUE)
    )
  ]

  hits <- unique(hits[!is.na(hits) & hits != ""])
  if (length(hits) == 0) NA_character_ else hits[1]
}

parse_preference_pos <- function(x) {
  tibble(Preference_pos = x) %>%
    mutate(
      Preference_pos = str_remove_all(
        Preference_pos,
        regex(",?\\s*(?:not|no)\\s+[\\w]+(?:\\s+or\\s+[\\w]+)*", ignore_case = TRUE)
      ),
      Preference_pos = str_trim(Preference_pos)
    ) %>%
    separate_rows(
      Preference_pos,
      sep = "(?i),\\s*(?=white|red|brown|black|yellow|grey|gray|pink|tan|dark|light|cream|pale|ivory)"
    ) %>%
    separate_rows(
      Preference_pos,
      sep = "(?i)\\s+and\\s+(?=white|red|brown|black|yellow|grey|gray|pink|tan|dark|light|cream|pale|ivory)"
    ) %>%
    separate_rows(Preference_pos, sep = ";") %>%
    separate_rows(Preference_pos, sep = "/") %>%
    mutate(Preference_pos = str_trim(Preference_pos)) %>%
    filter(!is.na(Preference_pos), Preference_pos != "")
}


## Dictionaries
trait_dictionary <- load_dict("Dictionaries/trait_dictionary.csv")
analysis_dictionary <- load_dict("Dictionaries/analysis_dictionary.csv")
methods_dictionary <- load_dict("Dictionaries/methods_dictionary.csv")
software_dictionary <- load_dict("Dictionaries/software_dictionary.csv")
vc_dictionary <- load_dict("Dictionaries/vc_dictionary.csv")
crop_dictionary <- load_dict("Dictionaries/crop_dictionary.csv")
country_dictionary <- load_dict("Dictionaries/country_dictionary.csv")
pubtype_dictionary <- load_dict("Dictionaries/pubtype_dictionary.csv")
prefstate_dictionary <- load_dict("Dictionaries/pref_dictionary.csv")
prefdir_dictionary <- load_dict("Dictionaries/prefdirection.csv")

trait_dict_map <- list(
  Location = country_dictionary,
  Crop = crop_dictionary,
  `VC Actor` = vc_dictionary,
  Trait = trait_dictionary
)

method_dict_map <- list(
  Country = country_dictionary,
  Analysis = analysis_dictionary,
  Software = software_dictionary
)


## Load and clean data
df <- read_csv("Extraction_Export_Raw.csv")

long_df <- df %>%
  rename(StudyID = `Covidence #`) %>%
  pivot_longer(
    cols = matches("^\\d+\\s"),
    names_to = c("entry_id", "field"),
    names_pattern = "(\\d+)\\s(.+)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = field,
    values_from = value
  )

meta_df <- df[, -c(25:95)]

formethods <- meta_df %>%
  dplyr::select(-c(DOI, `Reviewer Name`, Title...6, `Journal/Publisher`, `Location(s)`, `List any specific scales or methods named`)) %>%
  rename(Title = Title...3, Methods = `How were preferences determined?`, Analysis = `How was data analyzed?`, Software = `Was a statistical software used?`, VCs = `Value chain actors included`, Traits = `Grain quality traits included`, Crops = `Crop(s) studied`) %>%
  mutate(Country = gsub("Ethopia", "Ethiopia", Country)) %>%
  rename(StudyID = `Covidence #`) %>%
  dplyr::select(StudyID, Country, Methods, Analysis, Software, VCs, Traits, Crops) %>%
  separate_rows(Analysis, sep = ", ") %>%
  separate_rows(Software, sep = "; ")

fortraits <- long_df %>%
  filter(if_any(c(Trait, Preference), ~ !is.na(.) & . != "")) %>%
  mutate(across(c(Crop, Location, `VC Actor`, Trait, Preference), ~ str_trim(as.character(.)))) %>%
  dplyr::select(StudyID, Location, Crop, `VC Actor`, Trait, Preference, Disaggregation) %>%
  separate_rows(Location, sep = ", ") %>%
  separate_rows(`VC Actor`, sep = ", ") %>%
  separate_rows(`VC Actor`, sep = " ; ") %>%
  separate_rows(Crop, sep = ", ") %>%
  separate_rows(Crop, sep = " and ") %>%
  mutate(Location = gsub("^(and|also)\\s+", "", Location))

prefs <- read.csv("myprefs.csv") %>%
  unique()


dictionary <- read.csv(
  "hierarchy_df.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Methods analysis
coded_methods <- formethods

for (col in names(method_dict_map)) {
  new_col <- paste0(make.names(col), "_std")
  coded_methods[[new_col]] <- purrr::map_chr(
    coded_methods[[col]],
    ~ apply_dict_one(.x, method_dict_map[[col]])
  )
}


dictionary_clean <- dictionary %>%
  mutate(
    matched_term = ifelse(
      is.na(raw_term) | raw_term == "",
      category,
      raw_term
    )
  ) %>%
  dplyr::select(domain, matched_term, category, group) %>%
  distinct(domain, matched_term, .keep_all = TRUE)

domain_map <- list(
  Analysis = "Analysis_std",
  Software = "Software_std",
  Country  = "Country_std"
)

result <- coded_methods

for (d in names(domain_map)) {
  
  col_name <- domain_map[[d]]
  
  dict_sub <- dictionary_clean %>%
    filter(domain == d)
  
  # exact match first
  exact <- result %>%
    left_join(
      dict_sub,
      by = setNames("matched_term", col_name)
    )
  
  # unmatched rows only
  unmatched <- exact %>%
    filter(is.na(category) | category == "") %>%
    mutate(.row_id = row_number())
  
  # fuzzy fallback
  fuzzy <- unmatched %>%
    stringdist_left_join(
      dict_sub,
      by = setNames("matched_term", col_name),
      method = "jw",
      max_dist = 0.15,
      distance_col = "distance"
    ) %>%
    group_by(.row_id) %>%
    slice_min(order_by = distance, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # keep exact matches + best fuzzy matches
  result <- exact %>%
    filter(!(is.na(category) | category == "")) %>%
    bind_rows(
      fuzzy %>%
        transmute(
          across(all_of(names(result))),
          category = category.y,
          group = group.y
        )
    ) %>%
    rename(
      !!paste0(d, "_category") := category,
      !!paste0(d, "_group") := group
    )
}

result2 <- result %>% 
  dplyr::select(-c(Software, Country, Analysis, Methods, domain.x, domain.y, domain.x.x, domain.y.y, domain))

write.csv(result2, "Coded_Methods_hierarchy.csv", row.names = FALSE)






# Trait preferences
coded_traits <- fortraits

for (col in names(trait_dict_map)) {
  new_col <- paste0(make.names(col), "_std")
  coded_traits[[new_col]] <- purrr::map_chr(
    coded_traits[[col]],
    ~ apply_dict_one(.x, trait_dict_map[[col]])
  )
}


## Grain color preferences
graincolor_pos <- coded_traits %>%
  filter(Trait_std == "Grain Color") %>%
  mutate(raw_text = Preference) %>%
  rowwise() %>%
  mutate(
    negated_colors = {
      neg <- str_extract_all(
        Preference,
        regex("(?:not|no)\\s+([\\w]+(?:\\s+or\\s+[\\w]+)*)", ignore_case = TRUE)
      )[[1]]
      colors_found <- c()
      for (phrase in neg) {
        if (str_detect(phrase, regex("\\bblack\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "black")
        if (str_detect(phrase, regex("\\bpurple\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "purple")
        if (str_detect(phrase, regex("\\bwhite\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "white")
        if (str_detect(phrase, regex("\\bred\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "red")
        if (str_detect(phrase, regex("\\bbrown\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "brown")
        if (str_detect(phrase, regex("\\byellow\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "yellow")
        if (str_detect(phrase, regex("\\bgr[ae]y\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "grey")
        if (str_detect(phrase, regex("\\bpink\\b", ignore_case = TRUE))) colors_found <- c(colors_found, "pink")
      }
      if (length(colors_found) > 0) paste(colors_found, collapse = ";") else NA_character_
    },
    Preference_pos = str_remove_all(
      Preference,
      regex(",?\\s*(?:not|no)\\s+[\\w]+(?:\\s+or\\s+[\\w]+)*", ignore_case = TRUE)
    ) %>% str_trim()
  ) %>%
  ungroup() %>%
  separate_rows(Preference_pos, sep = "(?i),\\s*(?=white|red|brown|black|yellow|grey|gray|pink|tan|dark|light|cream|pale|ivory)") %>%
  separate_rows(Preference_pos, sep = "(?i)\\s+and\\s+(?=white|red|brown|black|yellow|grey|gray|pink|tan|dark|light|cream|pale|ivory)") %>%
  separate_rows(Preference_pos, sep = ";") %>%
  separate_rows(Preference_pos, sep = "/") %>%
  mutate(Preference_pos = str_trim(Preference_pos)) %>%
  filter(Preference_pos != "" & !is.na(Preference_pos)) %>%
  mutate(
    trait_dimension = "grain color",
    preferred_state = case_when(
      str_detect(Preference_pos, regex("dark\\s+brown", ignore_case = TRUE)) ~ "brown",
      str_detect(Preference_pos, regex("light\\s+gr[ae]y", ignore_case = TRUE)) ~ "gray",
      str_detect(Preference_pos, regex("\\bwhite\\b", ignore_case = TRUE)) ~ "white",
      str_detect(Preference_pos, regex("\\bred\\b|reddish", ignore_case = TRUE)) ~ "red",
      str_detect(Preference_pos, regex("\\bbrown\\b", ignore_case = TRUE)) ~ "brown",
      str_detect(Preference_pos, regex("\\bblack\\b", ignore_case = TRUE)) ~ "black",
      str_detect(Preference_pos, regex("\\byellow\\b", ignore_case = TRUE)) ~ "yellow",
      str_detect(Preference_pos, regex("\\bgr[ae]y\\b", ignore_case = TRUE)) ~ "gray",
      str_detect(Preference_pos, regex("\\bpink\\b", ignore_case = TRUE)) ~ "pink",
      str_detect(Preference_pos, regex("cream|pale|ivory|pearly|tan|light", ignore_case = TRUE)) ~ "tan/cream",
      str_detect(Preference_pos, regex("purple|maroon", ignore_case = TRUE)) ~ "purple/maroon",
      str_detect(Preference_pos, regex("\\bdark\\b", ignore_case = TRUE)) ~ "dark",
      TRUE ~ "other"
    ),
    direction = case_when(
      str_detect(Preference_pos, regex("dislike|unappealing|avoid|unwanted|undesirable", ignore_case = TRUE)) ~ "excluded",
      str_detect(Preference_pos, regex("\\bnon-(red|white|brown|black|yellow|grey|gray|pink|dark)\\b", ignore_case = TRUE)) ~ "excluded",
      str_detect(Preference_pos, regex("\\bfor\\b|\\bwhen\\b|\\bif\\b", ignore_case = TRUE)) ~ "conditional",
      str_detect(Preference_pos, regex("\\bover\\b|rather\\s+than|preferred\\s+over", ignore_case = TRUE)) ~ "preferred",
      TRUE ~ "preferred"
    )
  )

graincolor_neg <- graincolor_pos %>%
  filter(!is.na(negated_colors)) %>%
  distinct(raw_text, negated_colors, VC.Actor_std, Location_std, Crop_std) %>% # ← adjust column names
  separate_rows(negated_colors, sep = ";") %>%
  mutate(
    trait_dimension = "grain color",
    preferred_state = negated_colors,
    direction       = "excluded"
  ) %>%
  select(-negated_colors)

colorprefs <- bind_rows(graincolor_pos, graincolor_neg) %>%
  dplyr::select(
    StudyID, Location_std, Crop_std, VC.Actor_std, Trait_std,
    raw_text, direction,
    preferred_state
  ) %>%
  rename(raw_preference = raw_text, preference_state_std = preferred_state, preference_direction = direction)


non_color_prefs <- coded_traits %>%
  filter(Trait_std != "Grain Color") %>%
  mutate(
    raw_preference = Preference,
    preference_norm = clean_text(Preference),
    preference_direction = purrr::map_chr(
      preference_norm,
      apply_dict,
      dict_tbl = prefdir_dictionary
    ),
    preference_state_std = purrr::map_chr(
      preference_norm,
      apply_dict,
      dict_tbl = prefs,
      col = "canonical"
    ),
    preference_direction = dplyr::coalesce(preference_direction, "preferred")
  ) %>%
  dplyr::select(
    StudyID, Location_std, Crop_std, VC.Actor_std, Trait_std,
    raw_preference, preference_direction,
    preference_state_std
  )

preferences_std <- non_color_prefs %>%
  bind_rows(colorprefs)


## Preference hierarchy
pref_hierarchy <- preferences_std %>%
  left_join(dictionary, by = c("Trait_std" = "category")) %>%
  dplyr::select(-c(raw_term, domain))





# Export cleaned data
write_xlsx(preferences_std, "Cleaned_Preferences.xlsx")
