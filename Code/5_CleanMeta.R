library(tidyverse)
library(stringr)
library(fuzzyjoin)
library(stringdist)

# ============================================================
# 1. Helper functions
# ============================================================

load_dict <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ str_squish(.x)))
}

clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_lower()
}

standardize_with_dictionary <- function(data, col_name, dict, suffix = "_std") {
  
  out_col <- paste0(col_name, suffix)
  
  # Figure out which columns exist in this dictionary
  pattern_col <- dplyr::case_when(
    "pattern" %in% names(dict) ~ "pattern",
    "raw_term" %in% names(dict) ~ "raw_term",
    "matched_term" %in% names(dict) ~ "matched_term",
    TRUE ~ NA_character_
  )
  
  canonical_col <- dplyr::case_when(
    "canonical" %in% names(dict) ~ "canonical",
    "category" %in% names(dict) ~ "category",
    TRUE ~ NA_character_
  )
  
  if (is.na(pattern_col)) {
    stop("Dictionary for ", col_name, " does not have a pattern, raw_term, or matched_term column.")
  }
  
  if (is.na(canonical_col)) {
    stop("Dictionary for ", col_name, " does not have a canonical or category column.")
  }
  
  dict_clean <- dict %>%
    transmute(
      pattern = clean_text(.data[[pattern_col]]),
      canonical = clean_text(.data[[canonical_col]])
    ) %>%
    filter(
      !is.na(pattern),
      pattern != "",
      !is.na(canonical),
      canonical != ""
    ) %>%
    distinct(pattern, canonical)
  
  data %>%
    mutate(
      "{out_col}" := map_chr(
        .data[[col_name]],
        function(x) {
          
          x_clean <- clean_text(x)
          
          if (is.na(x_clean) || x_clean == "") {
            return(NA_character_)
          }
          
          hit <- dict_clean %>%
            filter(str_detect(x_clean, regex(pattern, ignore_case = TRUE))) %>%
            slice(1)
          
          if (nrow(hit) == 0) {
            return(x_clean)
          } else {
            return(hit$canonical[[1]])
          }
        }
      )
    )
}

add_hierarchy <- function(data, domain_name, std_col, max_dist = 0.15) {
  
  category_col <- paste0(domain_name, "_category")
  group_col <- paste0(domain_name, "_group")
  match_col <- paste0(domain_name, "_hierarchy_match")
  distance_col <- paste0(domain_name, "_hierarchy_distance")
  
  dict_sub <- dictionary_methods %>%
    filter(domain == domain_name) %>%
    transmute(
      hierarchy_term = matched_term_clean,
      hierarchy_category = category,
      hierarchy_group = group
    ) %>%
    filter(!is.na(hierarchy_term), hierarchy_term != "") %>%
    distinct(hierarchy_term, .keep_all = TRUE)
  
  data_working <- data %>%
    mutate(.row_id = row_number())
  
  # ------------------------------------------------------------
  # Exact match first
  # ------------------------------------------------------------
  exact <- data_working %>%
    left_join(
      dict_sub,
      by = setNames("hierarchy_term", std_col)
    )
  
  exact_matched <- exact %>%
    filter(!is.na(hierarchy_category), hierarchy_category != "") %>%
    mutate(
      "{category_col}" := hierarchy_category,
      "{group_col}" := hierarchy_group,
      "{match_col}" := .data[[std_col]],
      "{distance_col}" := 0
    ) %>%
    select(
      -any_of(c(
        "hierarchy_term",
        "hierarchy_category",
        "hierarchy_group"
      ))
    )
  
  unmatched <- exact %>%
    filter(is.na(hierarchy_category) | hierarchy_category == "") %>%
    select(
      -any_of(c(
        "hierarchy_term",
        "hierarchy_category",
        "hierarchy_group"
      ))
    )
  
  # ------------------------------------------------------------
  # Fuzzy fallback
  # ------------------------------------------------------------
  fuzzy <- unmatched %>%
    stringdist_left_join(
      dict_sub,
      by = setNames("hierarchy_term", std_col),
      method = "jw",
      max_dist = max_dist,
      distance_col = "distance"
    ) %>%
    group_by(.row_id) %>%
    slice_min(order_by = distance, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      "{category_col}" := hierarchy_category,
      "{group_col}" := hierarchy_group,
      "{match_col}" := hierarchy_term,
      "{distance_col}" := distance
    ) %>%
    select(
      -any_of(c(
        "hierarchy_term",
        "hierarchy_category",
        "hierarchy_group",
        "distance"
      ))
    )
  
  bind_rows(exact_matched, fuzzy) %>%
    arrange(.row_id) %>%
    select(-.row_id)
}
# ============================================================
# 2. Load dictionaries
# ============================================================

trait_dictionary     <- load_dict("Dictionaries/trait_dictionary.csv")
analysis_dictionary  <- load_dict("Dictionaries/analysis_dictionary.csv")
methods_dictionary   <- load_dict("Dictionaries/methods_dictionary.csv")
software_dictionary  <- load_dict("Dictionaries/software_dictionary.csv")
vc_dictionary        <- load_dict("Dictionaries/vc_dictionary.csv")
crop_dictionary      <- load_dict("Dictionaries/crop_dictionary.csv")
country_dictionary   <- load_dict("Dictionaries/country_dictionary.csv")
pubtype_dictionary   <- load_dict("Dictionaries/pubtype_dictionary.csv")
prefstate_dictionary <- load_dict("Dictionaries/pref_dictionary.csv")
prefdir_dictionary   <- load_dict("Dictionaries/prefdirection.csv")

method_dict_map <- list(
  Country_clean  = country_dictionary,
  Analysis_clean = analysis_dictionary,
  Software_clean = software_dictionary,
  Trait_clean    = trait_dictionary
)

trait_dict_map <- list(
  Country_clean = country_dictionary,
  Analysis_clean = analysis_dictionary,
  Trait_clean    = trait_dictionary,
  Software_clean = software_dictionary
)

# ============================================================
# 3. Load and reshape extraction data
# ============================================================

df <- read_csv("Extraction_Export_Raw.csv", show_col_types = FALSE)

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

# ============================================================
# 4. Clean methods-level data
# ============================================================

formethods <- meta_df %>%
  dplyr::select(
    -c(
      DOI,
      `Reviewer Name`,
      Title...6,
      `Journal/Publisher`,
      `Location(s)`,
      `List any specific scales or methods named`
    )
  ) %>%
  rename(
    StudyID  = `Covidence #`,
    Title    = Title...3,
    Methods  = `How were preferences determined?`,
    Analysis = `How was data analyzed?`,
    Software = `Was a statistical software used?`,
    VCs      = `Value chain actors included`,
    Traits   = `Grain quality traits included`,
    Crops    = `Crop(s) studied`
  ) %>%
  dplyr::select(
    StudyID,
    Country,
    Methods,
    Analysis,
    Software,
    VCs,
    Traits,
    Crops
  ) %>%
  mutate(
    Country = str_replace_all(Country, "Ethopia", "Ethiopia")
  ) %>%
  separate_rows(Analysis, sep = ",\\s*") %>%
  separate_rows(Software, sep = ";\\s*") %>%
  separate_rows(Country, sep = ",\\s*") %>%
  mutate(
    Country = str_remove(Country, "^(and|also)\\s+"),
    Country = case_when(
      Country == "Tanzania" ~ "United Republic of Tanzania",
      TRUE ~ Country
    ),
    Country_clean  = clean_text(Country),
    Analysis_clean = clean_text(Analysis),
    Software_clean = clean_text(Software),
    Trait_clean    = clean_text(Traits)
  )

fortraits <- long_df %>%
  rename(
    Methods  = `How were preferences determined?`,
    Analysis = `How was data analyzed?`,
    Software = `Was a statistical software used?`
  ) %>% 
  filter(if_any(c(Trait, Preference), ~ !is.na(.) & . != "")) %>%
  mutate(across(c(Crop, Country, Trait, Analysis), ~ str_trim(as.character(.)))) %>%
  dplyr::select(StudyID, Country, Methods, Crop, Trait, Analysis, Software) %>%
  separate_rows(Crop, sep = ", ") %>%
  separate_rows(Crop, sep = " and ") %>%
  separate_rows(Country, sep = ",\\s*") %>%
  separate_rows(Analysis, sep = ",\\s*") %>%
  mutate(
    Country = str_remove(Country, "^(and|also)\\s+"),
    Country = case_when(
      Country == "Tanzania" ~ "United Republic of Tanzania",
      TRUE ~ Country
    ),
    Country_clean  = clean_text(Country),
    Trait_clean    = clean_text(Trait),
    Analysis_clean = clean_text(Analysis),
    Software_clean = clean_text(Software)
  )

# ============================================================
# 5. Standardize methods-level data using individual dictionaries
# ============================================================

df_std <- formethods

for (nm in names(method_dict_map)) {
  message("Standardizing: ", nm)
  
  df_std <- standardize_with_dictionary(
    data = df_std,
    col_name = nm,
    dict = method_dict_map[[nm]],
    suffix = "_std"
  )
}

trait_std <- fortraits

for (nm in names(trait_dict_map)) {
  message("Standardizing: ", nm)
  
  trait_std <- standardize_with_dictionary(
    data = trait_std,
    col_name = nm,
    dict = trait_dict_map[[nm]],
    suffix = "_std"
  )
}

# ============================================================
# 6. Load hierarchy dictionary
# ============================================================

to_underscore <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:]]+", " ") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\\s+", "_")
}

# dictionary <- read_csv(
#   "hierarchy_df2.csv",
#   show_col_types = FALSE
# )
# 
# dictionary_methods <- dictionary %>%
#   mutate(
#     matched_term = ifelse(
#       is.na(raw_term) | raw_term == "",
#       category,
#       raw_term
#     ),
#     matched_term_clean = clean_text(matched_term),
#     category = str_squish(as.character(category)),
#     group = str_squish(as.character(group)),
#     domain = str_squish(as.character(domain))
#   ) %>%
#   select(domain, matched_term_clean, category, group) %>%
#   filter(!is.na(matched_term_clean), matched_term_clean != "") %>%
#   distinct(domain, matched_term_clean, .keep_all = TRUE)
# 
# dictionary_clean <- dictionary %>%
#   mutate(across(
#     c(sub_term, raw_term, category, group),
#     to_underscore
#   )) %>%
#   mutate(across(c(sub_term, raw_term, category, group), ~ na_if(., ""))) %>%
#   select(domain, sub_term, raw_term, category, group) %>%
#   distinct()


# ============================================================
# 7. Add hierarchy to methods-level data
# ============================================================
# 
dictionary <- read_csv(
  "hierarchy_df2.csv",
  show_col_types = FALSE
)

dictionary_clean <- dictionary %>%
  mutate(
    across(
      c(sub_term, raw_term, category, group),
      to_underscore
    ),
    across(
      c(sub_term, raw_term, category, group),
      ~ na_if(.x, "")
    ),
    
    matched_term = ifelse(
      is.na(raw_term) | raw_term == "",
      category,
      raw_term
    ),
    matched_term_clean = to_underscore(matched_term)
  ) %>%
  select(domain, sub_term, raw_term, matched_term_clean, category, group) %>%
  distinct()

trait_map <- list(
  Country = "Country_clean_std",
  TraitGroup = "Trait_clean_std",
  Analysis = "Analysis_clean_std"
)

test <- trait_std

for (d in names(trait_map)) {
  
  col_name <- trait_map[[d]]
  prefix <- make.names(d)
  
  dict_sub <- dictionary_clean %>%
    filter(domain == d)
  
  test <- test %>%
    
    mutate(
      "{col_name}" := to_underscore(.data[[col_name]])
    ) %>%
    
    # 1. match standardized term to sub_term
    left_join(
      dict_sub %>%
        transmute(
          join_sub = sub_term,
          cat_sub = matched_term_clean,
          grp_sub = group
        ) %>%
        filter(!is.na(join_sub), join_sub != "") %>%
        distinct(),
      by = setNames("join_sub", col_name)
    ) %>%
    
    # 2. if no sub_term match, match to raw_term/category fallback term
    left_join(
      dict_sub %>%
        transmute(
          join_raw = matched_term_clean,
          cat_raw = category,
          grp_raw = group
        ) %>%
        filter(!is.na(join_raw), join_raw != "") %>%
        distinct(),
      by = setNames("join_raw", col_name)
    ) %>%
    
    # 3. if no raw_term match, match to category
    left_join(
      dict_sub %>%
        transmute(
          join_cat = category,
          cat_cat = category,
          grp_cat = group
        ) %>%
        filter(!is.na(join_cat), join_cat != "") %>%
        distinct(),
      by = setNames("join_cat", col_name)
    ) %>%
    
    # 4. if no category match, match to group
    left_join(
      dict_sub %>%
        transmute(
          join_grp = group,
          cat_grp = category,
          grp_grp = group
        ) %>%
        filter(!is.na(join_grp), join_grp != "") %>%
        distinct(),
      by = setNames("join_grp", col_name)
    ) %>%
    
    mutate(
      !!paste0(prefix, "_category") :=
        coalesce(cat_sub, cat_raw, cat_cat, cat_grp),
      
      !!paste0(prefix, "_group") :=
        coalesce(grp_sub, grp_raw, grp_cat, grp_grp),
      
      !!paste0(prefix, "_domain") := d
    ) %>%
    
    dplyr::select(
      -any_of(c(
        "cat_sub", "grp_sub",
        "cat_raw", "grp_raw",
        "cat_cat", "grp_cat",
        "cat_grp", "grp_grp"
      ))
    )
}

# ============================================================
# 8. Final methods-level output
# ============================================================
coded_traits_final <- test %>%
  select(
    StudyID,
    
    Country = Country,
    Country_category,
    Country_group,
    
    Analysis = Analysis_clean_std,
    Analysis_category,
    Analysis_group,
    
    Trait = Trait_clean_std,
    Trait_category = TraitGroup_category,
    Trait_group = TraitGroup_group,
    
    Methods,
    Crop,
    Software = Software_clean_std
  ) %>%
  mutate(
    Analysis_category = case_when(
      is.na(Analysis_category) & Analysis == "other" ~ "other",
      is.na(Analysis_category) & Analysis == "descriptive_statistics" ~ "descriptive_statistics",
      TRUE ~ Analysis_category
    ),
    Analysis_group = case_when(
      is.na(Analysis_group) & Analysis == "other" ~ "other",
      is.na(Analysis_group) & Analysis == "descriptive_statistics" ~ "descriptive_statistics",
      TRUE ~ Analysis_group
    )
  )

coded_methods_final <- coded_methods %>%
  select(
    StudyID,
    
    Country = Country,
    Country_category,
    Country_group,
    
    Analysis = Analysis_clean_std,
    Analysis_category,
    Analysis_group,
    
    Software = Software,
    Software_category,
    Software_group,
    
    Methods,
    VCs,
    Traits,
    Crops
  )


write.csv(coded_traits_final, "Clean_Traits2.csv", row.names = FALSE)
#write.csv(coded_methods_final, "Clean_Methods.csv", row.names = FALSE)
