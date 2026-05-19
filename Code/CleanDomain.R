library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(stringdist)
library(tibble)
library(fuzzyjoin)


clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_lower()
}

load_dict <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ str_squish(.x)))
}

load_hierarchy <- function(hierarchy_path) {
  
  raw_hierarchy <- read_csv(hierarchy_path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ str_squish(.x)))
  
  # Add missing optional columns so the function does not break
  if (!"sub_term" %in% names(raw_hierarchy)) {
    raw_hierarchy$sub_term <- NA_character_
  }
  
  if (!"raw_term" %in% names(raw_hierarchy)) {
    raw_hierarchy$raw_term <- NA_character_
  }
  
  raw_hierarchy %>%
    mutate(
      matched_term = ifelse(
        is.na(raw_term) | raw_term == "",
        category,
        raw_term
      ),
      sub_term_clean = clean_text(sub_term),
      matched_term_clean = clean_text(matched_term),
      category = str_squish(as.character(category)),
      group = str_squish(as.character(group)),
      domain = str_squish(as.character(domain))
    ) %>%
    select(
      domain,
      sub_term_clean,
      matched_term_clean,
      category,
      group
    ) %>%
    distinct()
}

clean_domain <- function(data,
                         col_name,
                         domain_name,
                         std_dict_path = NULL,
                         hierarchy_path = "hierarchy_df.csv",
                         out_col = NULL,
                         prefix = NULL,
                         max_dist = 0.15,
                         use_fuzzy = TRUE) {
  
  if (is.null(out_col)) {
    out_col <- paste0(col_name, "_std")
  }
  
  if (is.null(prefix)) {
    prefix <- make.names(domain_name)
  }
  
  category_col <- paste0(prefix, "_category")
  group_col    <- paste0(prefix, "_group")
  match_col <- paste0(prefix, "_match")
  dist_col  <- paste0(prefix, "_distance")
  type_col  <- paste0(prefix, "_match_type")
  domain_col <- paste0(prefix, "_domain")
  
  # ------------------------------------------------------------
  # 1. Load hierarchy inside function
  # ------------------------------------------------------------
  
  hierarchy_dict <- load_hierarchy(hierarchy_path)
  
  # ------------------------------------------------------------
  # 2. Load standardization dictionary inside function
  # ------------------------------------------------------------
  
  if (!is.null(std_dict_path)) {
    
    std_dict <- load_dict(std_dict_path)
    
    pattern_col <- dplyr::case_when(
      "pattern" %in% names(std_dict) ~ "pattern",
      "raw_term" %in% names(std_dict) ~ "raw_term",
      "matched_term" %in% names(std_dict) ~ "matched_term",
      TRUE ~ NA_character_
    )
    
    canonical_col <- dplyr::case_when(
      "canonical" %in% names(std_dict) ~ "canonical",
      "category" %in% names(std_dict) ~ "category",
      TRUE ~ NA_character_
    )
    
    if (is.na(pattern_col)) {
      stop("Standardization dictionary does not have pattern, raw_term, or matched_term.")
    }
    
    if (is.na(canonical_col)) {
      stop("Standardization dictionary does not have canonical or category.")
    }
    
    std_dict_clean <- std_dict %>%
      transmute(
        pattern = clean_text(.data[[pattern_col]]),
        canonical = clean_text(.data[[canonical_col]])
      ) %>%
      filter(
        !is.na(pattern), pattern != "",
        !is.na(canonical), canonical != ""
      ) %>%
      distinct(pattern, canonical)
    
    data_std <- data %>%
      mutate(
        "{out_col}" := purrr::map_chr(
          .data[[col_name]],
          function(x) {
            
            x_clean <- clean_text(x)
            
            if (is.na(x_clean) || x_clean == "") {
              return(NA_character_)
            }
            
            hit <- std_dict_clean %>%
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
    
  } else {
    
    data_std <- data %>%
      mutate("{out_col}" := clean_text(.data[[col_name]]))
  }
  
  # ------------------------------------------------------------
  # 3. Build hierarchy lookup for selected domain only
  # ------------------------------------------------------------
  
  dict_domain <- hierarchy_dict %>%
    filter(domain == domain_name) %>%
    mutate(
      matched_term_clean = clean_text(matched_term_clean),
      category_clean = clean_text(category),
      group_clean = clean_text(group)
    )
  
  if (nrow(dict_domain) == 0) {
    warning("No hierarchy entries found for domain: ", domain_name)
  }
  
  hierarchy_terms <- bind_rows(
    
    # 1. match to sub_term first
    dict_domain %>%
      transmute(
        hierarchy_term = sub_term_clean,
        hierarchy_category = matched_term_clean,
        hierarchy_group = group,
        hierarchy_match_type = "sub_term",
        priority = 1
      ),
    
    # 2. match to matched_term/raw term
    dict_domain %>%
      transmute(
        hierarchy_term = matched_term_clean,
        hierarchy_category = category,
        hierarchy_group = group,
        hierarchy_match_type = "matched_term",
        priority = 2
      ),
    
    # 3. match to category
    dict_domain %>%
      transmute(
        hierarchy_term = clean_text(category),
        hierarchy_category = category,
        hierarchy_group = group,
        hierarchy_match_type = "category",
        priority = 3
      ),
    
    # 4. match to group
    dict_domain %>%
      transmute(
        hierarchy_term = clean_text(group),
        hierarchy_category = category,
        hierarchy_group = group,
        hierarchy_match_type = "group",
        priority = 4
      )
  ) %>%
    filter(!is.na(hierarchy_term), hierarchy_term != "") %>%
    arrange(priority) %>%
    group_by(hierarchy_term) %>%
    slice(1) %>%
    ungroup()
  
  # ------------------------------------------------------------
  # 4. Exact hierarchy match
  # ------------------------------------------------------------
  
  exact <- data_std %>%
    mutate(
      .row_id = row_number(),
      .query = clean_text(.data[[out_col]])
    ) %>%
    left_join(
      hierarchy_terms,
      by = c(".query" = "hierarchy_term")
    )
  
  # ------------------------------------------------------------
  # 5. Fuzzy fallback
  # ------------------------------------------------------------
  
  if (use_fuzzy && nrow(hierarchy_terms) > 0) {
    
    unmatched <- exact %>%
      filter(
        is.na(hierarchy_category),
        !is.na(.query),
        .query != ""
      ) %>%
      select(
        -any_of(c(
          "hierarchy_category",
          "hierarchy_group",
          "hierarchy_match_type",
          "priority"
        ))
      )
    
    if (nrow(unmatched) > 0) {
      
      fuzzy <- unmatched %>%
        fuzzyjoin::stringdist_left_join(
          hierarchy_terms,
          by = c(".query" = "hierarchy_term"),
          method = "jw",
          max_dist = max_dist,
          distance_col = "fuzzy_distance"
        ) %>%
        group_by(.row_id) %>%
        slice_min(order_by = fuzzy_distance, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        transmute(
          .row_id,
          fuzzy_category = hierarchy_category,
          fuzzy_group = hierarchy_group,
          fuzzy_match = hierarchy_term,
          fuzzy_match_type = hierarchy_match_type,
          fuzzy_distance = fuzzy_distance
        )
      
    } else {
      
      fuzzy <- tibble(
        .row_id = integer(),
        fuzzy_category = character(),
        fuzzy_group = character(),
        fuzzy_match = character(),
        fuzzy_match_type = character(),
        fuzzy_distance = numeric()
      )
    }
    
  } else {
    
    fuzzy <- tibble(
      .row_id = integer(),
      fuzzy_category = character(),
      fuzzy_group = character(),
      fuzzy_match = character(),
      fuzzy_match_type = character(),
      fuzzy_distance = numeric()
    )
  }
  
  # ------------------------------------------------------------
  # 6. Combine exact + fuzzy
  # ------------------------------------------------------------
  
  exact %>%
    left_join(fuzzy, by = ".row_id") %>%
    mutate(
      "{category_col}" := coalesce(hierarchy_category, fuzzy_category),
      "{group_col}" := coalesce(hierarchy_group, fuzzy_group),
      "{domain_col}" := domain_name,
      
      "{match_col}" := case_when(
        !is.na(hierarchy_category) ~ .query,
        !is.na(fuzzy_match) ~ fuzzy_match,
        TRUE ~ NA_character_
      ),
      
      "{dist_col}" := case_when(
        !is.na(hierarchy_category) ~ 0,
        !is.na(fuzzy_distance) ~ fuzzy_distance,
        TRUE ~ NA_real_
      ),
      
      "{type_col}" := coalesce(hierarchy_match_type, fuzzy_match_type)
    ) %>%
    select(
      -any_of(c(
        ".row_id",
        ".query",
        "hierarchy_category",
        "hierarchy_group",
        "hierarchy_match_type",
        "priority",
        "fuzzy_category",
        "fuzzy_group",
        "fuzzy_match",
        "fuzzy_match_type",
        "fuzzy_distance"
      ))
    )
}