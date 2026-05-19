library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(rcartocolor)
library(MaizePal)
library(MetBrewer)
library(maps)
library(ggrepel)
library(plotly)
library(treemap)
library(ggalluvial)
library(stringr)
library(purrr)
library(readr)
library(stringdist)
library(tibble)
library(fuzzyjoin)

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

make_pair_summary <- function(
    data,
    variable1,
    variable2,
    id_col = "StudyID",
    keep_cols = NULL
) {
  
  counting_cols <- unique(c(variable1, variable2, keep_cols))
  
  links <- data %>%
    filter(
      !is.na(.data[[id_col]]),
      if_all(all_of(counting_cols), ~ !is.na(.) & . != "")
    ) %>%
    distinct(
      .data[[id_col]],
      across(all_of(counting_cols))
    )
  
  summary <- links %>%
    count(
      across(all_of(counting_cols)),
      name = "n"
    ) %>%
    group_by(.data[[variable2]]) %>%
    mutate(
      variable2_total = sum(n),
      percent_within_variable2 = round(100 * n / variable2_total, 1)
    ) %>%
    ungroup() %>%
    mutate(
      variable1 = variable1,
      variable2 = variable2,
      counting_unit = paste(
        c(id_col, counting_cols),
        collapse = " × "
      )
    ) %>%
    arrange(.data[[variable2]], desc(n))
  
  return(summary)
}

plot_pair_counts <- function(summary_data, variable1, variable2, title = NULL) {
  
  ggplot(summary_data, aes(
    x = .data[[variable2]],
    y = n,
    fill = .data[[variable1]]
  )) +
    geom_col(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = n),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    labs(
      x = variable2,
      y = paste("Count of", variable1),
      fill = variable1,
      title = title
    ) +
    scale_fill_manual(values = longcols) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    )
}

plot_pair_summary <- function(summary_data, variable1, variable2, title = NULL) {
  
  ggplot(summary_data, aes(
    x = .data[[variable2]],
    y = percent_within_variable2 / 100,
    fill = .data[[variable1]]
  )) +
    geom_col(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = ifelse(
        percent_within_variable2 >= 5,
        paste0(round(percent_within_variable2, 1), "%"),
        ""
      )),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = longcols) +
    labs(
      x = variable2,
      y = paste("Percent within", variable2),
      fill = variable1,
      title = title
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    )
}

plot_pair_alluvial <- function(
    data,
    variable1,
    variable2,
    id_col = "StudyID",
    axis1_label = NULL,
    axis2_label = NULL,
    title = NULL
) {
  
  if (is.null(axis1_label)) axis1_label <- variable1
  if (is.null(axis2_label)) axis2_label <- variable2
  
  alluvial_data <- data %>%
    filter(
      !is.na(.data[[id_col]]),
      !is.na(.data[[variable1]]),
      !is.na(.data[[variable2]]),
      .data[[variable1]] != "",
      .data[[variable2]] != ""
    ) %>%
    distinct(
      .data[[id_col]],
      .data[[variable1]],
      .data[[variable2]]
    ) %>%
    count(
      .data[[variable1]],
      .data[[variable2]],
      name = "n"
    )
  
  ggplot(alluvial_data, aes(
    axis1 = .data[[variable1]],
    axis2 = .data[[variable2]],
    y = n
  )) +
    geom_alluvium(aes(fill = .data[[variable1]]), alpha = 0.8) +
    geom_stratum(width = 0.25, color = "grey30") +
    geom_text(
      stat = "stratum",
      aes(label = after_stat(stratum)),
      size = 3
    ) +
    scale_x_discrete(
      limits = c(axis1_label, axis2_label),
      expand = c(0.1, 0.1)
    ) +
    scale_fill_manual(values = longcols) +
    labs(
      title = title,
      y = "Number of study-level relationships",
      fill = variable1
    ) +
    theme_minimal()
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

load_hierarchy <- function(hierarchy_path) {
  
  raw_hierarchy <- readr::read_csv(hierarchy_path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ stringr::str_squish(.x)))
  
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
      category_clean = clean_text(category),
      group_clean = clean_text(group),
      category = stringr::str_squish(as.character(category)),
      group = stringr::str_squish(as.character(group)),
      domain = stringr::str_squish(as.character(domain))
    ) %>%
    select(
      domain,
      sub_term_clean,
      matched_term_clean,
      category_clean,
      group_clean,
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
                         use_fuzzy = TRUE,
                         underscore_input = FALSE,
                         update_std_from_hierarchy = TRUE) {
  
  # ------------------------------------------------------------
  # 0. Set output column names
  # ------------------------------------------------------------
  
  if (is.null(out_col)) {
    out_col <- paste0(col_name, "_std")
  }
  
  if (is.null(prefix)) {
    prefix <- make.names(domain_name)
  }
  
  subterm_col  <- paste0(prefix, "_subterm")
  term_col     <- paste0(prefix, "_term")
  category_col <- paste0(prefix, "_category")
  group_col    <- paste0(prefix, "_group")
  domain_col   <- paste0(prefix, "_domain")
  match_col    <- paste0(prefix, "_match")
  dist_col     <- paste0(prefix, "_distance")
  type_col     <- paste0(prefix, "_match_type")
  
  
  # ------------------------------------------------------------
  # 1. Load hierarchy
  # ------------------------------------------------------------
  
  hierarchy_dict <- load_hierarchy(hierarchy_path)
  
  # ------------------------------------------------------------
  # 2. Standardize input column
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
      mutate(
        "{out_col}" := if (underscore_input) {
          to_underscore(.data[[col_name]])
        } else {
          clean_text(.data[[col_name]])
        }
      )
  }
  
  # ------------------------------------------------------------
  # 3. Build hierarchy lookup for selected domain
  # ------------------------------------------------------------
  
  dict_domain <- hierarchy_dict %>%
    mutate(
      domain_clean = clean_text(domain)
    ) %>%
    filter(domain_clean == clean_text(domain_name)) %>%
    mutate(
      sub_term_clean = clean_text(sub_term_clean),
      matched_term_clean = clean_text(matched_term_clean),
      category_clean = clean_text(category),
      group_clean = clean_text(group)
    )
  
  if (nrow(dict_domain) == 0) {
    warning("No hierarchy entries found for domain: ", domain_name)
  }
  
  # Wide lookup for exact matching.
  # This keeps separate subterm, term, category, and group fields.
  hierarchy_terms_wide <- dict_domain %>%
    transmute(
      sub_term_key      = sub_term_clean,
      matched_term_key  = matched_term_clean,
      category_key      = category_clean,
      group_key         = group_clean,
      
      hierarchy_subterm  = sub_term_clean,
      hierarchy_term     = matched_term_clean,
      hierarchy_category = category,
      hierarchy_group    = group
    ) %>%
    distinct()
  
  # Long lookup for fuzzy matching only.
  # This avoids the hierarchy_term not found error.
  fuzzy_terms <- bind_rows(
    
    dict_domain %>%
      transmute(
        fuzzy_term = sub_term_clean,
        fuzzy_subterm = sub_term_clean,
        fuzzy_term_std = matched_term_clean,
        fuzzy_category = matched_term_clean,
        fuzzy_group = group,
        fuzzy_match_type = "sub_term",
        priority = 1
      ),
    
    dict_domain %>%
      transmute(
        fuzzy_term = matched_term_clean,
        fuzzy_subterm = sub_term_clean,
        fuzzy_term_std = matched_term_clean,
        fuzzy_category = category,
        fuzzy_group = group,
        fuzzy_match_type = "matched_term",
        priority = 2
      ),
    
    dict_domain %>%
      transmute(
        fuzzy_term = category_clean,
        fuzzy_subterm = sub_term_clean,
        fuzzy_term_std = matched_term_clean,
        fuzzy_category = category,
        fuzzy_group = group,
        fuzzy_match_type = "category",
        priority = 3
      ),
    
    dict_domain %>%
      transmute(
        fuzzy_term = group_clean,
        fuzzy_subterm = sub_term_clean,
        fuzzy_term_std = matched_term_clean,
        fuzzy_category = category,
        fuzzy_group = group,
        fuzzy_match_type = "group",
        priority = 4
      )
  ) %>%
    filter(!is.na(fuzzy_term), fuzzy_term != "") %>%
    arrange(priority) %>%
    distinct(fuzzy_term, .keep_all = TRUE)
  
  # ------------------------------------------------------------
  # 4. Exact hierarchy matching
  # Priority:
  # sub_term -> matched_term/raw_term -> category -> group
  # ------------------------------------------------------------
  
  exact <- data_std %>%
    mutate(
      .row_id = row_number(),
      .query = clean_text(.data[[out_col]])
    ) %>%
    
    # 1. match to sub_term
    left_join(
      hierarchy_terms_wide %>%
        filter(!is.na(sub_term_key), sub_term_key != "") %>%
        select(
          sub_term_key,
          sub_hierarchy_subterm = hierarchy_subterm,
          sub_hierarchy_term = hierarchy_term,
          sub_category = hierarchy_category,
          sub_group = hierarchy_group
        ) %>%
        distinct(),
      by = c(".query" = "sub_term_key")
    ) %>%
    
    # 2. match to matched_term/raw_term
    left_join(
      hierarchy_terms_wide %>%
        filter(!is.na(matched_term_key), matched_term_key != "") %>%
        select(
          matched_term_key,
          term_hierarchy_subterm = hierarchy_subterm,
          term_hierarchy_term = hierarchy_term,
          term_category = hierarchy_category,
          term_group = hierarchy_group
        ) %>%
        distinct(),
      by = c(".query" = "matched_term_key")
    ) %>%
    
    # 3. match to category
    left_join(
      hierarchy_terms_wide %>%
        filter(!is.na(category_key), category_key != "") %>%
        select(
          category_key,
          cat_hierarchy_subterm = hierarchy_subterm,
          cat_hierarchy_term = hierarchy_term,
          cat_category = hierarchy_category,
          cat_group = hierarchy_group
        ) %>%
        distinct(),
      by = c(".query" = "category_key")
    ) %>%
    
    # 4. match to group
    left_join(
      hierarchy_terms_wide %>%
        filter(!is.na(group_key), group_key != "") %>%
        select(
          group_key,
          grp_hierarchy_subterm = hierarchy_subterm,
          grp_hierarchy_term = hierarchy_term,
          grp_category = hierarchy_category,
          grp_group = hierarchy_group
        ) %>%
        distinct(),
      by = c(".query" = "group_key")
    ) %>%
    
    mutate(
      hierarchy_subterm = coalesce(
        sub_hierarchy_subterm,
        term_hierarchy_subterm,
        cat_hierarchy_subterm,
        grp_hierarchy_subterm
      ),
      
      hierarchy_term = coalesce(
        sub_hierarchy_term,
        term_hierarchy_term,
        cat_hierarchy_term,
        grp_hierarchy_term
      ),
      
      hierarchy_category = coalesce(
        sub_category,
        term_category,
        cat_category,
        grp_category
      ),
      
      hierarchy_group = coalesce(
        sub_group,
        term_group,
        cat_group,
        grp_group
      ),
      
      hierarchy_match_type = case_when(
        !is.na(sub_category)  ~ "sub_term",
        !is.na(term_category) ~ "matched_term",
        !is.na(cat_category)  ~ "category",
        !is.na(grp_category)  ~ "group",
        TRUE ~ NA_character_
      )
    )
  
  # ------------------------------------------------------------
  # 5. Fuzzy fallback for unmatched rows
  # ------------------------------------------------------------
  
  if (use_fuzzy && nrow(fuzzy_terms) > 0) {
    
    unmatched <- exact %>%
      filter(
        is.na(hierarchy_category),
        !is.na(.query),
        .query != ""
      ) %>%
      select(
        -any_of(c(
          "hierarchy_subterm",
          "hierarchy_term",
          "hierarchy_category",
          "hierarchy_group",
          "hierarchy_match_type"
        ))
      )
    
    message("Rows going to fuzzy matching: ", nrow(unmatched))
    message("Available fuzzy terms: ", nrow(fuzzy_terms))
    
    if (nrow(unmatched) > 0) {
      
      fuzzy <- unmatched %>%
        fuzzyjoin::stringdist_left_join(
          fuzzy_terms,
          by = c(".query" = "fuzzy_term"),
          method = "jw",
          max_dist = max_dist,
          distance_col = "fuzzy_distance"
        ) %>%
        filter(!is.na(fuzzy_term)) %>%
        group_by(.row_id) %>%
        arrange(priority, fuzzy_distance) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(
          .row_id,
          fuzzy_subterm = fuzzy_subterm,
          fuzzy_term_std = fuzzy_term_std,
          fuzzy_category = fuzzy_category,
          fuzzy_group = fuzzy_group,
          fuzzy_match = fuzzy_term,
          fuzzy_match_type = fuzzy_match_type,
          fuzzy_distance = fuzzy_distance
        )
      
      message("Rows matched by fuzzy matching: ", nrow(fuzzy))
      
    } else {
      
      fuzzy <- tibble(
        .row_id = integer(),
        fuzzy_subterm = character(),
        fuzzy_term_std = character(),
        fuzzy_category = character(),
        fuzzy_group = character(),
        fuzzy_match = character(),
        fuzzy_match_type = character(),
        fuzzy_distance = numeric()
      )
    }
    
  } else {
    
    message("Fuzzy matching skipped.")
    
    fuzzy <- tibble(
      .row_id = integer(),
      fuzzy_subterm = character(),
      fuzzy_term_std = character(),
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
      .matched_subterm = coalesce(hierarchy_subterm, fuzzy_subterm),
      .matched_term    = coalesce(hierarchy_term, fuzzy_term_std),
      .matched_category = coalesce(hierarchy_category, fuzzy_category),
      .matched_group    = coalesce(hierarchy_group, fuzzy_group),
      .matched_type     = coalesce(hierarchy_match_type, fuzzy_match_type),
      
      .matched_value = case_when(
        .matched_type == "sub_term"     ~ .matched_subterm,
        .matched_type == "matched_term" ~ .matched_term,
        .matched_type == "category"     ~ .matched_category,
        .matched_type == "group"        ~ .matched_group,
        TRUE ~ NA_character_
      ),
      
      "{subterm_col}" := coalesce(
        .matched_subterm,
        .matched_term,
        .matched_category,
        .matched_group,
        .matched_value
      ),
      
      "{term_col}" := coalesce(
        .matched_term,
        .matched_category,
        .matched_group,
        .matched_value
      ),
      
      "{category_col}" := coalesce(
        .matched_category,
        .matched_group,
        .matched_value
      ),
      
      "{group_col}" := coalesce(
        .matched_group,
        .matched_value
      )
    ) %>%
    select(
      -any_of(c(
        ".row_id",
        ".query",
        
        "sub_hierarchy_subterm",
        "sub_hierarchy_term",
        "sub_category",
        "sub_group",
        
        "term_hierarchy_subterm",
        "term_hierarchy_term",
        "term_category",
        "term_group",
        
        "cat_hierarchy_subterm",
        "cat_hierarchy_term",
        "cat_category",
        "cat_group",
        
        "grp_hierarchy_subterm",
        "grp_hierarchy_term",
        "grp_category",
        "grp_group",
        
        "hierarchy_subterm",
        "hierarchy_term",
        "hierarchy_category",
        "hierarchy_group",
        "hierarchy_match_type",
        
        "fuzzy_subterm",
        "fuzzy_term_std",
        "fuzzy_category",
        "fuzzy_group",
        "fuzzy_match",
        "fuzzy_match_type",
        "fuzzy_distance",
        ".matched_subterm",
        ".matched_term",
        ".matched_category",
        ".matched_group",
        ".matched_type",
        ".matched_value"
      ))
    )
}