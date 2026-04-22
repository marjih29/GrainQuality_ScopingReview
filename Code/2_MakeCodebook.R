# Make a codebook by fuzzy-matching input terms to ontology candidates from multiple sources

# Packages

pkgs <- c(
  "httr", "jsonlite", "dplyr", "purrr", "readr", "writexl",
  "stringr", "tibble", "tidyr", "ontologyIndex",
  "fuzzyjoin", "stringdist", "hunspell", "rlang", "rdflib", "furrr", "future"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Configs

cfg <- list(
  dict_file    = "rawdictionary.csv",
  output_file  = "codebook.xlsx",
  checkpoint   = "makecodebook.RData",
  
  # API endpoints
  agrovoc_rest    = "https://agrovoc.fao.org/browse/rest/v1/search/",
  agrovoc_sparql  = "https://agrovoc.fao.org/sparql",
  mesh_rest       = "https://id.nlm.nih.gov/mesh/lookup/term",      # NLM linked-data lookup
  mesh_sparql     = "https://id.nlm.nih.gov/mesh/sparql",
  envthes_sparql  = "https://vocabs.lter-europe.net/envthes/en/",    # SPARQL not public; we use REST
  envthes_rest    = "https://vocabs.lter-europe.net/rest/v1/EnvThes/search",
  
  sleep_sec       = 0.35,   # polite delay between API calls
  
  # Fuzzy matching
  fuzzy_max_dist  = 0.15,   # Jaro-Winkler threshold
  cluster_h       = 0.12,   # hierarchical cluster cut height
  n_best          = 5,      # candidates kept per (term × source)
  
  # Scoring weights  –  edit here to retune, no need to touch scoring code
  w = list(
    lexical   = list(d01 = 5, d03 = 4.5, d05 = 4, d08 = 3, d12 = 2, d15 = 1),
    context   = 2,
    exact     = 2,
    consensus = list(ge3 = 3, eq2 = 2),
    generic_penalty = -2
  ),
  
  # Source fit scores: named list[variable][source] = score
  source_fit = list(
    Trait       = c(TO=5, PATO=5, SOR=5, WHEAT=4, FOODON=3, PO=2,
                    AGROVOC=2, ONS=2, MESH=2, ENVTHES=1),
    Analysis    = c(STATO=5, SWO=4, ONS=3, AGROVOC=2, MESH=2, ENVTHES=1),
    Methods     = c(SWO=5, STATO=4, FOODON=3, AGROVOC=2, MESH=2, ONS=1, ENVTHES=1),
    Software    = c(SWO=6, AGROVOC=1, MESH=1),
    Crop        = c(PO=5, AGROVOC=5, SOR=5, TO=5, FOODON=2, ENVTHES=2,
                    PATO=1, ONS=1, MESH=1),
    Country     = c(AGROVOC=6, ENVTHES=3, MESH=1),
    `VC Actor`  = c(AGROVOC=5, MESH=1),
    VCs         = c(AGROVOC=5, MESH=1)
  ),
  
  # Context keyword patterns per variable (for context_score)
  context_kw = list(
    Trait      = "trait|quality|grain|seed|kernel|morphology|composition|color|texture|starch|protein|iron|zinc|nutrition|diet",
    Analysis   = "statistic|analysis|test|model|regression|anova|correlation|classification|multivariate|study design",
    Methods    = "method|protocol|assay|technique|procedure|experiment|study|sampling|measurement",
    Software   = "software|tool|application|package|program",
    Crop       = "plant|crop|species|cultivar|variety",
    Country    = "country|countries|region|geographic",
    `VC Actor` = "organization|institution|actor|stakeholder|group",
    VCs        = "organization|institution|actor|stakeholder|group"
  ),
  
  generic_terms = c(
    "trait", "quality", "method", "methods", "model", "models",
    "software", "country", "crop", "plant", "analysis", "statistical analysis"
  ),
  
  use_agrovoc = TRUE,
  use_mesh    = TRUE,
  use_envthes = TRUE,
  use_obo     = TRUE
)

checkpoint <- function() saveRDS(environment(), cfg$checkpoint)

# Functions

safe_chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

normalize_term <- function(x) {
  x %>%
    safe_chr() %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("@", " at ")  %>%
    str_replace_all("\\+", " plus ") %>%
    str_replace_all("[-_]", " ")  %>%
    str_remove_all("\\([^)]*\\)") %>%
    str_remove(regex("^other\\s*:\\s*", ignore_case = TRUE)) %>%
    str_replace_all("[^a-z0-9/ ]+", " ") %>%
    str_squish()
}

safe_singularize_word <- function(w) {
  stems <- tryCatch(hunspell::hunspell_stem(w)[[1]], error = function(e) character(0))
  if (length(stems) > 0) stems[1] else w
}

safe_singularize_phrase <- function(x) {
  words <- str_split(normalize_term(x), "\\s+")[[1]]
  if (length(words) == 0 || all(words == "")) return("")
  paste(map_chr(words, safe_singularize_word), collapse = " ")
}

coalesce_str <- function(...) {
  vals <- list(...); out <- vals[[1]]
  for (v in vals[-1]) out <- ifelse(is.na(out) | out == "", v, out)
  out
}

separate_terms <- function(x) {
  x %>%
    safe_chr() %>%
    str_replace_all("；", ";") %>%
    str_replace_all("\\s*[;,/]\\s*", ";") %>%
    str_split(pattern = ";") %>%
    unlist() %>%
    str_squish() %>%
    discard(~ .x == "" | is.na(.x)) %>%
    unique()
}

separate_country_terms <- function(x) {
  x %>%
    safe_chr() %>%
    str_squish() %>%
    str_replace_all("；", ";") %>%
    str_replace_all("\\s*[;,/]\\s*", ";") %>%
    str_replace_all("\\s+(and|also)\\s+", ";") %>%
    str_split(";") %>%
    unlist() %>%
    str_squish() %>%
    discard(~ .x == "" | is.na(.x)) %>%
    unique()
}

separate_analysis_terms <- function(x) {
  x %>% safe_chr() %>%
    str_squish() %>%
    str_replace_all("\\s*[;,/]\\s*", ";") %>%
    str_replace_all("\\band\\b", ";") %>%
    str_split(";") %>%
    unlist() %>% str_squish() %>%
    discard(~ .x == "" | is.na(.x))
}

make_search_variants <- function(term) {
  raw  <- safe_chr(term)
  norm <- normalize_term(raw)
  sing <- safe_singularize_phrase(norm)
  slash_parts <- str_split(norm, "\\s*/\\s*")[[1]] %>% discard(~ .x == "")
  unique(c(raw, norm, sing, slash_parts)) %>% discard(~ .x == "" | is.na(.x))
}

apply_dict <- function(term, dict_tbl, col = "canonical") {
  x <- normalize_term(term)
  hit <- which(str_detect(x, regex(dict_tbl$pattern, ignore_case = TRUE)))
  if (length(hit) == 0) return(NA_character_)
  dict_tbl[[col]][hit[1]]
}

# Dictionaries

load_dict <- function(path) read.csv(path, stringsAsFactors = FALSE)

trait_dictionary       <- load_dict("Dictionaries/trait_dictionary.csv")
analysis_dictionary    <- load_dict("Dictionaries/analysis_dictionary.csv")
methods_dictionary     <- load_dict("Dictionaries/methods_dictionary.csv")
software_dictionary    <- load_dict("Dictionaries/software_dictionary.csv")
vc_dictionary          <- load_dict("Dictionaries/vc_dictionary.csv")
crop_dictionary        <- load_dict("Dictionaries/crop_dictionary.csv")
country_dictionary     <- load_dict("Dictionaries/country_dictionary.csv")
pubtype_dictionary     <- load_dict("Dictionaries/pubtype_dictionary.csv")


# Read data and clean

dict_raw <- read_csv(cfg$dict_file, show_col_types = FALSE) %>%
  select(any_of(c("standard", "variable"))) %>%
  mutate(across(everything(), ~ str_trim(as.character(.)))) %>%
  filter(!is.na(standard), standard != "", variable != "Preference")

dict_expanded <- dict_raw %>%
  mutate(original_phrase = standard) %>%
  rowwise() %>%
  mutate(
    standard = list(
      if (variable == "Country") {
        separate_country_terms(standard)
      } else if (variable == "Analysis") {
        separate_analysis_terms(standard)  
      } else if (variable %in% c("Trait","Methods","Software","VC Actor","Crop","Crops","VCs")) {
        separate_terms(standard)
      } else {
        standard
      }
    )
  ) %>%
  unnest(standard) %>%
  ungroup() %>%
  mutate(standard = str_squish(standard)) %>%
  filter(!is.na(standard), standard != "") %>%
  distinct(original_phrase, standard, variable, .keep_all = TRUE)


# Apply manual dictionaries

apply_manual <- function(df, var_filter, dict, col_name) {
  df %>% mutate(!!col_name := ifelse(
    variable %in% var_filter,
    map_chr(standard_raw, apply_dict, dict_tbl = dict),
    NA_character_
  ))
}

dict_clean <- dict_expanded %>%
  rename(standard_raw = standard) %>%
  mutate(
    standard_norm = normalize_term(standard_raw),
    standard_sing = safe_singularize_phrase(standard_raw)
  ) %>%
  apply_manual("Trait",                       trait_dictionary,    "manual_trait_std")    %>%
  apply_manual("Analysis",                    analysis_dictionary, "manual_analysis_std") %>%
  apply_manual("Methods",                     methods_dictionary,  "manual_methods_std")  %>%
  apply_manual("Software",                    software_dictionary, "manual_software_std") %>%
  apply_manual(c("VC Actor","VCs"),           vc_dictionary,       "manual_vc_std")       %>%
  apply_manual(c("Crop","Crops"),             crop_dictionary,     "manual_crop_std")     %>%
  apply_manual("Country",                     country_dictionary,  "manual_country_std")  %>%
  apply_manual("Publication Type",            pubtype_dictionary,  "manual_pubtype_std")  %>%
  mutate(
    standard_for_match = case_when(
      variable == "Trait"                        & !is.na(manual_trait_std)    ~ manual_trait_std,
      variable == "Analysis"                     & !is.na(manual_analysis_std) ~ manual_analysis_std,
      variable == "Methods"                      & !is.na(manual_methods_std)  ~ manual_methods_std,
      variable == "Software"                     & !is.na(manual_software_std) ~ manual_software_std,
      variable %in% c("VC Actor","VCs")          & !is.na(manual_vc_std)       ~ manual_vc_std,
      variable %in% c("Crop","Crops")            & !is.na(manual_crop_std)     ~ manual_crop_std,
      variable == "Country"                      & !is.na(manual_country_std)  ~ manual_country_std,
      variable == "Publication Type"             & !is.na(manual_pubtype_std)  ~ manual_pubtype_std,
      TRUE ~ standard_raw
    ),
    match_norm = normalize_term(standard_for_match),
    match_sing = safe_singularize_phrase(standard_for_match)
  ) %>%
  distinct(original_phrase, standard_raw, variable, standard_for_match, .keep_all = TRUE)

saveRDS(dict_clean, "checkpoint_dictclean.rds")

# Ontology matching

terms_all        <- dict_clean %>% transmute(preferred_input = standard_for_match, variable,
                                             term_norm = match_norm, term_sing = match_sing) %>% distinct()
terms_general    <- terms_all %>% filter(variable %in% c("Trait","Crop","Crops","Country","VC Actor","VCs","Analysis"))
terms_swo        <- terms_all %>% filter(variable %in% c("Software","Methods","Analysis"))
terms_foodon     <- terms_all %>% filter(variable %in% c("Trait","Methods"),
                                         str_detect(term_norm, "food|flour|meal|porridge|dough|bread|baking|cook|edible|consum|ingredient|process|ferment|mill|grind"))
terms_ons        <- terms_all %>% filter(variable %in% c("Trait","Methods","Analysis"),
                                         str_detect(term_norm, "nutrition|nutrient|diet|protein|starch|amylose|amylopectin|iron|zinc|digest|bioavail|composition"))


# Query AGROVOC

agrovoc_search_one <- function(term) {
  Sys.sleep(cfg$sleep_sec)
  res <- tryCatch(
    GET(cfg$agrovoc_rest, query = list(query = term, lang = "en", vocab = "agrovoc"), timeout(20)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NULL)
  parsed <- tryCatch(fromJSON(rawToChar(res$content), simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed$results) || length(parsed$results) == 0) return(NULL)
  r <- parsed$results[[1]]
  list(uri = as.character(r$uri), rest_label = as.character(r$prefLabel), query_used = term)
}

agrovoc_hierarchy <- function(uri) {
  empty <- list(agrovoc_match=NA, broader1=NA, broader2=NA, broader3=NA)
  if (is.na(uri) || uri == "") return(empty)
  q <- sprintf('
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?prefLabel ?b1label ?b2label ?b3label WHERE {
  GRAPH ?g {
    <%s> skos:prefLabel ?prefLabel . FILTER(lang(?prefLabel)="en")
    OPTIONAL { <%s> skos:broader ?b1 . ?b1 skos:prefLabel ?b1label . FILTER(lang(?b1label)="en")
      OPTIONAL { ?b1 skos:broader ?b2 . ?b2 skos:prefLabel ?b2label . FILTER(lang(?b2label)="en")
        OPTIONAL { ?b2 skos:broader ?b3 . ?b3 skos:prefLabel ?b3label . FILTER(lang(?b3label)="en") }
      }
    }
  }
} LIMIT 10', uri, uri)
  res <- tryCatch(
    POST(cfg$agrovoc_sparql, body=list(query=q), encode="form",
         add_headers(Accept="application/sparql-results+json"), timeout(20)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(empty)
  txt  <- content(res, "text", encoding = "UTF-8")
  if (grepl("^\\s*<", txt)) return(empty)
  json <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  b    <- json$results$bindings
  if (is.null(b) || length(b) == 0) return(empty)
  sv <- function(x) if (!is.null(x$value)) as.character(x$value) else NA_character_
  list(
    agrovoc_match = na.omit(sapply(b, function(r) sv(r$prefLabel)))[1],
    broader1 = paste(unique(na.omit(sapply(b, function(r) sv(r$b1label)))), collapse="; "),
    broader2 = paste(unique(na.omit(sapply(b, function(r) sv(r$b2label)))), collapse="; "),
    broader3 = paste(unique(na.omit(sapply(b, function(r) sv(r$b3label)))), collapse="; ")
  )
}

search_agrovoc <- function(term) {
  for (v in make_search_variants(term)) {
    hit <- agrovoc_search_one(v)
    if (!is.null(hit)) {
      hier <- agrovoc_hierarchy(hit$uri)
      return(c(hit, hier))
    }
  }
  list(uri=NA, rest_label=NA, query_used=NA, agrovoc_match=NA, broader1=NA, broader2=NA, broader3=NA)
}

if (cfg$use_agrovoc) {
  message("Querying AGROVOC ...")
  agrovoc_enriched <- map_dfr(
    distinct(terms_general, preferred_input)$preferred_input,
    function(term) {
      h <- search_agrovoc(term)
      tibble(preferred_input   = term,
             agrovoc_query_used = h$query_used,
             agrovoc_match      = h$agrovoc_match,
             broader1           = h$broader1,
             broader2           = h$broader2,
             broader3           = h$broader3,
             agrovoc_uri        = h$uri)
    }
  )
  saveRDS(agrovoc_enriched, "checkpoint_agrovoc.rds")
} else {
  agrovoc_enriched <- tibble(preferred_input=character(), agrovoc_query_used=character(),
                             agrovoc_match=character(), broader1=character(),
                             broader2=character(), broader3=character(), agrovoc_uri=character())
}


# Use the NLM MeSH RDF SPARQL endpoint 

mesh_lookup_sparql <- function(term) {
  Sys.sleep(cfg$sleep_sec)
  # Escape for SPARQL string literal
  term_esc <- str_replace_all(term, '"', '\\\\"')
  q <- sprintf('
PREFIX meshv: <http://id.nlm.nih.gov/mesh/vocab#>
PREFIX rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?descriptor ?label ?treeNum WHERE {
  ?descriptor a meshv:TopicalDescriptor ;
              rdfs:label ?label ;
              meshv:treeNumber ?treeNum .
  FILTER(LCASE(STR(?label)) = "%s")
} LIMIT 5', str_to_lower(term_esc))
  
  res <- tryCatch(
    GET(cfg$mesh_sparql,
        query = list(query = q, format = "JSON", inference = "true"),
        timeout(25)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NULL)
  txt  <- tryCatch(content(res, "text", encoding="UTF-8"), error=function(e) NULL)
  json <- tryCatch(fromJSON(txt, simplifyVector=FALSE), error=function(e) NULL)
  b    <- json$results$bindings
  if (is.null(b) || length(b) == 0) return(NULL)
  
  sv <- function(x) if (!is.null(x$value)) as.character(x$value) else NA_character_
  list(
    uri        = sv(b[[1]]$descriptor),
    onto_term  = sv(b[[1]]$label),
    tree_nums  = paste(unique(sapply(b, function(r) sv(r$treeNum))), collapse="; "),
    query_used = term
  )
}

search_mesh <- function(term) {
  for (v in make_search_variants(term)) {
    hit <- mesh_lookup_sparql(v)
    if (!is.null(hit)) return(hit)
  }
  list(uri=NA, onto_term=NA, tree_nums=NA, query_used=NA)
}

build_mesh_candidates <- function(terms_df) {
  message("Querying MeSH ...")
  map_dfr(seq_len(nrow(terms_df)), function(i) {
    term <- terms_df$preferred_input[i]
    h    <- search_mesh(term)
    if (is.na(h$uri)) return(NULL)
    tibble(
      preferred_input = term,
      variable        = terms_df$variable[i],
      onto_id         = h$uri,
      onto_term       = h$onto_term,
      onto_def        = NA_character_,
      parents         = h$tree_nums,   # tree numbers serve as hierarchy
      ancestors       = NA_character_,
      source          = "MESH",
      distance        = 0,             # exact label match from SPARQL filter
      query_used      = h$query_used
    )
  })
}

# ENVTHES  (SKOSMOS REST API, LTER-Europe)

envthes_search_one <- function(term) {
  Sys.sleep(cfg$sleep_sec)
  res <- tryCatch(
    GET(cfg$envthes_rest,
        query = list(query = term, lang = "en", maxhits = 5),
        timeout(20)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NULL)
  parsed <- tryCatch(fromJSON(rawToChar(res$content), simplifyVector=FALSE), error=function(e) NULL)
  results <- parsed$results
  if (is.null(results) || length(results) == 0) return(NULL)
  r <- results[[1]]
  list(
    uri        = as.character(r$uri),
    onto_term  = as.character(r$prefLabel),
    broader    = if (!is.null(r$broader)) as.character(r$broader[[1]]$prefLabel) else NA_character_,
    query_used = term
  )
}

search_envthes <- function(term) {
  for (v in make_search_variants(term)) {
    hit <- envthes_search_one(v)
    if (!is.null(hit)) return(hit)
  }
  list(uri=NA, onto_term=NA, broader=NA, query_used=NA)
}

build_envthes_candidates <- function(terms_df) {
  message("Querying ENVTHES ...")
  map_dfr(seq_len(nrow(terms_df)), function(i) {
    term <- terms_df$preferred_input[i]
    h    <- search_envthes(term)
    if (is.na(h$uri)) return(NULL)
    tibble(
      preferred_input = term,
      variable        = terms_df$variable[i],
      onto_id         = h$uri,
      onto_term       = h$onto_term,
      onto_def        = NA_character_,
      parents         = h$broader,
      ancestors       = NA_character_,
      source          = "ENVTHES",
      distance        = 0,
      query_used      = h$query_used
    )
  })
}


# OBO / OWL ONTOLOGIES
read_owl_to_df <- function(owl_file) {
  g <- rdf_parse(owl_file)
  labels  <- rdf_query(g, 'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?id ?label WHERE { ?id rdfs:label ?label . }')
  defs    <- rdf_query(g, 'PREFIX obo: <http://purl.obolibrary.org/obo/>
    SELECT ?id ?def WHERE { ?id obo:IAO_0000115 ?def . }') %>%
    mutate(across(everything(), as.character)) %>%
    group_by(id) %>% summarise(def=paste(unique(def),collapse="; "), .groups="drop")
  parents <- rdf_query(g, 'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?id ?parent WHERE { ?id rdfs:subClassOf ?parent . }') %>%
    mutate(across(everything(), as.character)) %>%
    group_by(id) %>% summarise(parents=paste(unique(parent),collapse="; "), .groups="drop")
  
  labels %>%
    mutate(across(everything(), as.character)) %>%
    left_join(defs,    by="id") %>%
    left_join(parents, by="id") %>%
    transmute(id, name=label, def, parents=coalesce(parents, NA_character_), ancestors=NA_character_,
              name_norm=normalize_term(name), name_sing=safe_singularize_phrase(name)) %>%
    distinct(id, name, .keep_all=TRUE)
}

make_onto_df <- function(onto) {
  id2idx <- setNames(seq_along(onto$id), onto$id)
  tibble(id=onto$id, name=onto$name,
         def=map_chr(onto$def, ~ if(length(.x)) .x[[1]] else NA_character_)) %>%
    mutate(
      name_norm = normalize_term(name),
      name_sing = safe_singularize_phrase(name),
      parents   = map_chr(id, ~ paste(onto$name[onto$parents[[id2idx[.x]]]],   collapse="; ")),
      ancestors = map_chr(id, ~ paste(onto$name[onto$ancestors[[id2idx[.x]]]], collapse=" > "))
    )
}

if (cfg$use_obo) {
  message("Loading OBO/OWL ontologies ...")
  to_df    <- make_onto_df(get_ontology("Ontologies/to.obo",    extract_tags="everything"))
  po_df    <- make_onto_df(get_ontology("Ontologies/po.obo",    extract_tags="everything"))
  pato_df  <- make_onto_df(get_ontology("Ontologies/pato.obo",  extract_tags="everything"))
  stato_df <- make_onto_df(get_ontology("Ontologies/stato.obo", extract_tags="everything"))
  sor_df   <- make_onto_df(get_ontology("Ontologies/sorghum-with-crop-name.obo", extract_tags="everything"))
  wheat_df <- make_onto_df(get_ontology("Ontologies/wheat_trait.obo", extract_tags="everything"))
  swo_df      <- read_owl_to_df("Ontologies/swo.owl")
  foodon_df   <- read_owl_to_df("Ontologies/foodon.owl")
  ons_df      <- read_owl_to_df("Ontologies/ons.owl")
  saveRDS(list(to=to_df,po=po_df,pato=pato_df,stato=stato_df,
               sor=sor_df,wheat=wheat_df,swo=swo_df,foodon=foodon_df,ons=ons_df),
          "checkpoint_obo.rds")
}

fuzzy_match_ontology <- function(terms_df, onto_df, source_name, n_best = cfg$n_best) {
  join_and_tidy <- function(by_col, onto_col) {
    stringdist_left_join(terms_df, onto_df,
                         by       = setNames(onto_col, by_col),
                         method   = "jw",
                         max_dist = cfg$fuzzy_max_dist,
                         distance_col = "dist"
    ) %>%
      transmute(preferred_input, variable, onto_id=id, onto_term=name,
                onto_def=def, parents, ancestors, source=source_name, distance=dist)
  }
  bind_rows(
    join_and_tidy("term_norm", "name_norm"),
    join_and_tidy("term_sing", "name_sing")
  ) %>%
    filter(!is.na(onto_term), onto_term != "") %>%
    distinct(preferred_input, variable, source, onto_id, onto_term, .keep_all=TRUE) %>%
    group_by(preferred_input, variable, source) %>%
    slice_min(distance, n=n_best, with_ties=FALSE) %>%
    ungroup()
}

# Fuzzy-match input terms

message("Fuzzy-matching OBO ontologies ...")
obo_candidates <- bind_rows(
  fuzzy_match_ontology(terms_general, to_df,    "TO"),
  fuzzy_match_ontology(terms_general, po_df,    "PO"),
  fuzzy_match_ontology(terms_general, pato_df,  "PATO"),
  fuzzy_match_ontology(terms_general, stato_df, "STATO"),
  fuzzy_match_ontology(terms_general, sor_df,   "SOR"),
  fuzzy_match_ontology(terms_general, wheat_df, "WHEAT"),
  fuzzy_match_ontology(terms_swo,     swo_df,   "SWO"),
  fuzzy_match_ontology(terms_foodon,  foodon_df,"FOODON"),
  fuzzy_match_ontology(terms_ons,     ons_df,   "ONS")
)

mesh_candidates    <- if (cfg$use_mesh)    build_mesh_candidates(terms_all)    else tibble()
envthes_candidates <- if (cfg$use_envthes) build_envthes_candidates(terms_all) else tibble()

# AGROVOC as candidates frame
agrovoc_candidates <- agrovoc_enriched %>%
  left_join(distinct(terms_general, preferred_input, variable), by="preferred_input") %>%
  filter(!is.na(agrovoc_match), agrovoc_match != "") %>%
  transmute(
    preferred_input, variable,
    onto_id    = agrovoc_uri,
    onto_term  = agrovoc_match,
    onto_def   = NA_character_,
    parents    = broader1,
    ancestors  = NA_character_,
    source     = "AGROVOC",
    distance   = 0
  )

saveRDS(list(obo=obo_candidates, mesh=mesh_candidates,
             envthes=envthes_candidates, agrovoc=agrovoc_candidates),
        "checkpoint_candidates.rds")


# Combine all candidates, attach AGROVOC hierarchy info to non-AGROVOC rows, and prepare for scoring

all_candidates_raw <- bind_rows(obo_candidates, agrovoc_candidates,
                                mesh_candidates, envthes_candidates) %>%
  filter(!is.na(preferred_input), !is.na(onto_term), onto_term != "") %>%
  # attach AGROVOC hierarchy columns to non-AGROVOC rows
  left_join(agrovoc_enriched %>% select(preferred_input, agrovoc_match,
                                        broader1, broader2, broader3,
                                        agrovoc_uri, agrovoc_query_used),
            by="preferred_input") %>%
  mutate(
    onto_term_norm = normalize_term(onto_term),
    preferred_norm = normalize_term(preferred_input),
    distance       = replace_na(distance, 0.25),
    context_text   = str_to_lower(paste(
      coalesce(parents,""), coalesce(ancestors,""),
      coalesce(broader1,""), coalesce(broader2,""), coalesce(broader3,"")))
  ) %>%
  distinct(preferred_input, variable, source, onto_id, onto_term, .keep_all=TRUE)

# --- Scoring
score_candidates <- function(df) {
  w <- cfg$w
  
  df %>%
    mutate(
      # 1. Lexical (distance-based)
      lexical_score = case_when(
        distance <= 0.01 ~ w$lexical$d01,
        distance <= 0.03 ~ w$lexical$d03,
        distance <= 0.05 ~ w$lexical$d05,
        distance <= 0.08 ~ w$lexical$d08,
        distance <= 0.12 ~ w$lexical$d12,
        distance <= 0.15 ~ w$lexical$d15,
        TRUE ~ 0
      ),
      
      # 2. Source fit (lookup from config list)
      source_fit_score = map2_dbl(variable, source, function(var, src) {
        fit <- cfg$source_fit[[var]]
        if (is.null(fit)) return(0)
        unname(fit[src] %||% 0)
      }),
      
      # 3. Context keyword match
      context_score = map2_dbl(variable, context_text, function(var, ctx) {
        kw <- cfg$context_kw[[var]]
        if (is.null(kw)) return(0)
        if (str_detect(ctx, kw)) w$context else 0
      }),
      
      # 4. Exact normalised label match
      exact_norm_score = ifelse(preferred_norm == onto_term_norm, w$exact, 0),
      
      # 5. Generic term penalty
      generic_penalty = case_when(
        onto_term_norm %in% cfg$generic_terms          ~ w$generic_penalty,
        str_detect(onto_term_norm, "^other\\b|^misc")  ~ w$generic_penalty,
        TRUE ~ 0
      )
    ) %>%
    # 6. Consensus score: same normalised term matched by multiple sources
    group_by(preferred_input, variable, onto_term_norm) %>%
    mutate(consensus_n = n_distinct(source)) %>%
    ungroup() %>%
    mutate(
      consensus_score = case_when(
        consensus_n >= 3 ~ w$consensus$ge3,
        consensus_n == 2 ~ w$consensus$eq2,
        TRUE ~ 0
      ),
      total_score = lexical_score + source_fit_score + context_score +
        exact_norm_score + consensus_score + generic_penalty
    )
}

all_candidates_scored <- score_candidates(all_candidates_raw)

# --- Rank within (preferred_input × variable) 
ranked_candidates <- all_candidates_scored %>%
  group_by(preferred_input, variable) %>%
  arrange(desc(total_score), distance, onto_term, .by_group=TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

best_matches <- ranked_candidates %>%
  filter(rank == 1) %>%
  distinct(preferred_input, variable, .keep_all=TRUE)

score_summary <- ranked_candidates %>%
  group_by(preferred_input, variable) %>%
  summarise(
    n_candidates  = n(),
    best_score    = max(total_score, na.rm=TRUE),
    second_score  = if(n()>=2) sort(total_score, decreasing=TRUE)[2] else NA_real_,
    score_gap     = best_score - second_score,
    sources_found = paste(sort(unique(source)), collapse="; "),
    .groups = "drop"
  )

best_matches <- best_matches %>%
  left_join(score_summary, by=c("preferred_input","variable"))

saveRDS(list(ranked=ranked_candidates, best=best_matches, summary=score_summary),
        "checkpoint_scored.rds")


# Write final codebook

final_codebook <- dict_clean %>%
  left_join(best_matches, by=c("standard_for_match"="preferred_input","variable")) %>%
  mutate(
    final_term = case_when(
      variable == "Trait"                   & !is.na(manual_trait_std)    ~ manual_trait_std,
      variable == "Analysis"                & !is.na(manual_analysis_std) ~ manual_analysis_std,
      variable == "Methods"                 & !is.na(manual_methods_std)  ~ manual_methods_std,
      variable == "Software"                & !is.na(manual_software_std) ~ manual_software_std,
      variable %in% c("VC Actor","VCs")     & !is.na(manual_vc_std)       ~ manual_vc_std,
      variable %in% c("Crop","Crops")       & !is.na(manual_crop_std)     ~ manual_crop_std,
      variable == "Publication Type"        & !is.na(manual_pubtype_std)  ~ manual_pubtype_std,
      variable == "Country"                 & !is.na(manual_country_std)  ~ manual_country_std,
      !is.na(onto_term) & onto_term != ""   ~ onto_term,
      TRUE ~ standard_raw
    ),
    final_id = if_else(!is.na(onto_id) & onto_id != "", onto_id, NA_character_),
    
    final_source = case_when(
      variable == "Trait"               & !is.na(manual_trait_std)    ~ "MANUAL_TRAIT",
      variable == "Analysis"            & !is.na(manual_analysis_std) ~ "MANUAL_ANALYSIS",
      variable == "Methods"             & !is.na(manual_methods_std)  ~ "MANUAL_METHODS",
      variable == "Software"            & !is.na(manual_software_std) ~ "MANUAL_SOFTWARE",
      variable %in% c("VC Actor","VCs") & !is.na(manual_vc_std)       ~ "MANUAL_VC",
      variable %in% c("Crop","Crops")   & !is.na(manual_crop_std)     ~ "MANUAL_CROP",
      variable == "Publication Type"    & !is.na(manual_pubtype_std)  ~ "MANUAL_PUBTYPE",
      variable == "Country"             & !is.na(manual_country_std)  ~ "MANUAL_COUNTRY",
      !is.na(source) & source != ""     ~ source,
      TRUE ~ "UNMATCHED_ORIGINAL"
    ),
    
    review_flag = case_when(
      final_source == "UNMATCHED_ORIGINAL"              ~ "NO_MATCH",
      !is.na(best_score) & best_score < 4              ~ "LOW_CONFIDENCE",
      !is.na(score_gap)  & score_gap < 1.5             ~ "AMBIGUOUS_TOP_MATCH",
      !is.na(distance)   & distance  > 0.12            ~ "CHECK_FUZZY_MATCH",
      standard_raw != final_term                        ~ "STANDARDIZED",
      TRUE ~ "OK"
    )
  )

mycodebook <- final_codebook %>%
  transmute(
    standard = standard_raw, variable, original_phrase,
    preferred_input = standard_for_match,
    manual_trait_std, manual_analysis_std, manual_methods_std,
    manual_software_std, manual_vc_std, manual_crop_std,
    agrovoc_query_used,
    final_term, final_id, final_source, review_flag,
    broader1, broader2, broader3,
    onto_term, onto_def, parents, ancestors,
    distance, total_score, best_score, second_score, score_gap,
    n_candidates, sources_found
  ) %>%
  distinct(standard, variable, original_phrase, preferred_input, .keep_all=TRUE) %>%
  arrange(variable, standard)


# Review tables

unique_terms <- dict_clean %>%
  distinct(standard_for_match) %>%
  mutate(tc = normalize_term(standard_for_match)) %>%
  filter(tc != "") %>% pull(tc) %>% unique()

unmatched_terms <- mycodebook %>%
  filter(final_source == "UNMATCHED_ORIGINAL") %>%
  distinct(standard, variable, original_phrase, preferred_input, review_flag)
  
  if (length(unique_terms) > 1) {
    dm  <- stringdist::stringdistmatrix(unique_terms, unique_terms, method="jw")
    hc  <- hclust(as.dist(dm))
    grp <- cutree(hc, h = cfg$cluster_h)
    tibble(term_clean=unique_terms, cluster_id=grp) %>%
      left_join(dict_clean %>%
                  transmute(standard_raw, variable, standard_for_match,
                            term_clean=normalize_term(standard_for_match)) %>% distinct(),
                by="term_clean") %>%
      arrange(cluster_id, term_clean, variable)
  } else {
    tibble(term_clean=unique_terms, cluster_id=1L)
  }


manual_trait_hits <- dict_clean %>%
  filter(variable == "Trait") %>%
  transmute(standard_raw, manual_trait_std, standard_norm, standard_sing, standard_for_match) %>%
  distinct()

# Write xlsx

write_xlsx(
  list(
    codebook              = mycodebook,
    unmatched_terms       = unmatched_terms,
    score_summary         = score_summary,
    manual_trait_hits     = manual_trait_hits
  ),
  path = cfg$output_file
)

cat("\nDone. Output written to:", cfg$output_file, "\n")
