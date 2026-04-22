# Reads the output of makecodebook.R (fullcodebookclaude1.xlsx) and produces a
# Cellfie-ready Excel workbook that can be loaded directly into Protege via
# Tools > Create axioms from Excel workbook...
#
# This version uses the matched ontology hierarchy to choose parent classes
# where possible, while filtering out generic/junk parents and falling back
# to the top-level class when needed.



pkgs <- c("readxl", "dplyr", "stringr", "tidyr", "purrr", "writexl", "jsonlite")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}



INPUT_FILE  <- "codebook.xlsx"
OUTPUT_FILE <- "cellfie_import.xlsx"
RULES_JSON  <- "cellfie_rules.json"

ONT_PREFIX  <- "https://w3id.org/mh996/grainqualitypref#"

ACCEPTED_FLAGS <- c("OK", "STANDARDIZED", "AMBIGUOUS_TOP_MATCH")
MIN_SCORE <- 0

# Functions

safe_str <- function(x) ifelse(is.na(x) | x == "", "", str_squish(as.character(x)))

to_id <- function(x) {
  x %>%
    safe_str() %>%
    str_replace_all("[/]", " ") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    str_replace_all("_+", "_")
}

dedup <- function(df) {
  df %>%
    arrange(final_term) %>%
    distinct(final_term, .keep_all = TRUE)
}

first_piece <- function(x, split_pat = "\\s*;\\s*") {
  x <- safe_str(x)
  if (x == "") return("")
  parts <- str_split(x, split_pat)[[1]]
  parts <- str_squish(parts)
  parts <- parts[parts != ""]
  if (length(parts) == 0) "" else parts[1]
}

bad_parent <- function(x) {
  y <- str_to_lower(safe_str(x))
  y == "" ||
    str_detect(y, "^http") ||
    y %in% c(
      "na", "thing", "owl:thing", "entity", "quality", "qualities",
      "measure", "measures", "property", "properties",
      "physical quality", "physical object quality"
    ) ||
    str_detect(x, "^[A-Z]?[0-9]+(\\.[0-9]+)*$")
}

choose_ontology_parent <- function(final_source, parents, broader1, default_parent) {
  src <- safe_str(final_source)
  p1  <- safe_str(parents)
  b1  <- safe_str(broader1)
  
  candidate <- case_when(
    src %in% c("AGROVOC") ~ first_piece(b1),
    src %in% c("TO", "PO", "PATO", "STATO", "SOR", "WHEAT", "SWO", "FOODON", "ONS", "MESH", "ENVTHES") ~ first_piece(p1),
    TRUE ~ ""
  )
  
  candidate <- safe_str(candidate)
  if (bad_parent(candidate)) default_parent else candidate
}

build_hier_df <- function(df, default_parent, use_trait_group_first = FALSE) {
  df %>%
    mutate(
      ontology_parent_label = pmap_chr(
        list(final_source, parents, broader1, trait_group),
        function(final_source, parents, broader1, trait_group) {
          if (use_trait_group_first && safe_str(trait_group) != "") {
            safe_str(trait_group)
          } else {
            choose_ontology_parent(final_source, parents, broader1, default_parent)
          }
        }
      ),
      parent_id = to_id(ontology_parent_label)
    )
}

make_parent_sheet <- function(df, root_id, root_label, keep_cols) {
  out <- df %>%
    distinct(parent_id, ontology_parent_label) %>%
    filter(parent_id != "", parent_id != root_id, parent_id != "owl:Thing") %>%
    transmute(
      class_id    = parent_id,
      class_label = ontology_parent_label,
      parent_id   = root_id,
      definition  = "",
      source_id   = "",
      broader1    = "",
      broader2    = "",
      trait_group = ""
    )
  
  root_row <- tibble(
    class_id    = root_id,
    class_label = root_label,
    parent_id   = "owl:Thing",
    definition  = "",
    source_id   = "",
    broader1    = "",
    broader2    = "",
    trait_group = ""
  )
  
  bind_rows(root_row, out) %>%
    distinct(class_id, .keep_all = TRUE) %>%
    select(all_of(keep_cols))
}

rule <- function(...) paste(trimws(c(...)), collapse = "\n")

make_json_rule <- function(comment, dsl, sheet_name,
                           start_col = "A", end_col = "+",
                           start_row = "2", end_row = "+") {
  list(
    sheetName   = sheet_name,
    startColumn = start_col,
    endColumn   = end_col,
    startRow    = start_row,
    endRow      = end_row,
    comment     = comment,
    rule        = dsl,
    active      = TRUE
  )
}

# Read codebook

message("Reading codebook from: ", INPUT_FILE)

cb_raw <- read_excel(INPUT_FILE, sheet = "codebook") %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.)))) %>%
  filter(
    review_flag %in% ACCEPTED_FLAGS | review_flag == "",
    as.numeric(ifelse(best_score == "", "0", best_score)) >= MIN_SCORE | best_score == ""
  ) %>%
  mutate(
    final_term    = safe_str(final_term),
    variable      = safe_str(variable),
    final_id      = safe_str(final_id),
    onto_def      = safe_str(onto_def),
    broader1      = safe_str(broader1),
    broader2      = safe_str(broader2),
    trait_group   = safe_str(trait_group),
    final_source  = safe_str(final_source),
    parents       = safe_str(parents),
    ancestors     = safe_str(ancestors)
  ) %>%
  filter(final_term != "")


# Make dataframes

# Top-level classes sheet
top_classes <- tibble(
  class_id    = to_id(c("Trait","Crop","Country","Method","Software",
                        "Analysis","VCactor","PublicationType")),
  class_label = c("Trait","Crop","Country","Method","Software",
                  "Analysis","Value Chain Actor","Publication Type"),
  parent      = "owl:Thing",
  definition  = c(
    "An observable or measurable characteristic of a crop.",
    "A cultivated plant grown at scale for human use.",
    "A geopolitical entity recognised as a sovereign state or territory.",
    "A procedure or protocol used in research or processing.",
    "A software application or tool used in research.",
    "A statistical or data-analysis technique.",
    "An actor participating in an agricultural value chain.",
    "A type of scholarly or grey-literature publication."
  )
)

# Traits
traits_base <- cb_raw %>%
  filter(variable == "Trait", final_term != "") %>%
  dedup() %>%
  build_hier_df(default_parent = "Trait", use_trait_group_first = FALSE) %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = parent_id,
    definition  = onto_def,
    trait_group = trait_group,
    source_id   = final_id,
    broader1    = broader1,
    broader2    = broader2,
    ontology_parent_label = ontology_parent_label
  )

trait_parent_cols <- c("class_id","class_label","parent_id","definition","trait_group","source_id","broader1","broader2")
trait_parents <- make_parent_sheet(traits_base, "Trait", "Trait", trait_parent_cols)
traits_df <- bind_rows(
  trait_parents,
  traits_base %>% select(all_of(trait_parent_cols))
) %>%
  distinct(class_id, .keep_all = TRUE)

# Crops
crops_base <- cb_raw %>%
  filter(variable %in% c("Crop","Crops"), final_term != "") %>%
  dedup() %>%
  build_hier_df(default_parent = "Crop") %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = parent_id,
    definition  = onto_def,
    broader1    = broader1,
    source_id   = final_id,
    ontology_parent_label = ontology_parent_label
  )

crop_parent_cols <- c("class_id","class_label","parent_id","definition","broader1","source_id")
crop_parents <- make_parent_sheet(crops_base, "Crop", "Crop", crop_parent_cols)
crops_df <- bind_rows(
  crop_parents,
  crops_base %>% select(all_of(crop_parent_cols))
) %>%
  distinct(class_id, .keep_all = TRUE)

# Countries
countries_df <- cb_raw %>%
  filter(variable == "Country", final_term != "") %>%
  dedup() %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = "Country",
    definition  = onto_def
  )

countries_seed <- tibble(
  class_id = "Country",
  class_label = "Country",
  parent_id = "owl:Thing",
  definition = ""
)

countries_df <- bind_rows(countries_seed, countries_df) %>%
  distinct(class_id, .keep_all = TRUE)

# Methods
methods_base <- cb_raw %>%
  filter(variable == "Methods", final_term != "") %>%
  dedup() %>%
  build_hier_df(default_parent = "Method") %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = parent_id,
    definition  = onto_def,
    broader1    = broader1,
    source_id   = final_id,
    ontology_parent_label = ontology_parent_label
  )

generic_parent_cols <- c("class_id","class_label","parent_id","definition","broader1","source_id")
methods_parents <- make_parent_sheet(methods_base, "Method", "Method", generic_parent_cols)
methods_df <- bind_rows(
  methods_parents,
  methods_base %>% select(all_of(generic_parent_cols))
) %>%
  distinct(class_id, .keep_all = TRUE)

# Software
software_df <- cb_raw %>%
  filter(variable == "Software", final_term != "") %>%
  dedup() %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = "Software",
    definition  = onto_def,
    broader1    = broader1,
    source_id   = final_id
  )

software_seed <- tibble(
  class_id = "Software",
  class_label = "Software",
  parent_id = "owl:Thing",
  definition = "",
  broader1 = "",
  source_id = ""
)

software_df <- bind_rows(software_seed, software_df) %>%
  distinct(class_id, .keep_all = TRUE)

# Analysis
analysis_base <- cb_raw %>%
  filter(variable == "Analysis", final_term != "") %>%
  dedup() %>%
  build_hier_df(default_parent = "Analysis") %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = parent_id,
    definition  = onto_def,
    broader1    = broader1,
    source_id   = final_id,
    ontology_parent_label = ontology_parent_label
  )

analysis_parents <- make_parent_sheet(analysis_base, "Analysis", "Analysis", generic_parent_cols)
analysis_df <- bind_rows(
  analysis_parents,
  analysis_base %>% select(all_of(generic_parent_cols))
) %>%
  distinct(class_id, .keep_all = TRUE)

# VC actors
vc_base <- cb_raw %>%
  filter(variable %in% c("VC Actor","VCs"), final_term != "") %>%
  dedup() %>%
  build_hier_df(default_parent = "VCactor") %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = parent_id,
    definition  = onto_def,
    broader1    = broader1,
    source_id   = final_id,
    ontology_parent_label = ontology_parent_label
  )

vc_parents <- make_parent_sheet(vc_base, "VCactor", "Value Chain Actor", generic_parent_cols)
vc_df <- bind_rows(
  vc_parents,
  vc_base %>% select(all_of(generic_parent_cols))
) %>%
  distinct(class_id, .keep_all = TRUE)

# Publication types
pubtype_df <- cb_raw %>%
  filter(variable == "Publication Type", final_term != "") %>%
  dedup() %>%
  transmute(
    class_id    = to_id(final_term),
    class_label = final_term,
    parent_id   = "PublicationType",
    definition  = onto_def,
    source_id   = final_id
  )

pubtype_seed <- tibble(
  class_id = "PublicationType",
  class_label = "Publication Type",
  parent_id = "owl:Thing",
  definition = "",
  source_id = ""
)

pubtype_df <- bind_rows(pubtype_seed, pubtype_df) %>%
  distinct(class_id, .keep_all = TRUE)

# Make rules

rules_top_classes <- rule(
  "Class: @A*(rdf:ID mm:SkipIfEmptyLocation)",
  "  Annotations:",
  "    rdfs:label @B*(xsd:string),",
  "    rdfs:comment @D*(xsd:string mm:ProcessIfEmptyLocation)"
)

rules_traits <- rule(
  "Class: @A*(rdf:ID mm:SkipIfEmptyLocation)",
  "  SubClassOf: @C*(rdf:ID mm:SkipIfEmptyLocation)",
  "  Annotations:",
  "    rdfs:label @B*(xsd:string),",
  "    rdfs:comment @D*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:seeAlso @F*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:isDefinedBy @G*(xsd:string mm:ProcessIfEmptyLocation)"
)

rules_crops <- rule(
  "Class: @A*(rdf:ID mm:SkipIfEmptyLocation)",
  "  SubClassOf: @C*(rdf:ID mm:SkipIfEmptyLocation)",
  "  Annotations:",
  "    rdfs:label @B*(xsd:string),",
  "    rdfs:comment @D*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:isDefinedBy @E*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:seeAlso @F*(xsd:string mm:ProcessIfEmptyLocation)"
)

rules_countries <- rule(
  "Class: @A*(rdf:ID mm:SkipIfEmptyLocation)",
  "  SubClassOf: @C*(rdf:ID mm:SkipIfEmptyLocation)",
  "  Annotations:",
  "    rdfs:label @B*(xsd:string),",
  "    rdfs:comment @D*(xsd:string mm:ProcessIfEmptyLocation)"
)

rules_generic_subclass <- rule(
  "Class: @A*(rdf:ID mm:SkipIfEmptyLocation)",
  "  SubClassOf: @C*(rdf:ID mm:SkipIfEmptyLocation)",
  "  Annotations:",
  "    rdfs:label @B*(xsd:string),",
  "    rdfs:comment @D*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:isDefinedBy @E*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:seeAlso @F*(xsd:string mm:ProcessIfEmptyLocation)"
)

rules_pubtype <- rule(
  "Class: @A*(rdf:ID mm:SkipIfEmptyLocation)",
  "  SubClassOf: @C*(rdf:ID mm:SkipIfEmptyLocation)",
  "  Annotations:",
  "    rdfs:label @B*(xsd:string),",
  "    rdfs:comment @D*(xsd:string mm:ProcessIfEmptyLocation),",
  "    rdfs:seeAlso @E*(xsd:string mm:ProcessIfEmptyLocation)"
)

rules_tbl <- tibble(
  Rule_name = c(
    "TOP_CLASSES - paste into Cellfie; set sheet=top_classes rows 2-end",
    "TRAITS - paste into Cellfie; set sheet=traits rows 2-end",
    "CROPS - paste into Cellfie; set sheet=crops rows 2-end",
    "COUNTRIES - paste into Cellfie; set sheet=countries rows 2-end",
    "METHODS - paste into Cellfie; set sheet=methods rows 2-end",
    "SOFTWARE - paste into Cellfie; set sheet=software rows 2-end",
    "ANALYSIS - paste into Cellfie; set sheet=analysis rows 2-end",
    "VC_ACTORS - paste into Cellfie; set sheet=vc_actors rows 2-end",
    "PUB_TYPES - paste into Cellfie; set sheet=pub_types rows 2-end"
  ),
  MappingMaster_DSL_Rule = c(
    rules_top_classes,
    rules_traits,
    rules_crops,
    rules_countries,
    rules_generic_subclass,
    rules_generic_subclass,
    rules_generic_subclass,
    rules_generic_subclass,
    rules_pubtype
  ),
  Sheet_to_apply_to = c(
    "top_classes","traits","crops","countries",
    "methods","software","analysis","vc_actors","pub_types"
  ),
  Data_starts_row = rep(2, 9),
  Notes = c(
    "Fixed list of top-level classes; no SubClassOf",
    "Traits under ontology-derived or fallback parent",
    "Crops under ontology-derived or fallback parent",
    "Countries as subclasses of Country",
    "Methods under ontology-derived or fallback parent",
    "Software subclasses of Software",
    "Analysis terms under ontology-derived or fallback parent",
    "VC actors under ontology-derived or fallback parent",
    "Publication types under PublicationType"
  )
)


# JSON rules file

rules_list <- list(
  make_json_rule("Top-level domain classes", rules_top_classes,      "top_classes", end_col = "D"),
  make_json_rule("Trait classes",            rules_traits,           "traits",      end_col = "H"),
  make_json_rule("Crop classes",             rules_crops,            "crops",       end_col = "F"),
  make_json_rule("Country classes",          rules_countries,        "countries",   end_col = "D"),
  make_json_rule("Method classes",           rules_generic_subclass, "methods",     end_col = "F"),
  make_json_rule("Software classes",         rules_generic_subclass, "software",    end_col = "F"),
  make_json_rule("Analysis classes",         rules_generic_subclass, "analysis",    end_col = "F"),
  make_json_rule("VC actor classes",         rules_generic_subclass, "vc_actors",   end_col = "F"),
  make_json_rule("Publication type classes", rules_pubtype,          "pub_types",   end_col = "E")
)

cellfie_json <- list(Collections = rules_list)

write(toJSON(cellfie_json, pretty = TRUE, auto_unbox = TRUE), RULES_JSON)
message("Wrote companion rules JSON: ", RULES_JSON)


# Write xlsx

message("Writing Cellfie workbook: ", OUTPUT_FILE)

write_xlsx(
  list(
    TRANSFORMATION_RULES = rules_tbl,
    top_classes          = top_classes,
    traits               = traits_df,
    crops                = crops_df,
    countries            = countries_df,
    methods              = methods_df,
    software             = software_df,
    analysis             = analysis_df,
    vc_actors            = vc_df,
    pub_types            = pubtype_df
  ),
  path = OUTPUT_FILE
)

cat(sprintf("\nOutput Excel : %s\n", OUTPUT_FILE))
cat(sprintf("Output JSON  : %s\n\n", RULES_JSON))
