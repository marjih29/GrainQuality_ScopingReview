library(DiagrammeR)

grViz("
digraph workflow {

  graph [layout = dot, rankdir = TB]

  node [shape = rectangle,
        style = rounded,
        fontname = Helvetica,
        fontsize = 12,
        width = 3.2]

  A [label = 'Raw extraction export\\n(Covidence)']
  B [label = 'Restructure extraction data\\nwide to long format']
  C [label = 'Retain entries with\\ntrait and/or preference data']
  D [label = 'Clean variables\\nrename fields, trim text, correct terms']
  E [label = 'Split multi-response fields\\nmethods, actors, traits, crops, software']
  F [label = 'Standardize coded domains\\nusing dictionaries and hierarchy files']
  G [label = 'Collapse to distinct\\nstudy-category combinations']
  H [label = 'Calculate descriptive summaries\\ncounts and percentages']
  I [label = 'Generate outputs\\nsummary tables and figures']

  A -> B -> C -> D -> E -> F -> G -> H -> I
}
")


grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = TB
  ]

  node [
    shape = rectangle,
    style = rounded,
    fontname = Helvetica,
    fontsize = 11,
    width = 3.1
  ]

  edge [
    fontname = Helvetica,
    fontsize = 10
  ]

  # Main extraction data workflow
  A [label = 'Raw extraction export\\n(Covidence)']
  B [label = 'Restructure extraction data\\nwide to long format']
  C [label = 'Retain entries with\\ntrait and/or preference data']
  D [label = 'Clean extraction fields\\nrename variables, trim text,\\ncorrect inconsistencies']
  E [label = 'Separate multi-response fields\\nmethods, actors, traits, crops,\\nsoftware, disaggregation variables']

  # Coding framework branch
  F [label = 'Codebook / ontology\\npredefined coding domains']
  G [label = 'Dictionaries\\nmethods, value-chain actors,\\njournals, traits, software']
  H [label = 'Hierarchy file\\nterms, categories, groups']
  I [label = 'Standardization rules\\nfuzzy matching and manual harmonization']

  # Merge point
  J [label = 'Standardize extracted terms\\nusing dictionaries and hierarchy']
  K [label = 'Review and resolve\\nunmatched or Other terms']
  L [label = 'Create harmonized analytic dataset']

  # Analysis branch
  M [label = 'Collapse to unique\\nstudy-category combinations']
  N [label = 'Calculate descriptive summaries\\ncounts and percentages']
  O [label = 'Summarize relationships\\namong methods, actors, traits,\\nsoftware, and analysis types']
  P [label = 'Generate outputs\\nsummary tables and figures']

  # Edges
  A -> B -> C -> D -> E -> J

  F -> G
  F -> H
  G -> I
  H -> I
  I -> J

  J -> K -> L -> M -> N -> O -> P

}
")


grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = TB
  ]

  node [
    shape = rectangle,
    style = rounded,
    fontname = Helvetica,
    fontsize = 11,
    width = 3.2
  ]

  edge [
    fontname = Helvetica,
    fontsize = 10
  ]

  A [label = 'Raw extraction export\\n(Covidence)']
  B [label = 'Restructure and clean \\nextraction data']
  D [label = 'Clean extraction fields\\nrename variables, trim text,\\ncorrect inconsistencies']
  E [label = 'Separate multi-response fields,\\nmaintain study IDs']

  F [label = 'Review raw open-ended responses\\nand recurring terms']
  G [label = 'Develop initial dictionaries\\nfrom extracted responses']
  H [label = 'Assign standardized terms,\\ncategories, and groups']
  I [label = 'Apply dictionaries to\\nstandardize responses']
  J [label = 'Review unmatched or ambiguous terms']
  K [label = 'Revise dictionaries and hierarchy\\nthrough iterative refinement']

  L [label = 'Create harmonized analytic dataset']
  M [label = 'Collapse to unique\\nstudy-category combinations']
  N [label = 'Calculate descriptive summaries\\ncounts and percentages']
  O [label = 'Summarize relationships\\namong methods, actors, traits,\\analysis types, etc.']
  P [label = 'Generate output\\nsummary tables and figures']

  A -> B -> D -> E -> F -> G -> H -> I -> J -> L -> M -> N -> O -> P

  J -> K
  K -> G [label = 'Update coding framework']
}
")


grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = TB,
    nodesep = 0.35,
    ranksep = 0.55
  ]

  node [
    shape = plain,
    fontname = Helvetica
  ]

  edge [
    arrowsize = 0.7,
    penwidth = 1.1
  ]

  A [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Raw extraction export</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          Covidence export<BR/>
          study-level and entry-level extraction fields
        </TD>
      </TR>
    </TABLE>
  >]

  B [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Restructure extraction data</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          convert wide to long format<BR/>
          create study-entry structure<BR/>
          retain entries with trait and/or preference data
        </TD>
      </TR>
    </TABLE>
  >]

  C [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Clean raw extraction data</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          rename variables<BR/>
          trim whitespace<BR/>
          correct inconsistencies<BR/>
          identify open-ended and multi-response fields
        </TD>
      </TR>
    </TABLE>
  >]

  D [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Review open-ended responses</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          inspect recurring terms<BR/>
          identify spelling variants and synonyms<BR/>
          review responses within methods, actors, traits,<BR/>
          software, analysis, journals, and disaggregation
        </TD>
      </TR>
    </TABLE>
  >]

  E [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Develop coding framework</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          create dictionaries from raw responses<BR/>
          define standardized terms<BR/>
          assign categories and broader groups<BR/>
          develop hierarchy structure
        </TD>
      </TR>
    </TABLE>
  >]

  F [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Apply coding framework</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          separate multi-response fields<BR/>
          standardize extracted terms<BR/>
          map responses to standardized terms,<BR/>
          categories, and groups
        </TD>
      </TR>
    </TABLE>
  >]

  G [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Review and refine coding</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          inspect unmatched or ambiguous terms<BR/>
          revise dictionaries and hierarchy as needed<BR/>
          retain unresolved responses as Other where appropriate
        </TD>
      </TR>
    </TABLE>
  >]

  H [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Create harmonized analytic dataset</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          generate cleaned, standardized dataset<BR/>
          prepare variables for study-level summaries
        </TD>
      </TR>
    </TABLE>
  >]

  I [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Summarize study-level data</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          collapse to distinct study-category combinations<BR/>
          calculate counts and percentages<BR/>
          summarize methods, actors, traits, journals,<BR/>
          disaggregation, and variable pairings
        </TD>
      </TR>
    </TABLE>
  >]

  J [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR><TD><B>Generate outputs</B></TD></TR>
      <TR>
        <TD ALIGN='CENTER'>
          cleaned extraction dataset<BR/>
          summary tables<BR/>
          descriptive figures and comparisons
        </TD>
      </TR>
    </TABLE>
  >]

  A -> B -> C -> D -> E -> F -> G -> H -> I -> J
  G -> E [label = ' iterative refinement ', fontsize = 10]
}
")

grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = LR,
    nodesep = 0.35,
    ranksep = 0.55
  ]

  node [
    shape = plain,
    fontname = Helvetica
  ]

  edge [
    arrowsize = 0.7,
    penwidth = 1.1
  ]

  A [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Raw extraction export</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          Covidence export<BR/>
          study-level and entry-level extraction fields
        </TD>
      </TR>
    </TABLE>
  >]

  B [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Restructure extraction data</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          convert wide to long format<BR/>
          create study-entry structure<BR/>
          retain entries with trait and/or preference data
        </TD>
      </TR>
    </TABLE>
  >]

  C [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Clean raw extraction data</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          rename variables<BR/>
          trim whitespace<BR/>
          correct inconsistencies<BR/>
          identify open-ended and multi-response fields
        </TD>
      </TR>
    </TABLE>
  >]

  D [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Review open-ended responses</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          inspect recurring terms<BR/>
          identify spelling variants and synonyms<BR/>
          review responses within methods, actors,<BR/>
          traits, software, analysis, journals,<BR/>
          and disaggregation
        </TD>
      </TR>
    </TABLE>
  >]

  E [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Develop coding framework</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          create dictionaries from raw responses<BR/>
          define standardized terms<BR/>
          assign categories and broader groups<BR/>
          develop hierarchy structure
        </TD>
      </TR>
    </TABLE>
  >]

  F [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Apply coding framework</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          separate multi-response fields<BR/>
          standardize extracted terms<BR/>
          map responses to standardized terms,<BR/>
          categories, and groups
        </TD>
      </TR>
    </TABLE>
  >]

  G [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Review and refine coding</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          inspect unmatched or ambiguous terms<BR/>
          revise dictionaries and hierarchy as needed<BR/>
          retain unresolved responses as Other<BR/>
          where appropriate
        </TD>
      </TR>
    </TABLE>
  >]

  H [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Create harmonized analytic dataset</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          generate cleaned, standardized dataset<BR/>
          prepare variables for study-level summaries
        </TD>
      </TR>
    </TABLE>
  >]

  I [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Summarize study-level data</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          collapse to distinct study-category combinations<BR/>
          calculate counts and percentages<BR/>
          summarize methods, actors, traits, journals,<BR/>
          disaggregation, and variable pairings
        </TD>
      </TR>
    </TABLE>
  >]

  J [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'><B>Generate outputs</B></TD>
      </TR>
      <TR>
        <TD WIDTH='300' ALIGN='CENTER'>
          cleaned extraction dataset<BR/>
          summary tables<BR/>
          descriptive figures and comparisons
        </TD>
      </TR>
    </TABLE>
  >]

  A -> B -> C -> D -> E -> F -> G -> H -> I -> J

  G -> E [
    label = ' iterative refinement ',
    fontsize = 10
  ]
}
")


grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = LR,
    nodesep = 0.45,
    ranksep = 0.8
  ]

  node [
    shape = plain,
    fontname = Helvetica
  ]

  edge [
    arrowsize = 0.7,
    penwidth = 1.1,
    fontname = Helvetica,
    fontsize = 10
  ]

  A [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'><B>Raw extraction export</B></TD>
      </TR>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'>
          Covidence export<BR/>
          study-level and entry-level fields
        </TD>
      </TR>
    </TABLE>
  >]

  B [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'><B>Restructure and clean data</B></TD>
      </TR>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'>
          convert wide to long format<BR/>
          retain trait/preference entries<BR/>
          rename variables and trim text<BR/>
          correct inconsistencies
        </TD>
      </TR>
    </TABLE>
  >]

  C [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'><B>Develop coding framework</B></TD>
      </TR>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'>
          review open-ended responses<BR/>
          identify recurring terms and synonyms<BR/>
          create dictionaries<BR/>
          define hierarchy and categories
        </TD>
      </TR>
    </TABLE>
  >]

  D [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'><B>Standardize responses</B></TD>
      </TR>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'>
          separate multi-response fields<BR/>
          map raw responses to<BR/>
          standardized terms, categories,<BR/>
          and groups
        </TD>
      </TR>
    </TABLE>
  >]

  E [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'><B>Create analytic dataset</B></TD>
      </TR>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'>
          review unmatched terms<BR/>
          refine dictionaries as needed<BR/>
          create harmonized dataset<BR/>
          collapse to unique study-category pairs
        </TD>
      </TR>
    </TABLE>
  >]

  F [label = <
    <TABLE BORDER='1' CELLBORDER='0' CELLSPACING='0' CELLPADDING='8'>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'><B>Generate summaries and outputs</B></TD>
      </TR>
      <TR>
        <TD WIDTH='250' ALIGN='CENTER' BALIGN='CENTER'>
          calculate counts and percentages<BR/>
          summarize variable pairings<BR/>
          create tables and figures<BR/>
          export cleaned datasets
        </TD>
      </TR>
    </TABLE>
  >]

  A -> B -> C -> D -> E -> F

  E -> C [
    label = ' iterative refinement ',
    fontsize = 10
  ]
}
")


grViz("
digraph workflow {

  graph [
    layout = dot,
    rankdir = LR,
    nodesep = 0.45,
    ranksep = 0.8
  ]

  node [
    shape = rectangle,
    style = rounded,
    fontname = Helvetica,
    fontsize = 11,
    width = 2.9,
    height = 1.1,
    fixedsize = false,
    margin = 0.18
  ]

  edge [
    arrowsize = 0.7,
    penwidth = 1.1,
    fontname = Helvetica,
    fontsize = 10
  ]

  A [
    label = <<B>Raw extraction export</B><BR/><BR/>
    Covidence export<BR/>
    study-level and entry-level fields>
  ]

  B [
    label = <<B>Restructure and clean data</B><BR/><BR/>
    convert wide to long format<BR/>
    retain trait/preference entries<BR/>
    rename variables and trim text<BR/>
    correct inconsistencies>
  ]

  C [
    label = <<B>Develop coding framework</B><BR/><BR/>
    review open-ended responses<BR/>
    identify recurring terms and synonyms<BR/>
    create dictionaries<BR/>
    define hierarchy and categories>
  ]

  D [
    label = <<B>Standardize responses</B><BR/><BR/>
    separate multi-response fields<BR/>
    map raw responses to<BR/>
    standardized terms, categories,<BR/>
    and groups>
  ]

  E [
    label = <<B>Create analytic dataset</B><BR/><BR/>
    review unmatched terms<BR/>
    refine dictionaries as needed<BR/>
    create harmonized dataset<BR/>
    collapse to unique study-category pairs>
  ]

  F [
    label = <<B>Generate summaries and outputs</B><BR/><BR/>
    calculate counts and percentages<BR/>
    summarize variable pairings<BR/>
    create tables and figures<BR/>
    export cleaned datasets>
  ]

  A -> B -> C -> D -> E -> F

  E -> C [
    label = ' iterative refinement ',
    fontsize = 10
  ]
}
")
