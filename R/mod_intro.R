#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_ui <- function(id){
  ns <- NS(id)
  tabPanel("Background",
           tags$head(
             tags$link(rel="stylesheet",
                       href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
                       integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                       crossorigin="anonymous"),
             HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
             HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
             HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
           ),
    tabsetPanel(
      tabPanel("Introduction",
               style = "width: 80%; margin: auto;",
               img(src="www/favicon.ico", align = "right", height="20%", width="20%"),
               tags$h3(tags$b("Introduction")),
               tags$div(
                 HTML("This Shiny app is designed to identify phenotype driven gene in lab
                    evolved <em>E. coli</em>. It contains three main modules: .
                    "),
                 tags$br(),
                 tags$br(),
                 tags$div(style = "width: 50%;margin-left: 10px;",
                          HTML(
                            "<ul>",
                            "<li><b>Driver Gene Analysis:</b><br>Two orthogonal approaches,
                           EA integration and Frequency based method, are used to identify
                           phenotype driven genes.</li>",
                           "<br>",
                           "<li><b>Quick EA search:</b><br>EA scores predicts the functional impact
                           of protein coding mutations (see details below). This module allows user
                           to quickly search the EA score of any given mutations in the <em>E. coli</em>
                           MG1655 genome.</li>",
                           "<br>",
                           "<li><b>Structure Viewer:</b><br> Mapping mutation data and EA/ET scores to
                           AlphaFold structures.</li>",
                           "</ul>")),
               ),
               tags$br(),
               tags$h4(tags$b("Evolutionary Action (EA)")),
               tags$p("In breif, Evolutionary Action (EA) models the phenotypic
                      impact of a given amino acid chage comparing to its reference
                      protein sequence. It is defined as:"),
               tags$p("$EA = \\Delta\\phi = \\nabla{f} \\cdot \\Delta\\gamma$", align = "center"),
               HTML("Here $f$ is the fitness landscape, and $\\nabla{f}$ is the
                      gradient of the fitness landscape. EA or $\\Delta\\phi$ is the
                      fitness response triggered by a small change (mutation) in the
                      genotype $\\Delta\\gamma$. In practice, for a single point coding
                      mutation from amino acid $X$ to $Y$ at a sequence position
                      $i$, $\\nabla{f}$ is approximated by the evolutionary importance
                      of position $i$, given by the <b>Evolutionary Trace (ET)</b> algorithm.
                      And the magnitude of the substitution $\\Delta\\gamma$ is approximated
                      by amino acids substitutions log odds tables. EA has a value between
                      0-100, where larger values suggest higher functional impact."),
               tags$h4(tags$b("EA integration")),
               tags$p("Genes under selection during evolution will accumulate more mutations
                      with high functional impact. According to EA theory, the fitness
                      impact of a given set of mutations can be evaluated by integrating the
                      EA scores of these mutations. ", tags$b("EA_KS"), " and",
                      tags$b("EA_sum"), " can be used to approximate EA integration."),
               tags$p("In the ", tags$b("EA-KS"), " approach, mutated genes are ranked by
                      their EA mutational impact profile with a non-parametric Kolmogorov–
                      Smirnov (KS) test against a mutation background (random mutations or
                      mutations that occur without the selection of interest."),
               tags$p("For the ", tags$b("EA-sum"), " approach, EA scores for all coding mutations
                      observed in a gene across samples are summed and compared to the expected values from
                      the same mutation background as EA-KS. The expected EA-sum
                      values are calculated as EAavg $\\times$ expected mutation count wherein
                      EAavg is the average EA score of all mutations in the mutation background
                      and the expected mutation count is determined by the mutation count in
                      the samples and gene length."),
               tags$h4(tags$b("Frequency based method")),
               tags$p("Frequency based analyses are performed based on the assumption that
                      the probability of x mutations occurring in a protein with given
                      length $l$, follows a Poisson distribution with $\\lambda = l \\times m$,
                      where $m$ is the average mutation rate in each dataset. The frequency
                      p-value for each gene was calculated by $p = P[X≥x]$. "),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br()),
      tabPanel("Driver gene analysis",
               style = "width: 80%; margin: auto;",
               tags$h3(tags$b("Driver gene analysis")),
               tags$h4(tags$b("Step 1. Upload data")),
               HTML("Mutation data can be input from variant call format (VCF) files or substitution (SUB) files.
                    One file per sample. The VCF or SUB files should be generated using
                    the reference genome of <em>E. coli</em> K-12 MG1655 (NCBI: U00096.3).
                    A warning will show up if there are entries in the VCF/SUB files that do not match
                    to the reference genome. SUB files should contain two columns that are separated by comma (csv file), input_id and SUB,
                    where input_id stores the locus_tags (b4112) or gene names (basS), and SUB stores
                    the amino acid substituion (C84R)."),
               tags$p("Mutations in the founder strains (strains that are sequenced before selection)
                      are less likely to contribute to the adapted phenotype. Thus, they are subtracted
                      out from the evolve strains during the analysis. "),
               tags$h4(tags$b("Step 2: Get background distribution")),
               tags$p("EA integration compares the EA distribution between mutations in the evolve
                      strains and a mutation background (mutations that are not under selecion).
                      This background can be generated by simulating random mutations in the MG1655
                      genome, or by gathering mutations occured when passing MG1655 without selective
                      pressure. Using more than 1000 mutations as background distribution is
                      recommended."),
               tags$h4(tags$b("Step 3. Run analysis")),
               tags$p("After running the analysis, the gene rankings by EA_KS, EA_sum and
                      Frequency method will be returned as a table and a gene ranking plot.
                      The axes of the gene ranking plot can be adjusted. When selecting a
                      specific gene in the table, that gene will be highlighted in the
                      ranking plot. In addition, the mutations in that gene in the evolve
                      strains will be returned, and their EA distribution will be plotted."),
               tags$p("STRING analysis can be run for the top ranked genes to identify
                      functional pathways that are under selection. By default, top 5%
                      of the mutated genes are input to STRING database."),
               tags$h4(tags$b("Step 4. Overlapping genes")),
               tags$p("Genes that are predicted at the top by all methods are more likely
                      to be drivers. Venn diagram is generated for the top genes from all
                      three methods. Click on the the venn diagram to highlight overlapping
                      genes. STRING analysis can be run with the highlighted genes."),
               tags$h4(tags$b("Quick EA search")),
               tags$p("Use quick EA search to check the functional impact (EA) of specific
                      mutations of interest."),
               tags$h4(tags$b("Download")),
               tags$p("All tables in the app can be filtered or sorted. Use the \"CSV\" or
                      \"Excel\" button on the top of the tables to download the data. Note that only what is shown in the table will be downloaded/copied. To downloaded/copied the entire list, select \"show all entries\" first."),
               tags$p("The entire MG1655 EA dataset can be downloaded through the download
                      button in the Quick EA search tab."),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br()),
      tabPanel("Mapping to protein structure",
               style = "width: 80%; margin: auto;",
               tags$h3(tags$b("Mapping to protein structure")),
               HTML("Mutations in driver genes tend to cluster together in protein structure.
                    The Structure Viewer allows users to map values below to <em>E. coli</em>
                    K-12 MG1655 structures from AlphaFold."),
               tags$br(),
               tags$br(),
               tags$div(style = "width: 50%;margin-left: 10px;",
                        HTML(
                          "<ul>",
                          "<li><b>Evolutionary Trace (ET):</b><br> Estimates the evolutionary
                          importance of a residue position in a protein.</li>",
                          "<br>",
                          "<li><b>AlphaFold pLDDT:</b><br> The structure prediction accuracy score
                          from AlphaFold.</li>",
                          "<br>",
                          "<li><b>Mutation count and sumEA:</b><br> Estimates the overall mutational/EA
                          burden on a residue position.</li>",
                          "</ul>")),
               tags$p("Mutation information can be imported from evolve strains (Driver Gene Analysis,
                      step 1) or Quick EA Search."),
               tags$p("The raw values and color code for the structure is available in the Color table.
                      A Pymol session file is available to reproduce the coloring locally in Pymol")),
      tabPanel("Acknowledgement and References",
               style = "width: 80%; margin: auto;",
               tags$h3(tags$b("Acknowledgement")),
               tags$p("This research is based upon work supported [in part] by the Office
                      of the Director of National Intelligence (ODNI), Intelligence Advanced
                      Research Projects Activity (IARPA) under BAA-17-01, contract #2019-19071900001.
                      The views and conclusions contained herein are those of the authors and
                      should not be interpreted as necessarily representing the official
                      policies, either expressed or implied, of ODNI, IARPA, or the U.S.
                      Government."),
               tags$br(),
               tags$h3(tags$b("References")),
               tags$div(
                 style={'padding-left: 20px'},
                 tags$div("Marciano DC, Wang C, Hsu TK, Bourquard T, Atri B, Nehring RB, Abel NS, Bowling EA, Chen TJ, Lurie PD, Katsonis P, Rosenberg SM, Herman C, Lichtarge O.", tags$br(),
                          "Evolutionary action of mutations reveals antimicrobial resistance genes in Escherichia coli.", tags$br(),
                          "Nat Commun. 2022 Jun 9;13(1):3189.",
                          "[",
                          tags$a("link", href="https://www.nature.com/articles/s41467-022-30889-1",  target="_blank", .noWS = "outside"),
                          "]"),
                 tags$br(),
                 tags$div("Katsonis P, Lichtarge O.", tags$br(),
                          "A formal perturbation equation between genotype and phenotype determines the Evolutionary Action of protein-coding variations on fitness.", tags$br(),
                          "Genome Res. 2014 Dec;24(12):2050-8.",
                          "[",
                          tags$a("link", href="https://genome.cshlp.org/content/24/12/2050.long",  target="_blank", .noWS = "outside"),
                          "]")
               ),
      )
    ))
}

## To be copied in the UI
# mod_intro_ui("intro_1")

## To be copied in the server
# mod_intro_server("intro_1")
