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
  tabPanel("Introduction",
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
    tags$h3(tags$b("Introduction")),
    tags$p("This web tool is designed to identify phenotype driven genes in lab evolved E. coli. Two orthogonal approaches are used to answer this question:"),
    tags$h4(tags$b("Evolutionary Action (EA)")),
    tags$p("In breif, Evolutionary Action (EA) models the phenotypic impact of a given amino acid chage comparing to its reference protein sequence. It is defined as:"),
    tags$p("$EA = \\Delta\\phi = \\nabla{f} \\cdot \\Delta\\gamma$", align = "center"),
    tags$p("Here $f$ is the fitness landscape, and $\\nabla{f}$ is the gradient of the fitness landscape. EA or $\\Delta\\phi$ is the fitness response triggered by a small change (mutation) in the genotype $\\Delta\\gamma$. In practice, for a single point coding mutation from amino acid $X$ to $Y$ at a sequence position $i$, $\\nabla{f}$ is approximated by the evolutionary sensitivity of position $i$, given by the Evolutionary Trace (ET) algorithm. And the magnitude of the substitution $\\Delta\\gamma$ is approximated by amino acids substitutions log odds tables. EA has a value between 0-100, where larger values suggest higher functional impact."),
    tags$h4(tags$b("EA integration")),
    tags$p("Genes under selection during evolution will accumulate more mutations with high functional impact. Thus, According to EA theory, the fitness impact of a given set of mutations can be evaluated by integrating the EA scores of these mutations. ", tags$b("EA_KS"), " and", tags$b("EA_sum"), " can be used to approximate EA integration. "),
    tags$p("In the ", tags$b("EA-KS"), " approach, mutated genes were ranked by their EA mutational impact profile with a non-parametric Kolmogorov–Smirnov (KS) test against the in silico random mutation background."),
    tags$p("For ", tags$b("EA-sum"), " approach, EA scores for all codon mutations observed in a gene were summed and compared to the expected values from a random distribution of mutations. The expected background EA-sum values are calculated as EAavg $\\times$ expected mutation count wherein EAavg is the average EA score of all mutations in the in silico simulation and the expected mutation count is determined by the mutation count in the sample and gene length."),
    tags$h4(tags$b("Frequency based method")),
    tags$p("Frequency based analyses are performed based on the assumption that the probability of x mutations occurring in a protein with given length $l$, follows a Poisson distribution with $\\lambda = l \\times m$, where $m$ is the average mutation rate in each dataset. The frequency p-value for each gene was calculated by $p = P[X≥x]$. "),
    tags$br(),
    tags$h3(tags$b("Instructions")),
    tags$h4(tags$b("Step 1. Upload data")),
    tags$p("Input files should follow the format of single sample VCF file. The VCF files should be generated using the reference genome of E. coli K-12 MG1655 (NCBI: U00096.3). A warning will show up if there is entry in the VCF files that does not match to the reference genome. Mutations in the founder strains will be removed from the mutations in the evolve strains. "),
    tags$h4(tags$b("Step 2. Simulate random mutations")),
    tags$p("Using more than 1000 random mutations as background distribution is recommended."),
    tags$h4(tags$b("Step 3. Evolutionary Action and frequency analyses")),
    tags$p("After running the analyses, the gene rankings by EA_KS, EA_sum and Frequency method will be returned as a table and a gene ranking plot. The axes of the gene ranking plot can be adjusted. When selecting a specific gene in the table, that gene will be highlighted in the ranking plot. In addition, the mutations in that gene in the evolve strains will be returned, and their EA distribution will be plotted."),
    tags$p("STRING analysis can be run for the top ranked genes to identify functional pathways that are under selection. By default, top 5% of the mutated genes are input to STRING database."),
    tags$h4(tags$b("Quick EA search")),
    tags$p("Use quick EA search to check the functional impact (EA) of specific mutations of interest."),
    tags$h4(tags$b("Download")),
    tags$p("All tables in the app can be filtered or sorted. Use the \"CSV\" or \"Excel\" button on the top of the tables to download the data. Note that only what is shown in the table will be downloaded/copied. To downloaded/copied the entire list, select \"show all entries\" first."),
    tags$p("The entire MG1655 EA dataset can be downloaded through the download button in the Quick EA search tab."),
    tags$br(),
    tags$h3(tags$b("Acknowledgement")),
    tags$p("This research is based upon work supported [in part] by the Office of the Director of National Intelligence (ODNI), Intelligence Advanced Research Projects Activity (IARPA) under BAA-17-01, contract #2019-19071900001. The views and conclusions contained herein are those of the authors and should not be interpreted as necessarily representing the official policies, either expressed or implied, of ODNI, IARPA, or the U.S. Government."),
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
    )
  )
}

## To be copied in the UI
# mod_intro_ui("intro_1")

## To be copied in the server
# mod_intro_server("intro_1")
