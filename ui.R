library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  
                  tags$head(
                    tags$style(HTML("
                    li {
                    font-size: 18px;

                    }
                    li span {
                    font-size: 18px;
                    }
                    ul {
                    list-style-type: square;
                    }

                    "))
                  ),
                  
                  # Application title
                  titlePanel("Quelques éléments de statistique descriptive"),
                  
                  navbarPage("Ateliers interactifs",
                             tabPanel(icon("home"),
                                      br(),
                                      br(),
                                      p(em("There are three kinds of lies: Lies, Damned Lies, and Statistics (Mark Twain)"),
                                        style="text-align:center;color:darkgrey;background-color:white;font-size:28px"),
                                      br(),
                                      br(),
                                      fluidRow(column(tags$img(src = "014-06-19-8-statistique-humour.jpg", width = "600px"),
                                                      width = 4,
                                                      style="display: block; margin-left: auto; margin-right: auto"),
                                               column(
                                                 br(),
                                                 p("Avec cette application, l'objectif est de nous permettre de manipuler certains paramètres statistiques sur des exemples simples
                                          pour permettre de mieux les appréhender. Volontairement, l'accent n'est pas mis sur les aspects mathématiques, et aucune connaissance préalable de R n'est requise.", 
                                                   style="text-align:justify;color:black;background-color:white;font-size:20px"),
                                                 br(),
                                                 p(strong("Ne vous inquiétez pas!"), "Dans l'onglet `Quelques liens utiles...`, vous trouverez de quoi satisfaire votre curiosité si vous souhaitez aller plus loin.",
                                                   style="text-align:justify;color:black;background-color:white;font-size:20px"),
                                                 br(),
                                                 p("Les éléments suivants sont abordés ici:",
                                                   tags$ul(
                                                     tags$li("Le vocabulaire de la statistique descriptive (moyenne, médiane, histogrammes, corrélation et covariance...)"), 
                                                     tags$li("Une présentation des principales distributions de probabilité (discrètes et continues)"), 
                                                     tags$li("Un point sur le maximum de vraisemblance, sur un cas simple"),
                                                     tags$li("Une illustration du théorème central limite"),
                                                     tags$li("Une illustation sur une sortie de RiskSpectrum (EPS EI 1300, train P4)")
                                                   ),
                                                   style="text-align:justify;color:black;background-color:white;font-size:20px"),
                                                 br(),
                                                 width = 6),
                                               column(
                                                 br(),
                                                 p("Pour davantage de précisions sur",em("R"),"cliquez",
                                                   a(href="https://www.r-project.org/", "ici",target="_blank"),
                                                   style="text-align:center;color:black;font-size:16px"),
                                                 br(),
                                                 tags$img(src="Rlogo.png", 
                                                          width = "150px",
                                                          style="display: block; margin-left: auto; margin-right: auto"),
                                                 br(),
                                                 p("Pour davantage de précisions sur",em("Shiny"),"cliquez",
                                                   a(href="https://shiny.rstudio.com/", "ici",target="_blank"),
                                                   style="text-align:center;color:black;font-size:16px"),
                                                 tags$img(src="shiny-og-fb.jpg", 
                                                          width = "200px",
                                                          style="display: block; margin-left: auto; margin-right: auto"),
                                                 width=2)),
                                      hr(),
                                      p(em("Shiny proposé par Julien Beaucourt (SCEPS/BEPS)"),
                                        style="text-align:right; font-family: times")),
                             navbarMenu("Eléments de vocabulaire",
                                        tabPanel("Moyenne et espérance",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Moyenne et espérance"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")
                                                 ),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("La notion la plus importante lorsque l'on cherche à décrire un jeu de données (ie un ensemble de données, 
                                                                    issues de calculs, d'expériences, d'observation...) est la", em("moyenne.", style="color:red"), "En statistique, on parle 
                                                                    plutôt d'",em("espérance.", style="color:red"),
                                                                   "Les deux notions sont liées: disons que contrairement à l'espérance, la moyenne est accessible empiriquement, à partir de 
                                                                    données issues d'un calcul, d'une expérience ou d'une observation. Elle ne nécessite aucune connaissance de la distribution 
                                                                    de probabilité (discrète - cas du lancer de dé par exemple - ou continue - cas de la taille des agents du BEPS, par exemple). 
                                                                    Le lien entre ces deux notions est donné par la", em("loi des grands nombres", style="color:red"), ", dont il existe des 
                                                                    variantes - faible ou forte - et dont la discussion nous emmènerait trop loin pour moi dans ce contexte! En deux mots, on peut
                                                                    simplement retenir que lorsque l'on dispose d'un grand nombre d'observations, la moyenne tend vers l'espérance.",
                                                                   style="text-align:justify;color:black;background-color:lightgrey;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$\\mu = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\ \\longrightarrow
                                                                      \\mathbb{E} [X] = \\left\\{ \\begin{aligned} 
                                                                                                \\sum_{i=1}^n p_i x_i \\\\
                                                                                                \\int_{-\\infty}^\\infty f(x) x dx
                                                                                            \\end{aligned}
                                                                                            \\right.
                                                                             
                                                                       \\ (loi \\ des \\ grands \\ nombres)$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 style="background-color:lightgrey;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on fait simplement varier le nombre de points tirés aléatoirement dans l'intervalle
                                                       [0,1] et on observe l'impact sur la valeur moyenne (par rapport à l'espérance, qui est fixe, et qui
                                                       vaut...)",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     sliderInput("MeanPanel",
                                                                 "Nombre d'observations:",
                                                                 value = 10,
                                                                 min   = 1,
                                                                 max   = 1000,
                                                                 step  = 5)
                                                     
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("plotMean",
                                                                                 height = "250px"))
                                                   ))),
                                        tabPanel("Histogrammes",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Histogrammes"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")
                                                 ),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("La représentation des données unidimensionnelles sur un simple axe ne permet pas, nous l'avons vu, de bien rendre compte de la distribution
                                                                    des points sur le support de la variable aléatoire. Une représentation plus commode est l'", em("histogramme", style="color:red"), ": le support (ou, en pratique, l'intervalle
                                                                    borné par les valeurs minimale et maximale de la réalisation de la variable aléatoire) est divisé en un certain nombre d'intervalles, et on compte
                                                                    le nombre de points présents dans chacun de ces intervalles: ce nombre est représenté par une colonne verticale. Attention: souvent, c'est la ", em("densité", style="color:red"), "
                                                                    que l'on représente (c'est-à-dire le nombre de points dans l'intervalle, par unité de longueur, et divisé par le nombre total de points). Ceci permet de comparer des 
                                                                    distributions entre elles, même lorsque le nombre de points observés est différent pour chacune des deux variables aléatoires.",
                                                                   style="text-align:justify;color:black;background-color:lightgrey;font-size:18px"),
                                                                 p("Une autre représentation intéressante est l' ", em("histogramme cumulé", style="color:red"), ": cette fois, on compte le nombre de points présents dans l'intervalle, et dans
                                                                    tous les intervalles précédents. Comme précédemment, on rencontre plus fréquemment une telle représentation normalisée par le nombre de points
                                                                    observés. Quelle est alors la valeur limite lorsque la valeur de la variable aléatoire tend vers sa borne supérieure? Cette limite est-elle atteinte?",
                                                                   style="text-align:justify;color:black;background-color:lightgrey;font-size:18px"),
                                                                 p("Lorsque le nombre de points et le nombre d'intervalle tendent vers l'infini, on observe en fait la distribution de probabilité de 
                                                                   variable alétaoire, et sa", em("distribution cumulée", style="color:red")," et sa", em("fonction de répartition", style="color:red"), ". Ces notions sont illustrées dans l'onglet dédié, pour quelques distributions fameuses (et savoureuses).",
                                                                   style="text-align:justify;color:black;background-color:lightgrey;font-size:18px"),
                                                                 style="background-color:lightgrey;border-radius: 10px")
                                                 ),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on choisit la distribution de probabilité, le nombre de points, ainsi que le nombre d'intervalles:",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     radioButtons("HistPanel_Dist", "",
                                                                  c("Uniforme" = "unif",
                                                                    "Normale"  = "norm")),
                                                     sliderInput("HistPanel_Nobs",
                                                                 "Nombre d'observations:",
                                                                 value = 10,
                                                                 min   = 10,
                                                                 max   = 500,
                                                                 step  = 10),
                                                     sliderInput("HistPanel_Nbin",
                                                                 "Largeur des intervalles:",
                                                                 value = 1,
                                                                 min   = 0.025,
                                                                 max   = 1,
                                                                 step  = 0.025)
                                                     
                                                   ),
                                                   mainPanel(
                                                     tabPanel("HistPlot", plotOutput("HistPlot",
                                                                                     height = "400px"))
                                                   ))),
                                        tabPanel("Médiane et quantiles"),
                                        tabPanel("Variance et écart-type"),
                                        tabPanel("Distributions jointes et marginales"),
                                        tabPanel("Covariance et corrélation")),
                             navbarMenu("Les principales distributions de probabilité...",
                                        tabPanel("... discrètes"),
                                        tabPanel("... continues")),
                             tabPanel("Le théorème central limite"),
                             navbarMenu("Modélisation",
                                        tabPanel("Le maximum de vraisemblance"),
                                        tabPanel("Régression linéaire",
                                                 fluidRow(column(tags$img(src = "WIP.png", width = "100px"),
                                                                 width = 1,
                                                                 style="display: block; margin-left: auto; margin-right: auto",align="center"),
                                                          column(
                                                            h3(p(strong("Régression linéaire"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(tags$img(src = "WIP.png", width = "100px"),
                                                                 width = 1,
                                                                 style="display: block; margin-left: auto; margin-right: auto",align="center")),
                                                 fluidRow(column(tags$img(src = "Work_in_progress.jpg", width = "600px"),
                                                                 width = 12,
                                                                 style="display: block; margin-left: auto; margin-right: auto", align = "center"))
                                                 )
                             ),
                             tabPanel("Et RiskSpectrum dans tout ça?...",
                                      fluidRow(column(tags$img(src = "RiskSpectrum1.png", width = "100px"),
                                                      width = 1,
                                                      style="display: block; margin-left: auto; margin-right: auto",align="center"),
                                               column(
                                                 h3(p(strong("Et RiskSpectrum dans tout ça?..."),style="color:black;text-align:center")),
                                                 width=10,style="background-color:lightgrey;border-radius: 10px"),
                                               column(tags$img(src = "RiskSpectrum1.png", width = "100px"),
                                                      width = 1,
                                                      style="display: block; margin-left: auto; margin-right: auto",align="center"))),
                             tabPanel("Quelques liens utiles...",
                                      fluidRow(column(width=1, icon("book","fa-5x"),align="center"),
                                               column(
                                                 h3(p(strong("Quelques liens utiles..."),style="color:black;text-align:center")),
                                                 width=10,style="background-color:lightgrey;border-radius: 10px"),
                                               column(width=1, icon("book","fa-5x"),align="center"))
                             )),
                  
))
