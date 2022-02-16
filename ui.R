library(shiny)
library(shinythemes)
library(tidyverse)
library(latex2exp)
library(ggpubr)
library(mvtnorm)
library(ggExtra)
library(plot3D)
library(cowplot)

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
                                      fluidRow(column(tags$img(src = "014-06-19-8-statistique-humour.jpg", width = "100%"),
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
                                                     tags$li("Une illustration du théorème central limite"),
                                                     tags$li("Une approche empirique du maximum de vraisemblance, sur un cas simple"),
                                                     tags$li("Une présentation simple de la régression linéaire et du problème de la prévision (travail en cours)")
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
                                                          width = "50%",
                                                          style="display: block; margin-left: auto; margin-right: auto"),
                                                 br(),
                                                 p("Pour davantage de précisions sur",em("Shiny"),"cliquez",
                                                   a(href="https://shiny.rstudio.com/", "ici",target="_blank"),
                                                   style="text-align:center;color:black;font-size:16px"),
                                                 tags$img(src="shiny-og-fb.jpg", 
                                                          width = "50%",
                                                          style="display: block; margin-left: auto; margin-right: auto"),
                                                 width=2)),
                                      hr(),
                                      p(em("Shiny proposé par Julien Beaucourt (SCEPS/BEPS)"),
                                        style="text-align:right; font-family: times")),
                             navbarMenu("Eléments de vocabulaire",
                                        tabPanel("Variables aléatoires",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Variables aléatoires"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(p("La notion de variable aléatoire, bien que centrale en théorie des probabilités
                                                                    et en statistique, est assez abstraite et sa définition précise peut nécessiter
                                                                    le recours à un formalisme relativement sophistiqué. Prenons plutôt un exemple.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 br(),
                                                                 tags$img(src = "OnceUponATime.jpg", width = "40%"),
                                                                 br(),
                                                                 br(),
                                                                 withMathJax(),
                                                                 p("Dans le cas de figure ci-dessus, une question s'impose à nous: qui, du revanchard
                                                                   Charles Bronson ou du cruel Henry Fonda, tirera le premier? La situation est complexe: l'instant
                                                                   du premier tir est conditionné par de nombreuses variables (la date du dernier rendez-vous avec
                                                                   Claudia Cardinale, la survenue éventuelle d'un orage dans la vallée voisine, bref, la position de
                                                                   tous les atomes dans l'Univers, et ceux, depuis l'origine des temps).",
                                                                   "Suspens insoutenable que le statisticien rationnalise en définissant une
                                                                   application mathématique qui part de l'Univers \\(\\Omega\\) vers l'ensemble \\(\\mathbb{R}^+\\) des réels positifs (l'instant du 
                                                                   premier tir est un nombre réel positif):",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p('$$X: \\Omega \\longrightarrow \\mathbb{R}^+$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("C'est cette fonction X que l'on appelle variable aléatoire. On comprend bien qu'il serait vain de
                                                                   chercher à la caractériser entièrement, même en considérant une portion réduire de l'Univers (c'est l'étape 
                                                                   cruciale de modélisation: par exemple, on peut faire l'hypothèse l'hypothèse que seule la date du dernier 
                                                                   rendez-vous avec Claudia Cardinale influe sur l'instant
                                                                   du premier tir: après tout c'est raisonnable, on peut facilement concevoir qu'un tel rendez-vous - ou son 
                                                                   absence - modifie radicalement l'état de nervosité du valeureux cowboy). Un événement (par exemple, Henry Fonda tire avant 12h45)
                                                                   correspond alors à l'ensemble des états de l'Univers \\(\\Omega\\) tels que cet événement survient. Intuitivement, sa probabilité
                                                                   est simplement la `taille` du sous-espace de \\(\\Omega\\) correspondant à l'événement considéré, divisée par celle de \\(\\Omega\\).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("L'un des objectifs de la statistique descriptive est de caractériser la distribution de probabilité des variables
                                                                   aléatoires d'intérêt (l'instant du premier tir, le résultat d'un lancer de dé, le nombre de défaillance de la pompe
                                                                   RIS 052 PO sur les 10 dernières années) avec un ensemble réduit de paramètres: moyenne, médiane, écart-type, variance,
                                                                   quantiles, etc.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("A ce stade, une discussion sur ces éléments peut s'avérer nécessaire!",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 width = 10,
                                                                 style="background-color:lightblue;border-radius: 10px",align="center"))
                                        ), 
                                        tabPanel("Densités de probabilité et histogrammes",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Densités de probabilité et histogrammes"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(p("Finalement on sent bien que nous n'allons pas nous intéresser à la structure de \\(\\Omega\\), mais plutôt à la distribution
                                                                   de probabilité des événements. Naturellement, ne disposant pas de pouvoirs divins, nous n'avons accès qu'à une portion limitée
                                                                   del'Univers, et nos sens ne nous permettent pas de l'appréhender dans toute sa complexité... Nous n'aurons donc jamais vraiment
                                                                   accès à cette distribution de probabilité: tout ce que nous pourrons faire, c'est essayer de reconstruire ses caractéristiques
                                                                   globales à partir d'observations plus ou moins nombreuses. L'objectif de la statistique descriptive est précisément de construire
                                                                   des estimateurs des caractéristiques sous-jacentes d'une variable aléatoire, à partir de réalisations plus ou moins nombreuses (et
                                                                   entachées ou non d'incertitudes!). En fonction de la structure de l'Univers \\(\\Omega\\), on parlera de variable aléatoire discrète
                                                                   (exemple: le résultat d'un tirage de dé) ou continue (exemple: l'instant du premier tir dans un film de Sergio Leone).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Pour fixer les idées, on peut quand même préciser les notations: traditionnellement en statistique, les variables aléatoires
                                                                      sont notées avec des lettres majuscules (X, Y, etc.), et leurs réalisations par des minuscules (x, y, etc.). Pour préciser la 
                                                                      distribution d'une variable aléatoire, on a le choix entre deux notations (ici pour une variable aléatoire X distribuée selon une loi
                                                                      normal de moyenne \\(\\mu\\) et d'écart-type \\(\\sigma\\) - ces notions vont être précisées par la suite):",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$X \\sim \\mathcal{N}\\left(\\mu, \\sigma\\right) \\qquad \\Leftrightarrow \\qquad f_X\\left(x\\right) = \\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{\\left(x-\\mu\\right)^2}{2\\sigma^2}}$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("Plutôt qu'une longue discussion des propriétés mathématiques de ces distributions de probabilité, faisons appel à notre intuition
                                                                   et voyons en pratique à quoi cela ressemble. Sur la figure ci-dessous, on représente la distribution de probabilité d'une
                                                                   variable aléatoire (peu importe le type de distribution de probabilité). On fait varier le nombre de réalisations observées, et on trace 
                                                                   l'histogramme et la densité de probabilité associée. Sur la partie droite du graphe, on représente la distribution cumulée: sans 
                                                                   formaliser excessivement, cette distribution cumulée correspond simplement à l'intégrale de la densité.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Un point de vigilance: pour les variables aléatoires discrètes (par exemple, le résultat d'un lancer de dé), on donne
                                                                   directement la probabilité de chaque événement, qui est bien défini. Par contre pour une variable aléatoire, on donne
                                                                   la densité de probabilité \\(f_X\\left(x\\right)\\). Il n'y a aucune raison que cette densité soit inférieure à 1. En fait,
                                                                   Le lien avec la probabilité peut s'écrire ainsi:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$\\mathcal{P}\\left(x < X < x+dx \\right) = f_X\\left(x\\right)dx \\Leftrightarrow \\mathcal{P}\\left(X < x\\right) = \\underbrace{\\int_{-\\infty}^x f_X\\left(x\\right)dx}_{\\in [0,1]}$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("En fait, la probabilité d'observer exactement \\(X=x\\) est nulle!",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 width = 10,
                                                                 style="background-color:lightblue;border-radius: 10px",align="center")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on choisit le nombre de réalisation d'une variable aléatoire X, ainsi que le nombre d'intervalles utilisés pour construire l'histogramme:",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     sliderInput("HistPanel_Nobs",
                                                                 "Nombre d'observations:",
                                                                 value = 250,
                                                                 min   = 10,
                                                                 max   = 500,
                                                                 step  = 10),
                                                     sliderInput("HistPanel_Nbin",
                                                                 "Largeur des intervalles:",
                                                                 value = 0.5,
                                                                 min   = 0.025,
                                                                 max   = 1,
                                                                 step  = 0.025),
                                                     sliderInput("HistPanel_Xprob",
                                                                 "Valeur de X d'intérêt:",
                                                                 value = 1,
                                                                 min   = -3,
                                                                 max   = 3,
                                                                 step  = 0.05)
                                                     
                                                   ),
                                                   mainPanel(
                                                     tabPanel("HistPlot", plotOutput("HistPlot",
                                                                                     height = "450px"))
                                                   ))
                                        ),
                                        tabPanel("Moyenne et espérance",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Moyenne et espérance"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")
                                                 ),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("Lorsqu'on cherche à caractériser la distribution de probabilité d'une variable aléatoire, le premier réflexe que l'on a
                                                                 est de préciser la valeur moyenne que l'on s'attend à observer. On voit tout de suite la difficulté: comme nous n'avons pas accès à
                                                                 la distribution de probabilité, nous sommes obligés de nous baser uniquement sur les réalisations de la variable aléatoire (ce qui peut
                                                                 soulever des questions vertigineuses). Pour bien faire la distinction entre la propriété de la distribution de probabilité, et son estimation
                                                                 à partir des réalisations observées:",
                                                                   tags$ul(
                                                                     tags$li("On parle d'espérance pour la quantité associée à la distribution de probabilité;"), 
                                                                     tags$li("Lorsqu'il est question des réalisations, on parle de valeur moyenne.")
                                                                   ),
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Les deux notions sont liées: disons que contrairement à l'espérance, la moyenne est accessible empiriquement, à partir de 
                                                                    données issues d'un calcul, d'une expérience ou d'une observation. Elle ne nécessite aucune connaissance de la distribution 
                                                                    de probabilité (discrète - cas du lancer de dé par exemple - ou continue - cas de la taille des agents du BEPS, par exemple). 
                                                                    Le lien entre ces deux notions est donné par la", em("loi des grands nombres", style="color:red"), ", dont il existe des 
                                                                    variantes - faible ou forte - et dont la discussion nous emmènerait trop loin pour moi dans ce contexte! En deux mots, on peut
                                                                    simplement retenir que lorsque l'on dispose d'un grand nombre d'observations, la moyenne tend vers l'espérance:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$\\mu = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\ \\longrightarrow
                                                                      \\mathbb{E} [X] = \\left\\{ \\begin{aligned} 
                                                                                                \\sum_{i=1}^n p_i x_i \\\\
                                                                                                \\int_{-\\infty}^\\infty f(x) x dx
                                                                                            \\end{aligned}
                                                                                            \\right.
                                                                             
                                                                       \\ (loi \\ des \\ grands \\ nombres)$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
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
                                                                 max   = 100,
                                                                 step  = 1)
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("plotMean",
                                                                                 height = "250px"))
                                                   ))),
                                        tabPanel("Médiane et quantiles",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Médiane et quantiles"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")
                                                 ),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("Souvent, on s'intèresse plutôt à la valeur Q de la variable aléatoire telle que la probabilité d'observer une réalisation 
                                                                   inférieure à cette valeur soit égale à un certaine valeur P.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Cette valeur Q prend le nom de quantile. Le cas particulier P = 0.5 correspond à la médiane.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("La différence entre la moyenne et la médiane n'est pas forcément intuitive. Prenons l'exemple des salaires. Le salaire median
                                                                   en France est de l'ordre de 1800€ (source: INSEE). Cela signifie que si je demande à une personne salariée dans la rue son salaire, il y a une chance sur deux
                                                                   que son salaire soit inférience à 1800€ (on note au passage que cette probabilité est beaucoup plus faible si je restreins mon échantillon
                                                                   au personnel de l'IRSN: c'est la notion de probabitilité conditionnelle qui permet de formaliser cette intuition). Par contre, la valeur moyenne
                                                                   est plutôt de l'ordre de 2240€ (source: INSEE). Cela signifie simplement qu'il y a quelques personnes qui gagnent beaucoup plus que la valeur médiane.
                                                                   Bien sûr, si la distribution était symétrique (des pauvres aussi pauvres que les riches sont riches, ou, de façon plus idéaliste, des riches aussi pauvres
                                                                   que les pauvres sont riches), la médiane et la moyenne seraient identiques. On voit apparaître ici une notion importante, celle de symétrie de la distribution
                                                                   de probabilité (que 'on peut caractériser par des nombres, typiquement la skewness).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on choisit la valeur du quantile...",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     br(),
                                                     sliderInput("MedianPanel",
                                                                 "Valeur du quantile:",
                                                                 value = 0.5,
                                                                 min   = 0,
                                                                 max   = 1,
                                                                 step  = 0.05),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     column(br(),
                                                            tags$head(tags$style("#Median_Quant{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                            textOutput("Median_Quant"),
                                                            br(),
                                                            p("NB: le quantile à 50% porte un nom particulier: c'est la médiane.",
                                                              style="text-align:justify;color:black;font-size:18px"),
                                                            width = 12,
                                                            style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("MedianPlot",
                                                                                 height = "450px"))
                                                   ))),
                                        tabPanel("Intervalles de confiance",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Intervalles de confiance"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("En réalité, c'est plutôt aux intervalles de confiance que l'on s'intéresse au quotidien. On se demande souvent
                                                                   `Quelle sont les bornes de la variable aléatoire X telles que la probabilité d'observer une réalisation entre
                                                                   ces bornes vaille telle valeur?` (n'est-ce pas une de vos préoccupations quotidiennes à vous aussi?).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Rien de nouveau ici: si on connait la probabilité P qu'une observation soit inférieure à une certaine
                                                                   valeur, on connaît immédiatement la probabilité que cette même observation soit supérieure à cette valeur (cette probabilité vaut...?).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Dès lors, les bornes de l'intervalle de confiance sont définies très simplement:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$IC \\left( P \\right) = [ Q \\left(\\frac{1-P}{2}\\right) , Q \\left(\\frac{1+P}{2}\\right) ]$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on choisit largeur de l'intervalle de confiance...",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     br(),
                                                     sliderInput("ICPanel",
                                                                 "Largeur de l'intervalle de confiance (%):",
                                                                 value = 50,
                                                                 min   = 0,
                                                                 max   = 100,
                                                                 step  = 0.5),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     column(br(),
                                                            tags$head(tags$style("#IC{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                            textOutput("IC"),
                                                            br(),
                                                            p("NB: dans le cas présent, on note la symétrie de la distribution de probabilité.",
                                                              style="text-align:justify;color:black;font-size:18px"),
                                                            width = 12,
                                                            style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("ICPlot",
                                                                                 height = "450px"))
                                                   ))
                                        ),
                                        tabPanel("Variance et écart-type",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Variance et écart-type"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("L'intervalle de confiance nous donne une information précieuse sur la dispersion des observations. Toutefois,
                                                                   en pratique, elle nécessite de se fixer une valeur pour le taux de couverture souhaité. Pour éviter toute ambiguité
                                                                   et faciliter la comparaison entre deux populations (au sens large: deux séries de vannes, ou les catégories homme/femme, etc.), on
                                                                   utilise plutôt la notion de variance. Celle-ci est définie simplement comme la moyenne des carrés de la variable aléatoire, centrée
                                                                   sur sa moyenne. Comme précédemment, on n'a pas accès à la distribution de probabilité sous-jacente, on utilise donc un estimateur pour
                                                                   la variance:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$\\sigma^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} \\left(x_i-\\mu \\right)^2 \\ \\longrightarrow
                                                                      Var \\left( X \\right) = \\left\\{ \\begin{aligned} 
                                                                                                \\sum_{i=1}^n p_i \\left(x_i-\\mathbb{E} [X]\\right)^2 \\\\
                                                                                                \\int_{-\\infty}^\\infty f(x) \\left( x - \\mathbb{E} [X] \\right)^2 dx
                                                                                            \\end{aligned}
                                                                                            \\right.$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("Ah ah, me direz-vous! Pourquoi utilise-t-on \\(n-1\\) pour calculer l'estimateur de la variance? C'est légèrement compliqué (pas trop), mais
                                                                   l'idée est de construire un estimateur sans biais. Avec les mains: en utilisant l'estimateur \\(\\mu\\) de la moyenne, on a `consommé` un degré
                                                                   de liberté.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("On note aussi que si la variable aléatoire est une quantité physique (par exemple une longueur), la variance est homogène à la dimension de
                                                                   cette variable au carré (une surface), ce qui complique l'interprétation. Pour y remédier, les statisticiens ont interoduit la notion d'écart-type,
                                                                   qui n'est rien d'autre que la racine carrée de la variance.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Il est intéressant de faire le lien entre la largeur de l'intervalle de confiance et l'écaert-type pour la distribution normale: R est un bon
                                                                   outil pour essayer de comprendre ce lien de manière quantitative, mais on peut également utiliser des tables de lois normales comme celle que l'on 
                                                                   peut voir à la fin de cette page.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on fait varier la dispersion des données mesurées (ou calculées, au passage)...",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     br(),
                                                     sliderInput("VarPanel",
                                                                 "Dispersion des données (écart-type):",
                                                                 value = 1,
                                                                 min   = 0,
                                                                 max   = 10,
                                                                 step  = 0.5),
                                                     sliderInput("SDPlotPanel",
                                                                 "Nombre d'écart-types pour la représentation de l'IC:",
                                                                 value = 1,
                                                                 min   = 1,
                                                                 max   = 3,
                                                                 step  = 0.1),
                                                     br(),
                                                     column(br(),
                                                            tags$head(tags$style("#Var{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                            textOutput("Var"),
                                                            br(),
                                                            tags$head(tags$style("#sd{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                            textOutput("sd"),
                                                            br(),
                                                            width = 12,
                                                            style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("VarPlot",
                                                                                 height = "475px"))
                                                   )),
                                                 br(),
                                                 hr(),
                                                 br(),
                                                 fluidRow(column(tags$img(src = "table normale.png", width = "900px"),
                                                                 width = 12,
                                                                 style="display: block; margin-left: auto; margin-right: auto", align = "center"))),
                                        tabPanel("Distributions jointes et marginales",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Distributions jointes et marginales"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("Toutes ces notions se généralisent facilement à des distributions de probabilité multivariées: on parle alors de distributions
                                                                   jointes. Evidemment
                                                                   les calculs peuvent devenir plus pénibles, de même que les notations. En outre, on dispose de moins de
                                                                   `lois de distribution standard` pour les distributions multivariées: en général on se cantonne aux distributions
                                                                   symétriques (dans un sens qu'il conviendrait de préciser). Au passage, c'est tout l'intérêt des
                                                                   copules (l'un de leurs intérêts en tout cas) que de permettre de décrire des distributions multivariées
                                                                   très générales, mais le propos n'est pas là dans le cadre de cette présentation rapide. Typiquement, on peut
                                                                   chercher à caractériser la distribution de probabilité d'un couple de variables aléatoires (taille/poids, débit/durée 
                                                                   d'une crue, etc.), et on notera (exemple d'une distribution normale à deux dimensions):",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$\\left(X, Y\\right) \\sim \\mathcal{N}\\left(\\boldsymbol{\\mu}, \\boldsymbol{\\Sigma}\\right)
                                                                      \\qquad \\Leftrightarrow \\qquad
                                                                    f_{X, Y}\\left(\\boldsymbol{x}, \\boldsymbol{y} \\right) = \\frac{1}{\\sqrt{2\\pi \\: tr\\left(\\Sigma\\right)}} e^{-\\frac{\\left(\\boldsymbol{x}-\\boldsymbol{\\mu}\\right)\\left(\\boldsymbol{x}-\\boldsymbol{\\mu}\\right)^T}{2 \\boldsymbol{\\Sigma}}}$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("On voit tout de suite se profiler la difficulté. Typiquement, la variance n'est plus simplement un nombre scalaire: c'est une matrice, 
                                                                   dite matrice de variance-covariance, qui décrit non seulement la dispersion des variables (au niveau des termes diagonaux), mais aussi
                                                                   leurs interactions (via les termes extra-diagonaux). Nous y revenons sur la page suivante.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Illustrons cette notion sur un cas simple, avec deux variables indépendantes, distribuées normalement. La densité de probabilité
                                                                   peut toujours être représentée, mais comme c'est une fonction de deux variables, il faut utiliser une représentation 3D, pas forcément
                                                                   commode. En pratique on utilise plutôt des lignes de niveaux, qui sont un peu l'équivalent des histogrammes, `vus de dessus`.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Une quantité importante est la distribution marginale. On l'obtient tout simplement en intégrant toutes les autres variables: en 
                                                                   d'autres termes, cela revient toujours à compter le nombre de réalisations dans un petit intervalle de la variable aléatoire, mais
                                                                   ce comptage est effectué sans tenir compte de la valeur des autres variables (plus exactement, on les prend toutes en compte). Formellement,
                                                                   la distribution jointe de X se calcule ainsi:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$ f_{X} \\left( x \\right) = \\int_{-\\infty}^{+\\infty} f_{X, Y} \\left( \\boldsymbol{x}, \\boldsymbol{y} \\right) dy$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on fait varier la dispersion des variables X et Y, tout en les supposant indépendantes...",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     br(),
                                                     sliderInput("MarginalNsamplePanel",
                                                                 "Nombre de points:",
                                                                 value = 100,
                                                                 min   = 50,
                                                                 max   = 500,
                                                                 step  = 25),
                                                     sliderInput("MarginalXPanel",
                                                                 "Dispersion de la variable aléatoire X (écart-type):",
                                                                 value = 3,
                                                                 min   = 1,
                                                                 max   = 3,
                                                                 step  = 0.25),
                                                     sliderInput("MarginalYPanel",
                                                                 "Dispersion de la variable aléatoire Y (écart-type):",
                                                                 value = 1,
                                                                 min   = 1,
                                                                 max   = 3,
                                                                 step  = 0.25)
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("MarginalPlot",
                                                                                 height = "475px"))
                                                   ))
                                        ),
                                        tabPanel("Covariance et corrélation",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Covariance et corrélation"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("Le coefficient de corrélation permet de quantifier, comme son nom l'indique, le niveau de corrélation entre deux variables. C'est un
                                                                 paramètre normalisé, compris entre -1 et +1.
                                                                   Techniquement, il est calculé à partir de la covariance, qui est le terme qui apparaît sur les termes extra-diagonaux de la matrice
                                                                   de variance-covariance. La covariance entre deux variables aléatoires X et Y est donnée par:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$cov\\left(X, Y\\right) = \\mathbb{E}[(X-\\mathbb{E}[X])(Y-\\mathbb{E}[Y])]
                                                                    \\longrightarrow
                                                                    \\overline{cov}\\left(X, Y\\right) = \\frac{1}{n-1}\\sum_{i=1}^n \\left(X_i-\\mu_X\\right)\\left(Y_i-\\mu_Y\\right)
                                                                   $$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("A partir de cette définition, on voit immédiatement que Cov(X,X)=Var(X). Il existe de nombreuses autres propriétés de la
                                                                   covariance, que nous n'aborderons pas ici. A partir de cette expression, on définit la corrélation par simple normalisation:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$cor\\left(X, Y\\right) = \\frac{\\mathbb{E}[(X-\\mathbb{E}[X])(Y-\\mathbb{E}[Y])]}{\\sqrt{Var\\left(X\\right)Var\\left(Y\\right)}}$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("On peut sans doute ici rappeler le dogme: corrélation n'est pas causalité... Attention à l'interprétation de ces paramètres!",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 tabsetPanel(
                                                   tabPanel("Rotation", 
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                p("Essayons: ici, on fait varier la corrélation entre les deux variables X et Y, mais de manière indirecte: 
                                                       on jour en fait sur l'angle de rotation appliquée aux observations initiales...",
                                                                  style="text-align:justify;color:black;font-size:18px"),
                                                                br(),
                                                                sliderInput("CorAnglePanel",
                                                                            "Angle de rotation:",
                                                                            value = 0,
                                                                            min   = 0,
                                                                            max   = 360,
                                                                            step  = 5),
                                                                sliderInput("CorXPanel",
                                                                            "Dispersion de la variable aléatoire X (écart-type):",
                                                                            value = 3,
                                                                            min   = 1,
                                                                            max   = 3,
                                                                            step  = 0.25),
                                                                sliderInput("CorYPanel",
                                                                            "Dispersion de la variable aléatoire Y (écart-type):",
                                                                            value = 1,
                                                                            min   = 1,
                                                                            max   = 3,
                                                                            step  = 0.25),
                                                                column(br(),         
                                                                       tags$head(tags$style("#VarX{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                       textOutput("VarX"),
                                                                       br(),
                                                                       tags$head(tags$style("#VarY{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                       textOutput("VarY"),
                                                                       br(),
                                                                       tags$head(tags$style("#Cor{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                       textOutput("Cor"),
                                                                       br(),
                                                                       tags$head(tags$style("#Cov{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                       textOutput("Cov"),
                                                                       br(),
                                                                       width = 12,
                                                                       style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                br()
                                                              ),
                                                              mainPanel(
                                                                br(),
                                                                tabPanel("Plot", plotOutput("CorPlot",
                                                                                            height = "600px")))
                                                            ),           
                                                   ),
                                                   tabPanel("Corrélation",
                                                            sidebarPanel(
                                                              p("Essayons: ici, on fait directement varier la corrélation entre les deux variables X et Y, indépendamment
                                                                des variances des variances X et Y...",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              sliderInput("CorValuePanel",
                                                                          "Coefficient de corrélation:",
                                                                          value = 0,
                                                                          min   = -1,
                                                                          max   = 1,
                                                                          step  = 0.1),
                                                              sliderInput("CorX2Panel",
                                                                          "Dispersion de la variable aléatoire X (écart-type):",
                                                                          value = 3,
                                                                          min   = 1,
                                                                          max   = 3,
                                                                          step  = 0.25),
                                                              sliderInput("CorY2Panel",
                                                                          "Dispersion de la variable aléatoire Y (écart-type):",
                                                                          value = 1,
                                                                          min   = 1,
                                                                          max   = 3,
                                                                          step  = 0.25),
                                                              column(br(),
                                                                     tags$head(tags$style("#VarX2{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                     textOutput("VarX2"),
                                                                     br(),
                                                                     tags$head(tags$style("#VarY2{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                     textOutput("VarY2"),
                                                                     br(),
                                                                     tags$head(tags$style("#Cor2{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                     textOutput("Cor2"),
                                                                     br(),
                                                                     tags$head(tags$style("#Cov2{color: black;
                                                                        font-size: 18px;
                                                                        text-align: center;
                                                                        }")),
                                                                     textOutput("Cov2"),
                                                                     br(),
                                                                     width = 12,
                                                                     style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br()
                                                            ),
                                                            mainPanel(
                                                              br(),
                                                              tabPanel("Plot", plotOutput("Cor2Plot",
                                                                                          height = "600px")))
                                                   )
                                                   
                                                 )), 
                                        tabPanel("Au-delà de deux dimensions?",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Au-delà de deux dimensions?"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("Il est possible de représenter des distributions de probabilité jointes pour des triplets 
                                                                   de variables, sous la forme de nuages de points. On aime en général représenter les lignes
                                                                   de niveaux des projections sur les différents plans (à trois dimensions, paires de variables
                                                                   sont envisageables: X-Y, X-Z et Y-Z).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Ici nous allons juste faire varier le nombre de points et l'angle de vue...",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     br(),
                                                     sliderInput("Nbpts3DPanel",
                                                                 "Nombre de points:",
                                                                 value = 2500,
                                                                 min   = 1000,
                                                                 max   = 5000,
                                                                 step  = 100),
                                                     br(),
                                                     sliderInput("Theta3DPanel",
                                                                 "Angle 1:",
                                                                 value = 45,
                                                                 min   = 0,
                                                                 max   = 180,
                                                                 step  = 5),
                                                     br(),
                                                     sliderInput("Phi3DPanel",
                                                                 "Angle 2:",
                                                                 value = 25,
                                                                 min   = 0,
                                                                 max   = 90,
                                                                 step  = 5)
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("D3Plot",
                                                                                 height = "600px"))
                                                   ))
                                        )),
                             navbarMenu("Les principales distributions de probabilité...",
                                        tabPanel("... discrètes",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Les principales ditributions de probabilité discrètes"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("On parle de distribution de probabilité discrète lorsque le support de la variable aléatoire \\(X\\) est discret (en
                                                                   pratique, un sous-ensemble de \\(\\mathbb{Z}\\)).
                                                                   Ici on se propose, pour quelques-unes d'entre elles, de présenter les principales caractérisitques (paramètres, 
                                                                   distribution de probabilité et fonction de répartition, moyenne, médiane, quantiles...).
                                                                   La courte liste proposée ici pourra être complétée à l'envie (par exemple avec la négative-binomiale, 
                                                                   une variante de la loi de Poisson à deux paramètres, utiles pour décrire les phénomènes rares - on parle
                                                                   de distribution `à queue épaisse`).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 tabsetPanel(
                                                   tabPanel("Bernoulli",
                                                            sidebarPanel(
                                                              p("La distribution de Bernoulli permet de décrire des variables aléatoires à deux alternatives (par exemple
                                                                le résultat d'un tirage pile ou face). Elle est donc définie sur le support \\(\\left\\{0,1\\right\\}\\). Le 
                                                                seul paramètre de cette distribution de probabilité est la probabilité de succès du tirage, que l'on note
                                                                traditionnellement \\(p\\).",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$\\mathcal{P}\\left(X=x\\right) = p^x\\left(1-p\\right)^{1-x}
                                                                    \\longrightarrow
                                                                    \\left\\{ 
                                                                      \\begin{aligned} 
                                                                        \\mathbb{E}\\left[ X \\right] = p \\\\
                                                                        Var\\left( X \\right) = p\\left( 1-p \\right)
                                                                      \\end{aligned}
                                                                    \\right.
                                                                   $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                              br(),
                                                              sliderInput("ProbBernoulli",
                                                                          "Probabilité de succès:",
                                                                          value = 0.5,
                                                                          min   = 0,
                                                                          max   = 1,
                                                                          step  = 0.01),
                                                              sliderInput("NBernoulli",
                                                                          "Nombre de tirages:",
                                                                          value = 100,
                                                                          min   = 2,
                                                                          max   = 1000,
                                                                          step  = 2)
                                                            ),
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotBernoulli",
                                                                                          height = "600px"))
                                                            )),
                                                   tabPanel("Binomiale",
                                                            
                                                            sidebarPanel(
                                                              p("La distribution Binomiale permet de décrire le nombre de succès parmi n tirages d'une variable aléatoire
                                                              de Bernoulli. Elle est donc définie sur le support \\(\\left\\{0,n\\right\\}\\). Il s'agit d'une distribution
                                                              à deux paramètres, la probabilité de succès de chaque épreuve de Bernoulli, que l'on note traditionnellement \\(p\\),
                                                                ainsi que le nombre de tirages, que l'on note ici \\(n\\).",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$\\mathcal{P}\\left(X=x\\right) = 
                                                              \\left(
                                                                \\begin{aligned}
                                                                n \\\\
                                                                x
                                                                \\end{aligned}
                                                              \\right) p^x\\left(1-p\\right)^{n-x}
                                                                    \\longrightarrow
                                                                    \\left\\{ 
                                                                      \\begin{aligned} 
                                                                        \\mathbb{E}\\left[ X \\right] = np \\\\
                                                                        Var\\left( X \\right) = np\\left( 1-p \\right)
                                                                      \\end{aligned}
                                                                    \\right.
                                                                   $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                              br(),
                                                              sliderInput("ProbBinom",
                                                                          "Probabilité de succès:",
                                                                          value = 0.5,
                                                                          min   = 0,
                                                                          max   = 1,
                                                                          step  = 0.01),
                                                              sliderInput("SizeBinom",
                                                                          "Nombre de tentatives:",
                                                                          value = 10,
                                                                          min   = 1,
                                                                          max   = 50,
                                                                          step  = 1),
                                                              sliderInput("NBinom",
                                                                          "Nombre de tirages:",
                                                                          value = 100,
                                                                          min   = 2,
                                                                          max   = 1000,
                                                                          step  = 2)),
                                                            
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotBinom",
                                                                                          height = "600px"))
                                                            )
                                                   ),
                                                   tabPanel("Poisson",
                                                            
                                                            
                                                            sidebarPanel(
                                                              p("La distribution de Poisson permet de décrire de très nombreux phénomènes, en particulier à leur 
                                                              comptage (elle est définie sur l'ensemble des entiers naturels). Pour l'introduire 
                                                              correctement, il faudrait faire le lien avec la notion de processus de Poisson. En deux mots, on 
                                                              peut simplement dire que si l'occurrence d'un événement donné (désintégration radioactive, par 
                                                              exemple) respecte les propriétés suivantes:", 
                                                                
                                                                tags$ul(
                                                                  tags$li("Les nombres d'occurrences du phénomène dans deux intervalles de temps séparés (disjoints) 
                                                                          sont indépendants l'un de l'autre"), 
                                                                  tags$li("La probabilité d'observer l'occurrence du phénomène pendant un intervalle de temps 
                                                                          infinitésimal est proportionnel à la longeur de cet intervalle: 
                                                                          \\(\\mathcal{P}\\left(E \\in [t, t+dt]\\right) = \\lambda dt\\)"), 
                                                                  tags$li("La probabilité d'observer plus d'un événement dans un intervalle de temps infinitésimal
                                                                          est nulle")
                                                                ),
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              p("alors le nombre d'occurrences du phénomène pendant une durée \\(T\\) est une variable aléatoire
                                                                distribuée selon une loi de Poisson.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              p("On montre également que l'intervalle de temps séparant deux événements est une variable aléatoire
                                                                distribuée selon une loi exponentielle: cette distribution possède une propriété importante d'absence
                                                                de mémoire, illustré dans la page dédiée à cette distribution.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              p("Dans le domaine de la fiabilité elle joue un rôle important pour modéliser le nombre de défaillance d'un 
                                                                matériel pendant une durée \\(T\\).",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              sliderInput("LambdaPoisson",
                                                                          "Taux de défaillance:",
                                                                          value = 0.1,
                                                                          min   = 0,
                                                                          max   = 1,
                                                                          step  = 0.01),
                                                              sliderInput("TPoisson",
                                                                          "Durée de scrutation:",
                                                                          value = 10,
                                                                          min   = 1,
                                                                          max   = 50,
                                                                          step  = 1),
                                                              sliderInput("NPoisson",
                                                                          "Nombre de tirages:",
                                                                          value = 100,
                                                                          min   = 2,
                                                                          max   = 1000,
                                                                          step  = 2),
                                                              p("Le paramètre clef de cette distribution, outre cette
                                                                       durée de scrutation, est le taux de défaillance \\(\\lambda\\).",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              p("Dans les ouvrages de statistique, cette distribution est caratérisée par un paramètre
                                                                         unique \\(\\theta\\) (ou même \\(\\lambda\\)): en pratique, il est facile de ne pas
                                                                         confondre ces paramètres: le taux de défaillance à la dimension de l'inverse de \\(T\\),
                                                                alors que le paramètre \\(\\theta\\) utilisé en statistique est sans dimension.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$\\mathcal{P}\\left(X=x\\right) = 
                                                                       \\frac{\\left(\\lambda T\\right)^x e^{-\\lambda T}}{x!}
                                                                    \\longrightarrow
                                                                    \\left\\{ 
                                                                      \\begin{aligned} 
                                                                        \\mathbb{E}\\left[ X \\right] = \\lambda T \\\\
                                                                        Var\\left( X \\right) = \\lambda T
                                                                      \\end{aligned}
                                                                    \\right.
                                                                   $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px")
                                                            ),
                                                            
                                                            mainPanel(
                                                              fluidRow(column(tags$img(src = "800px-SimeonDenisPoisson.jpg", width = "400px"),
                                                                              width = 12,
                                                                              style="display: block; margin-left: auto; margin-right: auto", align = "center")),
                                                              br(),
                                                              tabPanel("Plot", plotOutput("PlotPoisson",
                                                                                          height = "800px"))
                                                            )
                                                   )
                                                 )
                                        ),
                                        tabPanel("... continues",
                                                 
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Les principales ditributions de probabilité continues"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("On parle de distribution de probabilité continue lorsque le support de la variable aléatoire \\(X\\) est continu (en
                                                                                                                                                                                      pratique, un sous-ensemble de \\(\\mathbb{R}\\)).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("La classe des distributions continues pour lesquelles on dispose d'une formulation analytique est beaucoup
                                                                 plus vaste que celle des distributions discrètes. on se propose, pour quelques-unes d'entre elles seulement, de présenter les principales caractérisitques (paramètres, 
                                                                                                                                                                                                                             distribution de probabilité et fonction de répartition, moyenne, médiane, quantiles...).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("La courte liste proposée ici pourra être complétée à l'envie (Student, Dirichlet...).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 tabsetPanel(
                                                   tabPanel("Beta",
                                                            
                                                            sidebarPanel(
                                                              p("La distribution Beta est une distribution définie sur le segment \\([0, 1]\\).
                                                                Il s'agit d'une distribution très `flexible`, permettant de décrire un grand nombre
                                                                de situations (symétriques ou disymétriques), y compris la distribution uniforme.",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              p("Cette distribution est caractérisée par deux paramètres de forme \\(\\alpha\\) et \\(\\beta\\).",
                                                                br(),
                                                                sliderInput("AlphaBeta",
                                                                            "Paramètre de forme \\(\\alpha\\):",
                                                                            value = 1,
                                                                            min   = 0,
                                                                            max   = 10,
                                                                            step  = 0.1),
                                                                sliderInput("BetaBeta",
                                                                            "Paramètre de forme \\(\\beta\\):",
                                                                            value = 1,
                                                                            min   = 0,
                                                                            max   = 10,
                                                                            step  = 0.1),
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$f_X\\left(x\\right) = 
                                                                  \\frac{\\Gamma\\left(\\alpha + \\beta \\right)}{\\Gamma\\left(\\alpha \\right)\\Gamma\\left(\\beta \\right)} x^{\\alpha -1} \\left(1-x\\right)^{\\beta -1} \\\\
                                                                \\longrightarrow \\left\\{ 
                                                                  \\begin{aligned} 
                                                                  \\mathbb{E}\\left[ X \\right] = \\frac{\\alpha}{\\alpha + \\beta}  \\\\
                                                                  Var\\left( X \\right) = \\frac{\\alpha \\beta}{\\left(\\alpha + \\beta \\right)^2\\left(\\alpha + \\beta + 1 \\right)}
                                                                  \\end{aligned}
                                                                  \\right.
                                                                  $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px")),
                                                            
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotBeta",
                                                                                          height = "550px"))
                                                            )
                                                            
                                                   ),
                                                   tabPanel("Exponentielle",
                                                            
                                                            sidebarPanel(
                                                              p("On l'a vu, la loi Exponentielle est intimement liée à la loi de Poisson: 
                                                                lorsque des événements surviennent selon un processus de Poisson (événements
                                                                rares), alors le nombre d'événements observés pendant une durée \\(T\\) est
                                                                distribué selon une loi de Poisson de paramètre \\(\\lambda T\\), et les intervalles
                                                                de temps entre deux événements successifs sont distribués selon une loi exponentielle.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              p("La distribution Exponentielle est définie sur l'ensemble des réels positifs, et elle est
                                                                caractérisée par un seul paramètre \\(\\lambda\\) (le même que celui de la loi de Poisson associée):",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              withMathJax(),
                                                              p('$$X \\sim Exp\\left(\\lambda\\right) \\Leftrightarrow f_X\\left(x\\right) = \\lambda e^{-\\lambda x} \\\\
                                                                \\longrightarrow \\left\\{ 
                                                                  \\begin{aligned} 
                                                                  \\mathbb{E}\\left[ X \\right] = \\frac{1}{\\lambda}  \\\\
                                                                  Var\\left( X \\right) = \\frac{1}{\\lambda^2}
                                                                  \\end{aligned}
                                                                  \\right.
                                                                  $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                              sliderInput("LambdaExp",
                                                                          "Taux \\(\\lambda\\):",
                                                                          value = 0.5,
                                                                          min   = 0,
                                                                          max   = 1,
                                                                          step  = 0.05),
                                                              p("La distribution exponentielle est commode pour modéliser les temps de restauration dans nos modèles EPS EI.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              p("Ici on se propose d'illustrer le lien entre la loi exponentielle et la loi de Poison. Pour cela, on génère un
                                                                grand nombre de fois un Univers dans lequel on recense le nombre d'occurences d'un événement X donné pendant une période
                                                                d'un an. Chaque appui sur le bouton ci-dessous génére une nouvelle réalisation de l'Univers.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              actionButton("New", icon("hand-point-right","fa-5x")),
                                                              br(),
                                                              p("A chaque réalisation de l'Univers, on garde le nombre d'événements observés, et on trace l'histogramme. On peut
                                                                alors constater qu'asymptotiquement, la distribution ainsi obtenue converge vers une distribution de Poisson de 
                                                                paramètre \\(\\lambda\\), conformément à l'attendu. On contrôle avec la réglette ci-dessous le nombre de réalisations
                                                                de l'Univers prises en compte.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              sliderInput("NExpPois",
                                                                          "Nombre de répétitions:",
                                                                          value = 100,
                                                                          min   = 10,
                                                                          max   = 1000,
                                                                          step  = 10)
                                                            ),
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotExp",
                                                                                          height = "500px")),
                                                              br(),
                                                              hr(),
                                                              br(),
                                                              tabPanel("Plot", plotOutput("PlotExpGen",
                                                                                          height = "100px")),
                                                              br(),
                                                              hr(),
                                                              br(),
                                                              tabPanel("Plot", plotOutput("PlotExpPois",
                                                                                          height = "450px"))
                                                            )
                                                            
                                                   ),
                                                   tabPanel("Normale",
                                                            
                                                            sidebarPanel(
                                                              p("La distribution Normale est une distribution définie sur l'ensemble des réels. Elle est en général
                                                                  directement paramétrée par sa moyenne \\(\\mu\\) et son écart-type \\(\\sigma\\).",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              sliderInput("MuNorm",
                                                                          "Moyenne \\(\\mu\\):",
                                                                          value = 0,
                                                                          min   = -15,
                                                                          max   = +15,
                                                                          step  = 0.1),
                                                              sliderInput("SDNorm",
                                                                          "Ecart-type \\(\\sigma\\):",
                                                                          value = 1,
                                                                          min   = 0.5,
                                                                          max   = 10,
                                                                          step  = 0.1),
                                                              p("C'est une distribution symétrique (la moyenne et la médiane se condondent), qui joue un rôle très
                                                              important en statistiques, du fait de ses nombreuses propriétés (qui ne seront pas détaillées ici). On peut ainsi démontrer (cf mémo 
                                                                interne BEPS ou ouvrages de référence) que la somme de variables aléatoires distribuées
                                                                normalement est une variable aléatoire dont la distribution est normale. Formellement cela s'écrit:",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              withMathJax(),
                                                              p('$$
                                                                    \\forall i \\in \\left[1,n\\right], X_i \\sim \\mathcal{N}\\left(\\mu_i, \\sigma_i\\right) \\\\
                                                                    \\Rightarrow
                                                                    Y = \\sum_{i=1}^n X_i \\sim \\mathcal{N}\\left(\\sum_{i=1}^n\\mu_i, \\sum_{i=1}^n\\sigma_i\\right)
                                                                 $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                              p("La densité de probabilité s'écrit de la façon suivante:",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$
                                                                    X \\sim \\mathcal{N}\\left(\\mu, \\sigma\\right) \\qquad \\Leftrightarrow \\qquad f_X\\left(x\\right) = \\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{\\left(x-\\mu\\right)^2}{2\\sigma^2}}
                                                                  $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px")
                                                            ),
                                                            
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotNorm",
                                                                                          height = "850px"))
                                                            )
                                                            
                                                   ),
                                                   tabPanel("Lognormale",
                                                            
                                                            
                                                            sidebarPanel(
                                                              p("La distribution LogNormale est une distribution définie sur l'ensemble des réels strictement positifs. Elle est en général
                                                                  caractérisée par deux paramètres \\(\\mu_{SD}\\) et son écart-type \\(\\sigma_{SD}\\): il est important de ne pas confondre
                                                                  ces paramètres avec la moyenne et l'écart-type de la distribution (voir bandeau inférieur pour une relation analytique entre
                                                                ces différents paramètres). En fait, il existe une relation simple entre la médiane de \\(X\\) et le paramètre
                                                                \\(\\mu_{SD}\\): \\(Med\\left(X\\right) = e^{\\mu_{SD}}\\). cette relation est importante dans la paramétrisation
                                                                qui est traditionnellement retenue pour décrire la distribution de probabilité associée à l'alea sismique.",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              sliderInput("MuLNorm",
                                                                          "Paramètre \\(\\mu_{LN}\\):",
                                                                          value = 1,
                                                                          min   = 0,
                                                                          max   = 2,
                                                                          step  = 0.1),
                                                              sliderInput("SDLNorm",
                                                                          "Paramètre \\(\\sigma_{LN}\\):",
                                                                          value = 1,
                                                                          min   = 0.5,
                                                                          max   = 4,
                                                                          step  = 0.1),
                                                              p("Par définition, si le logarithme d'une variable aléatoire est distribué selon une loi Normale, alors cette variable aléatoire
                                                              est distribuée selon une loi Log-Normale:",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              withMathJax(),
                                                              p('$$
                                                                    Y = \\log\\left(X\\right) \\sim \\mathcal{N}\\left(\\mu, \\sigma\\right) 
                                                                  \\Leftrightarrow
                                                                  X \\sim \\mathcal{LN}\\left(\\mu, \\sigma\\right)
                                                                  $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                              p("A partir des propriétés élémentaires du logarithme et de la stabilité par sommation des variables
                                                                Normales, on montre immédiatement que le produit de variables aléatoires LogNormales est une variable
                                                                aléatoire LogNormale.",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              p("La densité de probabilité s'écrit de la façon suivante (on retrouve la structure de la loi Normale, au
                                                                                                                         Jacobien du changement de variable près):",
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              withMathJax(),
                                                              p('$$
                                                                  X \\sim \\mathcal{LN}\\left(\\mu_{SD}, \\sigma_{SD}\\right) \\Leftrightarrow f_X\\left(x\\right) = \\frac{1}{x\\sigma_{SD}\\sqrt{2\\pi}}e^{-\\frac{\\left(\\log\\left(x\\right)-\\mu_{SD}\\right)^2}{2\\sigma_{SD}^2}} \\\\
                                                                  \\longrightarrow \\left\\{ 
                                                                      \\begin{aligned} 
                                                                        \\mathbb{E}\\left[ X \\right] = e^{ \\mu_{SD}+\\sigma_{SD}^2/2}  \\\\
                                                                        Var\\left( X \\right)         = e^{2\\mu_{SD}+\\sigma_{SD}^2} \\left( e^{\\sigma_{SD}^2}-1 \\right)
                                                                      \\end{aligned}
                                                                    \\right.
                                                                $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                            ),
                                                            
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotLNorm",
                                                                                          height = "600px"))
                                                            )
                                                            
                                                   ),
                                                   tabPanel("Gamma",
                                                            
                                                            sidebarPanel(
                                                              p("La distribution Gamma est une distribution définie sur l'ensemble des réels positifs.
                                                                C'est une distribution intéressante pour l'inférence bayésienne (conjuguée de la loi de
                                                                Poisson).",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              p("Plusieurs paramétrisations co-existent pour cette distribution, qui est caractérisée
                                                                par deux paramètres. Ici nous retenons la paramétrisation de RiskSpectrum, dans laquelle
                                                                deux paramètres de forme \\(\\alpha\\) et \\(\\beta\\). En fonction du calcul réalisé, du
                                                                logiciel utilisé, etc., des choix différents pourront être rencontrés, et une vigilance
                                                                est nécessaire pour éviter toute erreur d'interprétation.",
                                                                br(),
                                                                sliderInput("ShapeGamma",
                                                                            "Paramètre de forme \\(\\alpha\\):",
                                                                            value = 1,
                                                                            min   = 0,
                                                                            max   = 10,
                                                                            step  = 0.1),
                                                                sliderInput("RateGamma",
                                                                            "Paramètre de forme \\(\\beta\\):",
                                                                            value = 1,
                                                                            min   = 0,
                                                                            max   = 10,
                                                                            step  = 0.1),
                                                                style="text-align:justify;color:black;font-size:18px"),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$f_X\\left(x\\right) = 
                                                                  \\frac{\\Gamma\\left(\\alpha + \\beta \\right)}{\\Gamma\\left(\\alpha \\right)\\Gamma\\left(\\beta \\right)} x^{\\alpha -1} \\left(1-x\\right)^{\\beta -1} \\\\
                                                                \\longrightarrow \\left\\{ 
                                                                  \\begin{aligned} 
                                                                  \\mathbb{E}\\left[ X \\right] = \\frac{\\alpha}{\\alpha + \\beta}  \\\\
                                                                  Var\\left( X \\right) = \\frac{\\alpha \\beta}{\\left(\\alpha + \\beta \\right)^2\\left(\\alpha + \\beta + 1 \\right)}
                                                                  \\end{aligned}
                                                                  \\right.
                                                                  $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px")),
                                                            
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotGamma",
                                                                                          height = "550px"))
                                                            )
                                                            
                                                   ),
                                                   tabPanel("\\(\\chi^2\\)",
                                                            
                                                            sidebarPanel(
                                                              p("La distribution du Chi2 est une distribution définie sur l'ensemble des réels positifs.
                                                                Elle est directement reliée à la distribution de Poisson, et elle permet de quantifier
                                                                l'intervalle de confiance associé à l'estimation du paramètre \\(\\lambda\\) à partir
                                                                du REX (voir mémo BEPS).",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              p("La base de la définition de la distribution du Chi2 est la suivante. Si \\(n\\) variables
                                                                aléatoires sont distribuées selon des lois normales centrées réduites, alors la somme des
                                                                carrés de ces variables aléatoires est distribuée selon une loi du Chi2 à n degrès de libertés:",
                                                                style="text-align:justify;color:black;font-size:18px"), 
                                                              withMathJax(),
                                                              p('$$
                                                                    \\forall i \\in \\left[1, n\\right], X_i \\sim \\mathcal{N} \\left(0, 1\\right)
                                                                    \\Leftrightarrow
                                                                    Y = \\sum_{i=1}^n X_i^2 \\sim \\chi^2_n
                                                                 $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                              br(),
                                                              sliderInput("KChi2",
                                                                          "Nombre de degrés de liberté (n):",
                                                                          value = 1,
                                                                          min   = 1,
                                                                          max   = 10,
                                                                          step  = 0.5),
                                                              br(),
                                                              withMathJax(),
                                                              p('$$X \\sim \\chi^2_n \\Leftrightarrow f_X\\left(x\\right) = 
                                                                  \\frac{1}{\\Gamma\\left( n/2 \\right) 2^{n/2}} x^{n/2 -1} e^{-x/2} \\\\
                                                                \\longrightarrow \\left\\{ 
                                                                  \\begin{aligned} 
                                                                  \\mathbb{E}\\left[ X \\right] = n  \\\\
                                                                  Var\\left( X \\right) = 2n
                                                                  \\end{aligned}
                                                                  \\right.
                                                                  $$',
                                                                style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"), 
                                                              p("En réalité, la valeur de n n'est pas forcément entière: l'expression analytique de la densité de probabilité
                                                                est compatible avec cette généralisation.",
                                                                style="text-align:justify;color:black;font-size:18px")),
                                                            
                                                            mainPanel(
                                                              tabPanel("Plot", plotOutput("PlotChi2",
                                                                                          height = "550px"))
                                                            )
                                                            
                                                   )
                                                 )
                                        )
                             ),
                             tabPanel("Le théorème central limite",
                                      fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                               column(
                                                 h3(p(strong("Le théorème central limite"),style="color:black;text-align:center")),
                                                 width=10,style="background-color:lightgrey;border-radius: 10px"),
                                               column(width=1, icon("hand-point-left","fa-5x"),align="center")),
                                      fluidRow(column(width = 1),
                                               column(width=10,
                                                      p("Nous avons vu que la loi des grands nombres nous permettait d'estimer l'espérance d'une variable
                                                        aléatoire à partir de la moyenne empirique des observations. Le théorème central limite permet d'être
                                                        beaucoup plus précis, en exprimant non seulement l'erreur commise sur cette estimation en fonction du
                                                        nombre d'observations, mais également en précisant la loi de distribution des moyennes empiriques.",
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p("Plus formellement: soit \\(\\left(X_n\\right)_{n\\geq 1}\\) une suite de variables aléatoires
                                                        indépendantes, de même loi, et admettant une variance \\(\\sigma^2>0\\). Notons \\(m = \\mathbb{E}[X_1]\\)
                                                        leur espérance. Alors, si on pose:",
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p('$$
                                                          Y_n = \\frac{\\mu-m}{\\sigma/\\sqrt{n}}
                                                         $$',
                                                        style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                      p("la suite \\(\\left(Y_n\\right)_{n\\geq 1}\\) converge en loi vers la loi normale centrée réduite:",
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p('$$
                                                          Y_n \\xrightarrow{loi} \\mathcal{N}\\left(0,1\\right)
                                                         $$',
                                                        style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                      p("Cette notion de convergence en loi est assez technique, mais au fond, on sent bien que la notion de
                                                        distance entre deux lois de probabilité doit être plus complexe que lea distance élémentaire dans les
                                                        espaces auxquels nous sommes habitués. En pratique, on peut simplement retenir que la distribution de
                                                        la moyenne empirique ressemble de plus en plus à une loi normale centrée sur l'espérance de la loi
                                                        sous-jacente, et dont la variance tend vers 0 en \\(1/sqrt{n}\\). L'expression précédente peut en effet
                                                        s'écrire:",
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p('$$
                                                          \\mu = \\frac{1}{n} \\sum_{i=1}^n X_i \\xrightarrow{loi} \\mathcal{N}\\left(m = \\mathbb{E}\\left(X_1\\right),\\frac{\\sigma}{\\sqrt{n}}\\right)
                                                         $$',
                                                        style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                      p("Plutôt que de longs discours, nous pouvons essayer de voir comment ce théorème se comporte en pratique.",
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      style="background-color:lightblue;border-radius: 10px")),
                                      br(),
                                      sidebarLayout(
                                        sidebarPanel(
                                          p("Ici, on se propose de choisir la distribution sous-jacente, son espérance et sa variance,
                                                                  ainsi que le nombre de points dans l'échantillon et le nombre d'échantillons...",
                                            style="text-align:justify;color:black;font-size:18px"),
                                          br(),
                                          radioButtons("dist", 
                                                       "Distribution de la variable aléatoire:",
                                                       c("Normal"      = "norm",
                                                         "Log-normal"  = "lnorm",
                                                         "Gamma"       = "gamma")),
                                          sliderInput("TCLmeanPanel",
                                                      "Espérance de la variable aléatoire:",
                                                      value = 2,
                                                      min   = 1,
                                                      max   = 10,
                                                      step  = 0.5),
                                          sliderInput("TCLsdPanel",
                                                      "Variance de la variable aléatoire:",
                                                      value = 1,
                                                      min   = 1,
                                                      max   = 5,
                                                      step  = 0.5),
                                          sliderInput("TCLNsample",
                                                      "Nombre de points dans chaque échantillon:",
                                                      value = 10,
                                                      min   = 10,
                                                      max   = 5000,
                                                      step  = 10),
                                          sliderInput("TCLNhist",
                                                      "Nombre d'échantillons:",
                                                      value = 100,
                                                      min   = 100,
                                                      max   = 5000,
                                                      step  = 100)
                                        ),
                                        mainPanel(
                                          tabPanel("Plot", plotOutput("TCL",
                                                                      height = "700px"))))),
                             navbarMenu("Modélisation",
                                        tabPanel("Le maximum de vraisemblance",
                                                 fluidRow(column(width=1, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Le maximum de vraisemblance"),style="color:black;text-align:center")),
                                                            width=10,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=1, icon("hand-point-left","fa-5x"),align="center")
                                                 ),
                                                 fluidRow(column(width = 1),
                                                          column(width=10,
                                                                 p("Lorsque l'on cherche à modéliser une variable aléatoire quelconque à partir du REX, le plus
                                                                   simple consiste à calculer les caractéristiques empiriques des observations (moyenne, variance, etc.).
                                                                   Des tests plus ou moins sophistiqués permettent de déterminer si les observations sont suffisantes
                                                                   pour conclure à l'évolution ou au contraire au maintien des performances de l'installation ou du système
                                                                   (dans une étude de fiabilité), par exemple.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("Mais au fond, nous n'avons pas encore vraiment vu comment évaluer, à partir de ces données
                                                                   empiriques, les caractéristiques des lois de distribution. Insistons sur un point: la première
                                                                   chose à faire avant d'évaluer les paramètres de la loi de distribution, c'est de choisir la loi
                                                                   de distribution. Il s'agit d'une étape essentielle de la modélisation statistique, souvent
                                                                   guidée par le bon sens, le jugement d'expert, ou éventuellement par des arguments plus profonds.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("On appelle `Vraisemblance` la distribution de probabilité des données, que l'on cherche à caractériser.
                                                                   Dans le cadre d'une approche fréquentiste, n considère que les données observées sont les réalisations les
                                                                   plus probables de la variable aléatoire caractérisée par la vraisemblance. On cherche donc le jeu de
                                                                   paramètres de la loi qui maximise la probabilité des données observées. On peut essayer de le faire à
                                                                   la main pour un cas simple, dans lequel on cherche à caractériser le taux de défaillance d'un système
                                                                   pour lequel on a observé n défaillances sur la dernière année (\\(T=1\\)). On choisit une vraisemblance de Poisson, dont
                                                                   le seul paramètre ajustable (puisque le temps de scrutation est connu) est le taux de défaillance
                                                                   \\(\\lambda\\).",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("On choisit le nombre de défaillances observées sur la dernière année, et on ajuste le taux
                                                       de défaillance pour que le maximum de la distribution de Poisson
                                                       coïncide avec le nombre de défaillances observées.",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     br(),
                                                     sliderInput("NMaxVrais",
                                                                 "Nombre de défaillances:",
                                                                 value = 5,
                                                                 min   = 0,
                                                                 max   = 9,
                                                                 step  = 1),
                                                     br(),
                                                     sliderInput("LambdaMaxVrais",
                                                                 "Taux de défaillance \\(\\lambda\\):",
                                                                 value = .5,
                                                                 min   = 0,
                                                                 max   = 9,
                                                                 step  = 0.01),
                                                     p("Evidemment on ne procède pas au hasard en pratique. La maximisation de la vraisemblance passe tout
                                                       simplement par la recherche du maximum de la distribution de probabilité, en fonction du paramètre 
                                                       \\(\\lambda\\). Et pour simplifier encore davantage, on maximise la log-vraisemblance:",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     p('$$
                                                          \\frac{\\partial}{\\partial \\lambda} \\log \\left( \\frac{e^{-\\lambda T} \\left(\\lambda T\\right)^x}{x!} \\right) 
                                                          \\Leftrightarrow
                                                          \\lambda = \\frac{x}{T}
                                                         $$',
                                                       style="color:black;border:1px solid white;background-color:white;border-radius: 9px;font-size:18px"),
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Plot", plotOutput("PlotMaxVrais",
                                                                                 height = "700px"))))
                                                 
                                        ),
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
                                                 fluidRow(column(tags$img(src = "RegLin.jpg", width = "600px"),
                                                                 width = 12,
                                                                 style="display: block; margin-left: auto; margin-right: auto", align = "center"))
                                        )
                                        
                             ),
                             tabPanel("Quelques références et liens utiles...",
                                      fluidRow(column(width=1, icon("book","fa-5x"),align="center"),
                                               column(
                                                 h3(p(strong("Quelques références et liens utiles..."),style="color:black;text-align:center")),
                                                 width=10,style="background-color:lightgrey;border-radius: 10px"),
                                               column(width=1, icon("book","fa-5x"),align="center")),
                                      br(),
                                      br(),
                                      p(em("Les questions les plus importantes de la vie ne sont, pour la plupart, que des problèmes de probabilité (Pierre Simon de Laplace)"),
                                        style="text-align:center;color:darkgrey;background-color:white;font-size:28px"),
                                      br(),
                                      br(),
                                      fluidRow(column(width = 1),
                                               column(width=10,
                                                      p("De très nombreux ouvrages existent: j'en ai acheté quelques uns, à votre disposition pour les consulter!",
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p("Quelques ouvrages intéressants:",
                                                        tags$ul(
                                                          tags$li("Probabilités pour les non-probabilistes - Walter Appel (H&K éditions) - 38,90€: comme son nom ne 
                                                                  l'indique pas, plutôt très technique et mathématique!"),
                                                          tags$li("Introduction to probability - Dimitri P. Nertsekas & John N. Tsitsiklis (Athena Scientific) - ~110€: 
                                                                  nettement plus abordable!"),
                                                          tags$li("All of statistics - A concise course in statistical inference - Larry Wasserman (Springer) - ~80€: 
                                                                  très complet, beaucoup plus nettement orienté vers les statistiques")),
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p("Plus spécifiquement sur R:",
                                                        tags$ul(
                                                          tags$li("R pour la statistique et la science des données - Sous la direction de François Husson (Presses universitaires de Rennes) - 25€"),
                                                          tags$li("The R book - Michael J. Crawley (Wiley) - 70€"),
                                                          tags$li("Initiation à la statistique avec R - Cours et exercices corrigés - Frédéric Bertrand & Myriam Maumy-Bertrand (Dunod) - 29,90€: en
                                                                  fait j'ai commencé avec ce livre 6 mois après mon arrivée au BEPS...")),
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      p("Internet:",
                                                        tags$ul(
                                                          tags$li(a(href="https://ben-lambert.com/about/", "Le blog de Ben Lambert",target="_blank"))),
                                                        style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                      style="background-color:lightblue;border-radius: 10px"))
                             ))
                  
))
