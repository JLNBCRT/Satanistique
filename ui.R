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
                                                                 tags$img(src = "OnceUponATime.jpg", width = "600px"),
                                                                 br(),
                                                                 br(),
                                                                 withMathJax(),
                                                                 p("Dans le cas de figure ci-dessus, une question s'impose à nous: qui, du mystérieux vengeur
                                                                   Charles Bronson ou du cruel Henry Fonda, tirera le premier? La situation est complexe: l'instant
                                                                   du premier tir est conditionné par de nombreuses variables (la date du dernier rendez-vous avec
                                                                   Claudia Cardinale, la survenue éventuelle d'un orage dans la vallée voisine, bref, la position de
                                                                   tous les atomes dans l'Univers, et ceux, depuis l'origine des temps).",
                                                                   "Suspens insoutenable que le statisticien rationnalise en définissant une
                                                                   application mathématique qui part de l'Univers \\(\\Omega\\) vers l'ensemble \\(\\mathbb{R}^+\\) des réels positifs (l'instant du 
                                                                   premier tir est un nombre réel positif):",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p('$$X: \\Omega \\longrightarrow \\mathbb{R}^+ \\qquad (variable\\ aléatoire:\\ instant\\ du\\ premier\\ tir)$$',
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
                                                                 width = 10,
                                                                 style="background-color:lightblue;border-radius: 10px",align="center")),
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     p("Essayons: ici, on choisit le nombre de réalisation d'une variable aléatoire X, ainsi que le nombre d'intervalles utilisés pour construire l'histogramme:",
                                                       style="text-align:justify;color:black;font-size:18px"),
                                                     # radioButtons("HistPanel_Dist", "",
                                                     #              c("Uniforme" = "unif",
                                                     #                "Normale"  = "norm")),
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
                                                                 max   = 1000,
                                                                 step  = 5)
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
                                                                 p('$$\\sigma = \\frac{1}{n-1} \\sum_{i=1}^{n} \\left(x_i-\\mu \\right)^2 \\ \\longrightarrow
                                                                      Var \\left( X \\right) = \\left\\{ \\begin{aligned} 
                                                                                                \\sum_{i=1}^n p_i \\left(x_i-\\mathbb{E} [X]\\right)^2 \\\\
                                                                                                \\int_{-\\infty}^\\infty f(x) \\left( x - \\mathbb{E} [X] \\right)^2 dx
                                                                                            \\end{aligned}
                                                                                            \\right.$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 p("Ah ah, me direz-vous! Pourquoi utilise-t-on \\(n-1\\) pour calculer l'estimateur de la variance? C'est légèrement compliqué (pas trop), mais
                                                                   l'idée est de construire un estimateur sans biais. Avec les mains: en utilisant l'estimateur \\(\\mu\\) de la moyenne, on a `consommé` un degrès
                                                                   de liberté.",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 p("On note aussi que si la variable aléatoire est une quantité physique (par exemple une longueur), la variance est homogène à la dimension de
                                                                   cette variable au carré (une surface), ce qui complique l'interprétation. Pour y remédier, les statisticiens ont interoduit la notion d'écart-type,
                                                                   qui n'est rien d'autre que la racine carrée de la variance:",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$sd = \\sqrt{\\sigma}$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
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
                                                                 p("Non mais vous ne pensiez pas vous en sortir aussi facilement??",
                                                                   style="text-align:justify;color:black;background-color:lightblue;font-size:18px"),
                                                                 style="background-color:lightblue;border-radius: 10px"))
                                        ),
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
                             tabPanel("Quelques références et liens utiles...",
                                      fluidRow(column(width=1, icon("book","fa-5x"),align="center"),
                                               column(
                                                 h3(p(strong("Quelques références et liens utiles..."),style="color:black;text-align:center")),
                                                 width=10,style="background-color:lightgrey;border-radius: 10px"),
                                               column(width=1, icon("book","fa-5x"),align="center"))
                             )),
                  
))
