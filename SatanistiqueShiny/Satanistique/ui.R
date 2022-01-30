library(shiny)

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
                                                 p("Avec cette application, l'objectif est de nous permettre de facilement jouer avec certains paramètres statistiques sur des exemples très simples
                                          pour permettre de mieux les appréhender. L'accent n'est pas mis sur les aspects mathématiques, et aucune connaissance préalable de R n'est requise.", 
                                                   style="text-align:justify;color:black;background-color:white;font-size:20px"),
                                                 br(),
                                                 p(strong("Ne vous inquiétez pas!"), "Dans l'onglet `Quelques liens utiles`, vous trouverez de quoi satisfaire votre curiosité si vous souhaitez aller plus loin.",
                                                   style="text-align:justify;color:black;background-color:white;font-size:20px"),
                                                 br(),
                                                 p("Les éléments suivants sont abordés ici:",
                                                   tags$ul(
                                                     tags$li("Les principaux termes de la statistique descriptive (moyenne, médiane, corrélation et covariance, histogramme..."), 
                                                     tags$li("Une présentation des principales distributions de probabilité (discrètes et continues)"), 
                                                     tags$li("Un point sur le maximum de vraisemblance, sur un cas simple"),
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
                                                 fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                                                          column(
                                                            h3(p(strong("Moyenne et espérance"),style="color:black;text-align:center")),
                                                            width=8,style="background-color:lightgrey;border-radius: 10px"),
                                                          column(width=2, icon("hand-point-left","fa-5x"),align="center")
                                                 ),
                                                 fluidRow(column(width = 2),
                                                          column(width=8,
                                                                 p("La notion la plus importante lorsque l'on cherche à décrire un jeu de données (ie un ensemble de données, 
                                                   issues de calculs, d'expériences, d'observation...) est la moyenne. En statistique, on parle plutôt d'espérance.
                                                   La dénition formelle de cette quantité, pour un jeu de n données, est la suivante (dans cette expression, Xi 
                                                   correspond à la ième observation de la variable aléatoire X):",
                                                                   style="text-align:justify;color:black;background-color:lightgrey;font-size:18px"),
                                                                 withMathJax(),
                                                                 p('$$\\mathbb{E} [X] = \\left\\{ \\begin{aligned} 
                                                                                                \\sum_{i=1}^n p_i x_i \\\\
                                                                                                \\int_{-\\infty}^\\infty f(x) x dx
                                                                                            \\end{aligned}
                                                                                            \\right.
                                                                                      \\longrightarrow      
                                                                   \\frac{1}{n} \\sum_{i=1}^{n} X_i \\ (loi \\ des \\ grands \\ nombres)$$',
                                                                   style="color:black;border:1px solid white;background-color:white;border-radius: 10px;font-size:18px"),
                                                                 style="background-color:lightgrey;border-radius: 10px")
                                                 )),
                                        
                                        tabPanel("Médiane et quantiles"),
                                        tabPanel("Variance et écart-type"),
                                        tabPanel("Distributions jointes et marginales"),
                                        tabPanel("Covariance et corrélation")),
                             navbarMenu("Les principales distributions de probabilité...",
                                        tabPanel("... discrètes"),
                                        tabPanel("... continues")),
                             tabPanel("Le théorème central limite"),
                             tabPanel("Le maximum de vraisemblance"),
                             tabPanel("Et RiskSpectrum dans tout ça?..."),
                             tabPanel("Quelques liens utiles...")),
                  
                  # # Sidebar with a slider input for number of bins
                  # sidebarLayout(
                  #     sidebarPanel(
                  #         sliderInput("bins",
                  #                     "Number of bins:",
                  #                     min = 1,
                  #                     max = 50,
                  #                     value = 30)
                  #     ),
                  # 
                  #     # Show a plot of the generated distribution
                  #     mainPanel(
                  #         plotOutput("distPlot")
                  #     )
                  #)
))
