shinyUI(pageWithSidebar(
  
  headerPanel("Le pouvoir de l'alea..."),
  
  sidebarPanel(p(HTML("<form class='well'>Les différents panneaux de cette page
                      illustrent des propriétés importantes des probabilités
                      qui sont utilisées en statistique et, par exemple, lors de
                      sondages.</well>")),
               p(HTML("Ces illustrations sont proposées par
                      <a href='http://www.nathalievilla.org'>Nathalie
                      Villa-Vialaneix</a>. Elles ont été conçues à l'aide du 
                      logiciel <a href='http://cran.univ-paris1.fr'><strong>R
                      </strong></a>. Les scripts sont disponibles sur 
                      <a href='http://www.github.com'>GitHub</a> et peuvent être
                      récupérés avec : <div style='font-size:12px;
                      font-family:courrier; background-color:#FADDF2; 
                      border:1px solid black;'><font color='#870500'><b>
                      git clone <a href='https://github.com/tuxette/alea.git'>
                      https://github.com/tuxette/alea.git</a></b></font>
                      </div>")),
               p(HTML("L'application est utilisable en ligne à : 
                      <div style='font-size:12px; font-family:courrier; 
                      background-color:#FADDF2; border:1px solid black;'>
                      <font color='#870500'><b> <a 
                      href='http://shiny.nathalievilla.org/alea'>
                      http://shiny.nathalievilla.org/alea</a></b></font>
                      </div>")),
               p(HTML('Pour des explications sur les simulations, voir <a href=
                      "http://www.slideshare.net/slideshow/embed_code/32418693">
                      http://www.slideshare.net/slideshow/embed_code/32418693
                      </a>'))),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Loi des grands nombres",
               numericInput('nSimu', 'Nombre de simulations', 20),
               selectInput("dist", "Processus de simulation", 
                           c("piece","uniforme","gaussien")),
               # nombres au hasard
               verbatimTextOutput("sample"),
               # évolution de la moyenne de la simulation
               plotOutput("lgn"),
               # illustration de la distribution choisie
               plotOutput("density")
      ),
      tabPanel("Sondages",
               p(HTML("Description de la population : 3 groupes de personnes<br>
                      P=0,5004<br>
                      Nombre de personnes dans la population : 60 000")),
               plotOutput("plotPopulation"),
               numericInput("nSample", "nombre de sondés", 30),
               numericInput("nSondage", "nombre de sondages", 100),
               p(HTML("Histogramme des valeurs trouvées pour 500 sondages")),
               plotOutput("sondagePlot"),
               p(HTML("Valeurs estimées par sondage : ")),
               verbatimTextOutput("sondageSampling")
      ),
      tabPanel("Théorème Centrale Limite",
               selectInput("dist", "Processus de simulation", 
                           c("piece","uniforme","gaussien")),
               numericInput("nMean", "Nombre de moyennes calculées", 100),
               numericInput('nSimu2', 'Nombre de simulations pour une moyenne',
                            100),
               # moyennes
               p(HTML("Moyennes centrées et réduites")),
               verbatimTextOutput("estMeans"),
               # évolution de la moyenne de la simulation
               plotOutput("tcl")
      ),
      tabPanel("Intervalle de confiance",
               p(HTML("Description de la population : P=0,5004<br>
                      Nombre de personnes dans la population : 60 000")),
               numericInput("nSample2", "nombre de sondés", 30),
               numericInput("nSondage2", "nombre de sondages", 30),
               plotOutput("ICPlot")
      )
    )
  )
))