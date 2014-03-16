# loading useful libraries
library(ggplot2)

# for emulating ggplot2 default colors
ggColors <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

shinyServer(function(input, output) {
  
  #############################################################################
  ## Loi forte des grands nombres
  newDataFunction <- reactive({
    switch(input$dist, "piece"=rbinom(input$nSimu,1,0.5),
           "uniforme"=runif(input$nSimu),
           "gaussien"=rnorm(input$nSimu))
  })
  
  # Densité de la distribution
  output$density <- renderPlot({
    if (input$dist=="piece") {
      barplot(c(0.5,0.5), names=c("pile","face"), col=ggColors(3)[3],
              ylab="Probabilité", main="Distribution d'un tirage de pièce",
              ylim=c(0,1))
    } else {
      x <- seq(-2,2,length=1000)
      y <- switch(input$dist, "uniforme"=dunif(x), "gaussien"=dnorm(x))
      xlimits <- switch(input$dist, "uniforme"=c(0,1), "gaussien"=c(-2,2))
      plot(x, y, type="l", xlab="Valeur", ylab="Probabilité",
           main=paste("Distribution d'une loi", input$dist), xlim=xlimits,
           col=ggColors(3)[1], lwd=2, axes=FALSE)
      box()
      axis(1)
    }
  })
  
  # Sorties
  output$sample <- renderText({
    newData <- newDataFunction()
    if (input$dist=="piece") {
      newData2 <- rep("pile", length(newData))
      newData2[newData==0] <- "face"
      newData <- newData2
    }
    
    head(newData, 50)
  })
  
  # Graphique d'évolution
  output$lgn <- renderPlot({
    newData <- newDataFunction()
    expMean <- switch(input$dist, "piece"=0.5, "uniforme"=0.5, "gaussien"=0)
    evolMean <- cumsum(newData)/1:input$nSimu
    plotData <- data.frame(x=1:input$nSimu, y=evolMean)
    p <- ggplot(plotData, aes(x=x,y=y), colour=ggColors(3)[3]) + geom_line() +
      xlab("Nombre de simulations") + ylab("Moyennes") + 
      geom_hline(yintercept=expMean, colour=ggColors(3)[1], linewidth=2,
                 linetype=2) + ggtitle("Loi des grands nombres")
    print(p)
  })
  
  
  #############################################################################
  ## Sondages
  
  # Création de la population
  n <- 6*10^4
  population <- {
    set.seed(2807)
    grappe <- rep(1:3, rep(n/3,3))
    caractere <- rep("Oui", n)
    temp <- rep(0, n)
    temp[1:(n/3)] <- rbinom(n/3, 1, 0.2)
    temp[((n/3)+1):(2*n/3)] <- rbinom(n/3, 1, 0.5)
    temp[(2*n/3+1):n] <- rbinom(n/3, 1, 0.8)
    caractere[temp==0] <- "Non"
    data.frame(grappe=factor(grappe), caractere=factor(caractere), temp=temp)
  }
  
  # Distribution de P dans les trois groupes
  output$plotPopulation <- renderPlot({
    p <- ggplot(population, aes(grappe, fill=caractere)) +
      geom_bar() + xlab("Groupe") + ylab("Aime la pizza ?") + 
      labs(fill="Aime la pizza ?")
    print(p)
  })
  
  # Fonctions de sondage
  sondageSimple <- function(nbSondes) {
    sondes <- sample(1:n, nbSondes, replace=TRUE)
    simple <- mean(population$temp[sondes])
    simple
  }
  sondageStratifie <- function(nbSondes) {
    sondes <- c(sample(1:(n/3), round(nbSondes/3), replace=TRUE),
                sample((n/3+1):(2*n/3), round(nbSondes/3), replace=TRUE),
                sample((2*n/3+1):n, round(nbSondes/3), replace=TRUE))
    stratifie <- mean(population$temp[sondes])
    stratifie
  }
  
  # sampling...
  sampling <- reactive({
    simple <- sapply(1:input$nSondage, 
                     function(ind) sondageSimple(input$nSample))
    stratifie <- sapply(1:input$nSondage, 
                        function(ind) sondageStratifie(input$nSample))
    list(simple=simple, stratifie=stratifie)
  })
  output$sondageSampling <- renderText({
    sampleData <- sampling()
    paste0("simple : ", head(sampleData$simple, 20), " ; stratifé : ",
           head(sampleData$stratifie, 20), "\n")
  })
  
  # Histogramme de la distribution des valeurs de sondages
  output$sondagePlot <- renderPlot({
    sampleData <- sampling()
    boxplot(data.frame(sampleData$simple, sampleData$stratifie), col="pink")
    abline(h=mean(population$temp), lwd=2, col="darkred")
  })
  
  #############################################################################
  ## Théorème Centrale Limite
  # Moyennes centrées réduites
  newDataFunction2 <- reactive({
    expMean <- switch(input$dist, "piece"=0.5, "uniforme"=0.5, "gaussien"=0)
    expVar <- switch(input$dist, "piece"=0.5, "uniforme"=0.25, "gaussian"=1)/
      sqrt(input$nSimu2)
    allData <- matrix(switch(input$dist,
                             "piece"=rbinom(input$nSimu2*input$nMean,1,0.5),
                             "uniforme"=runif(input$nSimu2*input$nMean),
                             "gaussien"=rnorm(input$nSimu2*input$nMean)),
                      ncol=input$nSimu2, nrow=input$nMean)
    allMeans <- (apply(allData, 1, mean)-expMean)/expVar
    data.frame(allMeans)
  })
  
  output$estMeans <- renderText({
    allMeans <- newDataFunction2()
    allMeans <- allMeans$allMeans
    head(allMeans, 50)
  })
  
  # Histogramme de la distribution des moyennes centrées réduites
  output$tcl <- renderPlot({
    allMeans <- newDataFunction2()
    p <- ggplot(allMeans, aes(allMeans)) +
      geom_histogram(binwidth=0.5, aes(y=..density.., fill=..density..)) +
      xlab("Moyennes") + ylab("Nombre de fois où la moyenne est observée") +
      scale_fill_gradient(low="pink", high="red") + 
      geom_vline(xintercept=0, linewidth=2, linetype=2) + xlim(c(-4,4)) +
      stat_function(fun=dnorm)
    print(p)
  })
  
})