library(shiny)
library(shinythemes)
library(tidyverse)
library(latex2exp)
library(ggpubr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  VariableMean <- reactive({
    data.frame(x = runif(input$MeanPanel), y = 0)
  })
  
  output$plotMean <- renderPlot({
    df  <- data.frame(x = runif(100), y = 0)
    ggplot(VariableMean()) + 
      geom_point(aes(x = x,       y = y,   color = "Data"),      size = 5, alpha = 0.5) + 
      geom_point(aes(x = mean(x), y = y,   color = "Moyenne"),   size = 6, alpha = 0.5) + 
      geom_point(aes(x = 0.5,     y = y,   color = "Espérance"), size = 6, alpha = 0.5) +
      geom_vline(aes(xintercept = mean(x), color = "Moyenne"),   size = 1, alpha = 0.5, linetype = "dashed") + 
      geom_vline(aes(xintercept = 0.5,     color = "Espérance"), size = 1, alpha = 0.5, linetype = "dashed") +
      xlim(c(0,1)) +
      ylim(c(-1,1)) +
      xlab("") +
      ylab("") +
      annotate(geom  = "text",
               x     = 0.1,
               y     = 0.5,
               label = "E[X] = 0.5", 
               color = "blue",
               size  = 10) +
      annotate(geom  = "text",
               x     = 0.1,
               y     = -0.5,
               label = TeX(sprintf(r'($\mu = %f$)', mean(df$x))), 
               color = "red",
               size  = 10) +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      theme_minimal() + 
      theme(axis.text.y  = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x  = element_text(size = 16),
            legend.text  = element_text(size = 16))
  })
  
  VariableHist_Nobs <- reactive({
    input$HistPanel_Nobs
  })
  VariableHist_Nbin <- reactive({
    input$HistPanel_Nbin
  })

    output$HistPlot <- renderPlot({
    dist <- switch(input$HistPanel_Dist, norm = rnorm, unif = runif, runif)
    mean <- switch(input$HistPanel_Dist, norm = 0    , unif = 0.5  , 0.5)
    df <- as.data.frame(dist(VariableHist_Nobs()))
    names(df) <- "x"
    g  <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),      size = 5, alpha = 0.5, shape = 3) +
      geom_point(    aes(x = mean(x), y = 0,     color = "Moyenne"),   size = 6, alpha = 0.5) +
      geom_point(    aes(x = mean,    y = 0,     color = "Espérance"), size = 6, alpha = 0.5) +
      geom_histogram(aes(x = x, y = ..density.., color = "Data"),                alpha = 0.2, show.legend = F, binwidth = VariableHist_Nbin(), center = -1) +
      geom_vline(    aes(xintercept = mean(x),   color = "Moyenne"),   size = 1, alpha = 0.5, linetype = "dashed") +
      geom_vline(    aes(xintercept = mean,      color = "Espérance"), size = 1, alpha = 0.5, linetype = "dashed") +
      xlab("x") +
      ylab("Densité de probabilité") +
      annotate(geom  = "text",
               x     = mean(c(min(df$x), mean)),
               y     = 0.5,
               label = paste0("E[X] = ", mean),
               color = "blue",
               size  = 10) +
      annotate(geom  = "text",
               x     = mean(c(max(df$x), mean)),
               y     = 0.5,
               label = TeX(sprintf(r'($\mu = %f$)', mean(df$x))),
               color = "red",
               size  = 10) +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))

    h <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),                       size = 5, alpha = 0.5, shape = 3) +
      geom_point(    aes(x = mean(x), y = 0,     color = "Moyenne"),                    size = 6, alpha = 0.5) +
      geom_point(    aes(x = mean,    y = 0,     color = "Espérance"),                  size = 6, alpha = 0.5) +
      geom_histogram(aes(x = x, y = cumsum(..count..)/VariableHist_Nobs(), color = "none"),                alpha = 0.2, show.legend = F, binwidth = VariableHist_Nbin(), center = -1) +
      geom_vline(    aes(xintercept = mean(x),   color = "Moyenne"),                    size = 1, alpha = 0.5, linetype = "dashed") +
      geom_vline(    aes(xintercept = mean,      color = "Espérance"),                  size = 1, alpha = 0.5, linetype = "dashed") +
      xlab("x") +
      ylab("Fonction de répartition") +
      annotate(geom  = "text",
               x     = mean(c(min(df$x), mean)),
               y     = 1.0,
               label = paste0("E[X] = ", mean),
               color = "blue",
               size  = 10) +
      annotate(geom  = "text",
               x     = mean(c(max(df$x), mean)),
               y     = 1.0,
               label = TeX(sprintf(r'($\mu = %f$)', mean(df$x))),
               color = "red",
               size  = 10) +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    ggarrange(g, h, legend = F, align = "hv")
  })
  
})
