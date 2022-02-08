library(shiny)
library(shinythemes)
library(tidyverse)
library(latex2exp)
library(ggpubr)
library(mvtnorm)
library(ggExtra)
library(plot3D)

source("Functions.R")

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
  VariableHist_Xprob <- reactive({
    input$HistPanel_Xprob
  })
  
  output$HistPlot <- renderPlot({
    # dist <- switch(input$HistPanel_Dist, norm = rnorm, unif = runif, runif)
    # mean <- switch(input$HistPanel_Dist, norm = 0    , unif = 0.5  , 0.5)
    df  <- as.data.frame(rnorm(VariableHist_Nobs()))
    tmp <- runif(1e3, -3, 3)
    df2 <- data.frame(x = tmp, y = dnorm(tmp), z = pnorm(tmp))
    names(df) <- "x"
    g  <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),      size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = ..density.., color = "Data"),                alpha = 0.2, show.legend = F, binwidth = VariableHist_Nbin(), center = -1) +
      geom_area(data = df2[df2$x<VariableHist_Xprob(),], aes(x = x, y = y), fill = "orange", alpha = 0.25, size = 1.5) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      xlab("x") +
      ylab("Densité de probabilité") +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      ylim(c(0,0.5)) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    h <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),                       size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = cumsum(..count..)/VariableHist_Nobs(), color = "none"),                alpha = 0.2, show.legend = F, binwidth = VariableHist_Nbin(), center = -1) +
      geom_segment(aes(x = VariableHist_Xprob() , y = 0,                              xend = VariableHist_Xprob(), yend = pnorm(VariableHist_Xprob())), color = "orange", size = 0.8) +
      geom_segment(aes(xend = min(-3,min(x)) , y = pnorm(VariableHist_Xprob()),    x = VariableHist_Xprob(), yend = pnorm(VariableHist_Xprob())), arrow = arrow(length = unit(0.03, "npc")), color = "orange", size = 0.8) +
      stat_function(fun = pnorm, color = "orange", size = 1.5, xlim = c(-3,3)) +
      xlab("x") +
      ylab("Fonction de répartition") +
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
  
  VariableMedian_Quant <- reactive({
    input$MedianPanel
  })
  
  output$Median_Quant <- renderText({
    
    quant <- round(qnorm(as.double(VariableMedian_Quant())), 3)
    
    quant_txt <- paste0("Q (",round(VariableMedian_Quant()*100),"%) = ", quant)
    quant_txt
    
  })
  
  output$MedianPlot <- renderPlot({
    nb <- 2.5e2
    df  <- as.data.frame(rnorm(nb))
    tmp <- runif(1e3, -3, 3)
    df2 <- data.frame(x = tmp, y = dnorm(tmp), z = pnorm(tmp))
    names(df) <- "x"
    g  <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),      size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = ..density.., color = "Data"),                alpha = 0.2, show.legend = F, binwidth = 0.25, center = -1) +
      geom_area(data = df2[df2$x<qnorm(VariableMedian_Quant()),], aes(x = x, y = y), fill = "orange", alpha = 0.25, size = 1.5) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      xlab("x") +
      ylab("Densité de probabilité") +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      xlim(c(-3,3)) +
      ylim(c(0,0.5)) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    h <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"), size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = cumsum(..count..)/nb, color = "none"), alpha = 0.2, show.legend = F, binwidth = 0.25, center = -1) +
      geom_segment(aes(x = qnorm(VariableMedian_Quant()) , yend = 0, xend = qnorm(VariableMedian_Quant()), y = VariableMedian_Quant()), arrow = arrow(length = unit(0.03, "npc")), color = "orange", size = 0.8) +
      geom_segment(aes(x = -2.9 , y = VariableMedian_Quant(), xend = qnorm(VariableMedian_Quant()), yend = VariableMedian_Quant()), color = "orange", size = 0.8) +
      stat_function(fun = pnorm, color = "orange", size = 1.5, xlim = c(-3,3)) +
      xlim(c(-3,3)) +
      xlab("x") +
      ylab("Fonction de répartition") +
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
  
  VariableIC <- reactive({
    input$ICPanel
  })
  
  output$IC <- renderText({
    
    ICinf <- round(qnorm(as.double((1-VariableIC()/100)/2)), 3)
    ICsup <- round(qnorm(as.double((1+VariableIC()/100)/2)), 3)
    
    quant_txt <- paste0("IC (",round(VariableIC()),"%) = [", ICinf, " , ", ICsup, "]" )
    quant_txt
    
  })
  
  output$ICPlot <- renderPlot({
    nb <- 2.5e2
    df  <- as.data.frame(rnorm(nb))
    tmp <- runif(1e3, -3, 3)
    df2 <- data.frame(x = tmp, y = dnorm(tmp), z = pnorm(tmp))
    IC <- 1-VariableIC()/100
    names(df) <- "x"
    g  <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),      size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = ..density.., color = "Data"),                alpha = 0.2, show.legend = F, binwidth = 0.25, center = -1) +
      geom_area(data = df2[df2$x<qnorm(1-IC/2) & df2$x>qnorm(IC/2),], aes(x = x, y = y), fill = "orange", alpha = 0.25, size = 1.5) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      annotate(geom  = "text",
               x     = 0,
               y     = 0.2,
               label = paste0(VariableIC(), " %"), 
               color = "orangered4",
               size  = 12) +
      xlab("x") +
      ylab("Densité de probabilité") +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      xlim(c(-3,3)) +
      ylim(c(0,0.5)) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    h <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"), size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = cumsum(..count..)/nb, color = "none"), alpha = 0.2, show.legend = F, binwidth = 0.25, center = -1) +
      geom_segment(aes(x = qnorm(IC/2) , yend = 0, xend = qnorm(IC/2), y = IC/2), arrow = arrow(length = unit(0.03, "npc")), color = "orange", size = 0.8) +
      geom_segment(aes(x = qnorm(1-IC/2) , yend = 0, xend = qnorm(1-IC/2), y = 1-IC/2), arrow = arrow(length = unit(0.03, "npc")), color = "orange", size = 0.8) +
      geom_segment(aes(x = -2.9 , y = IC/2, xend = qnorm(IC/2), yend = IC/2), color = "orange", size = 0.8) +
      geom_segment(aes(x = -2.9 , y = 1-IC/2, xend = qnorm(1-IC/2), yend = 1-IC/2), color = "orange", size = 0.8) +
      stat_function(fun = pnorm, color = "orange", size = 1.5, xlim = c(-3,3)) +
      annotate(geom  = "text",
               x     = -2,
               y     = 0.5,
               label = paste0(VariableIC(), " %"), 
               color = "orangered4",
               size  = 12) +
      xlim(c(-3,3)) +
      xlab("x") +
      ylab("Fonction de répartition") +
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
  
  VariableVar <- reactive({
    input$VarPanel
  })
  
  VariableSD <- reactive({
    input$SDPlotPanel
  })
  
  output$Var <- renderText({
    
    quant_txt <- paste0("La variance vaut: ", VariableVar()^2)
    quant_txt
    
  })
  
  output$sd <- renderText({
    
    quant_txt <- paste0("L'écart-type vaut: ", VariableVar())
    quant_txt
    
  })
  
  output$VarPlot <- renderPlot({
    nb <- 1.25e2
    df  <- as.data.frame(rnorm(nb, mean = 0, sd = VariableVar()))
    tmp <- runif(1e3, -3*VariableVar(), 3*VariableVar())
    df2 <- data.frame(x = tmp, y = dnorm(tmp, mean = 0, sd = VariableVar()), z = pnorm(tmp, mean = 0, sd = VariableVar()))
    names(df) <- "x"
    g  <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),      size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = ..density.., color = "Data"),                alpha = 0.2, show.legend = F, bins = 30, center = -1) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      geom_area(data = df2[df2$x<VariableSD()*VariableVar() & df2$x>-VariableSD()*VariableVar(),], aes(x = x, y = y), fill = "orange", alpha = 0.25, size = 1.5) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      xlab("x") +
      ylab("Densité de probabilité") +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      xlim(c(-30, 30)) +
      ylim(c(0,1.5/sqrt(2*pi*VariableVar()^2))) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    h <- ggplot(df) +
      geom_point(    aes(x = x,       y = 0,     color = "Data"),      size = 5, alpha = 0.5, shape = 3) +
      geom_histogram(aes(x = x, y = ..density.., color = "Data"),                alpha = 0.2, show.legend = F, bins = 30, center = -1) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      geom_area(data = df2[df2$x<VariableSD()*VariableVar() & df2$x>-VariableSD()*VariableVar(),], aes(x = x, y = y), fill = "orange", alpha = 0.25, size = 1.5) +
      geom_line(data = df2, aes(x = x, y = y), color = "blue", size = 1.5) +
      annotate(geom  = "text",
               x     = 0,
               y     = 1.5/sqrt(2*pi*VariableVar()^2),
               label = "(zoom sur l'IC à 99%)", 
               color = "black",
               size  = 10) +
      annotate(geom  = "text",
               x     = 0,
               y     = .5/sqrt(2*pi*VariableVar()^2),
               label = paste0(round(100*pnorm(VariableSD()*VariableVar(), mean = 0, sd = VariableVar())-100*pnorm(-VariableSD()*VariableVar(), mean = 0, sd = VariableVar()), 1), " %"), 
               color = "orangered4",
               size  = 12) +
      xlab("x") +
      ylab("Densité de probabilité") +
      scale_color_manual(name   = "",
                         values = c("Data"      = "black",
                                    "Moyenne"   = "red",
                                    "Espérance" = "blue")) +
      ylim(c(0,1.5/sqrt(2*pi*VariableVar()^2))) +
      theme_minimal() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    ggarrange(g, h, legend = F, align = "hv")
  })
  
  VariableMargNsample <- reactive({
    input$MarginalNsamplePanel
  })
  
  VariableMargX <- reactive({
    input$MarginalXPanel
  })
  
  VariableMargY <- reactive({
    input$MarginalYPanel
  })
  
  output$MarginalPlot <- renderPlot({
    # lim        <- 5*max(VariableMargX(), VariableMargY()) 
    lim        <- 5*2 
    nsample    <- VariableMargNsample()
    Sigma      <- diag(c(VariableMargX()^2, VariableMargY()^2))
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    names(df3) <- c("X", "Y")
    i <- ggplot(df3) +
      geom_point(aes(x = X, y = Y), size = 5, alpha = 0.5) +
      geom_density_2d(aes(x = X, y = Y), color = "blue") +
      xlim(c(-lim, lim)) +
      ylim(c(-lim, lim)) +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableMargX(), VariableMargY()), 5*max(VariableMargX(), VariableMargY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableMargX(), VariableMargY()), 5*max(VariableMargX(), VariableMargY()), 1.25), alpha = 0.75))
    j <- ggplot(df3) +
      geom_point(aes(x = X, y = Y), size = 0.1, alpha = 0.5) +
      geom_density_2d_filled(aes(x = X, y = Y), size = 1, show.legend = F) +
      xlim(c(-lim, lim)) +
      ylim(c(-lim, lim)) +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableMargX(), VariableMargY()), 5*max(VariableMargX(), VariableMargY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableMargX(), VariableMargY()), 5*max(VariableMargX(), VariableMargY()), 1.25), alpha = 0.75))
    jm <- ggMarginal(j, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableMargX(), VariableMargY()), 5*max(VariableMargX(), VariableMargY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableMargX(), VariableMargY()), 5*max(VariableMargX(), VariableMargY()), 1.25), alpha = 0.75))
    
    ggarrange(im, jm, align = "hv", ncol = 2)
  })
  
  VariableCorAngle <- reactive({
    input$CorAnglePanel*2*pi/360
  })
  
  VariableCorX <- reactive({
    input$CorXPanel
  })
  
  VariableCorY <- reactive({
    input$CorYPanel
  })
  
  output$Cov <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    theta <- VariableCorAngle()
    rot <- matrix(data = c(cos(theta), -sin(theta),
                           sin(theta),  cos(theta)), ncol = 2)
    Sigma <- t(rot) %*% Sigma %*% rot
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    cov_txt <- paste0("La covariance vaut: ", round(cov(df3)[1,2],3), " (théorie: ", round(Sigma[1,2],3), ")")
    cov_txt
    
  })
  
  output$VarX <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    theta <- VariableCorAngle()
    rot <- matrix(data = c(cos(theta), -sin(theta),
                           sin(theta),  cos(theta)), ncol = 2)
    Sigma <- t(rot) %*% Sigma %*% rot
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    VarX_txt <- paste0("La variance en X vaut : ", round(var(df3[,1]),3), " (théorie: ", round(Sigma[1,1],3), ")")
    VarX_txt
    
  })
  
  output$VarY <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    theta <- VariableCorAngle()
    rot <- matrix(data = c(cos(theta), -sin(theta),
                           sin(theta),  cos(theta)), ncol = 2)
    Sigma <- t(rot) %*% Sigma %*% rot
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    VarX_txt <- paste0("La variance en Y vaut : ", round(var(df3[,2]),3), " (théorie: ", round(Sigma[2,2],3), ")")
    VarX_txt
    
  })
  
  output$Cor <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    theta <- VariableCorAngle()
    rot <- matrix(data = c(cos(theta), -sin(theta),
                           sin(theta),  cos(theta)), ncol = 2)
    Sigma <- t(rot) %*% Sigma %*% rot
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    cor_txt <- paste0("Le coefficient de corrélation vaut: ", round(cor(df3)[1,2],3), " (théorie: ", round(Sigma[1,2]/sqrt(Sigma[1,1]*Sigma[2,2]),3), ")")
    cor_txt
    
  })
  
  output$CorPlot <- renderPlot({
    # lim        <- 5*max(VariableMargX(), VariableMargY()) 
    lim        <- 5*2 
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    theta <- VariableCorAngle()
    rot <- matrix(data = c(cos(theta), -sin(theta),
                           sin(theta),  cos(theta)), ncol = 2)
    Sigma <- t(rot) %*% Sigma %*% rot
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    names(df3) <- c("X", "Y")
    i <- ggplot(df3) +
      geom_point(aes(x = X, y = Y), size = 5, alpha = 0.5) +
      geom_density_2d(aes(x = X, y = Y), color = "blue") +
      xlim(c(-lim, lim)) +
      ylim(c(-lim, lim)) +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75))
    j <- ggplot(df3) +
      geom_point(aes(x = X, y = Y), size = 0.1, alpha = 0.5) +
      geom_density_2d_filled(aes(x = X, y = Y), size = 1, show.legend = F) +
      xlim(c(-lim, lim)) +
      ylim(c(-lim, lim)) +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75))
    jm <- ggMarginal(j, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75))
    
    ggarrange(im, jm, align = "hv", ncol = 2)
  })
  
  VariableCorValue <- reactive({
    input$CorValuePanel
  })
  
  VariableCorX2 <- reactive({
    input$CorX2Panel
  })
  
  VariableCorY2 <- reactive({
    input$CorY2Panel
  })
  
  output$Cov2 <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    Sigma[1,2] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    Sigma[2,1] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    cov_txt <- paste0("La covariance vaut: ", round(cov(df3)[1,2],3), " (théorie: ", round(Sigma[1,2],3), ")")
    cov_txt
    
  })
  
  output$VarX2 <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    Sigma[1,2] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    Sigma[2,1] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    VarX_txt <- paste0("La variance en X vaut : ", round(var(df3[,1]),3), " (théorie: ", round(Sigma[1,1],3), ")")
    VarX_txt
    
  })
  
  output$VarY2 <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    Sigma[1,2] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    Sigma[2,1] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    VarX_txt <- paste0("La variance en Y vaut : ", round(var(df3[,2]),3), " (théorie: ", round(Sigma[2,2],3), ")")
    VarX_txt
    
  })
  
  output$Cor2 <- renderText({
    
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX()^2, VariableCorY()^2))
    Sigma[1,2] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    Sigma[2,1] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    
    cor_txt <- paste0("Le coefficient de corrélation vaut: ", round(cor(df3)[1,2],3), " (théorie: ", round(Sigma[1,2]/sqrt(Sigma[1,1]*Sigma[2,2]),3), ")")
    cor_txt
    
  })
  
  output$Cor2Plot <- renderPlot({
    # lim        <- 5*max(VariableMargX(), VariableMargY()) 
    lim        <- 5*2 
    nsample    <- 1e3
    Sigma      <- diag(c(VariableCorX2()^2, VariableCorY2()^2))
    Sigma[1,2] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    Sigma[2,1] <- VariableCorValue()*sqrt(VariableCorX2()^2* VariableCorY2()^2)
    df3        <- as.data.frame(rmvnorm(nsample, mean = c(0, 0), sigma = Sigma))
    names(df3) <- c("X", "Y")
    i <- ggplot(df3) +
      geom_point(aes(x = X, y = Y), size = 5, alpha = 0.5) +
      geom_density_2d(aes(x = X, y = Y), color = "blue") +
      xlim(c(-lim, lim)) +
      ylim(c(-lim, lim)) +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75))
    j <- ggplot(df3) +
      geom_point(aes(x = X, y = Y), size = 0.1, alpha = 0.5) +
      geom_density_2d_filled(aes(x = X, y = Y), size = 1, show.legend = F) +
      xlim(c(-lim, lim)) +
      ylim(c(-lim, lim)) +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75))
    jm <- ggMarginal(j, type = "histogram", color = "darkgrey", fill = "black", 
                     xparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75), 
                     yparams = list(breaks = seq(-5*max(VariableCorX(), VariableCorY()), 5*max(VariableCorX(), VariableCorY()), 1.25), alpha = 0.75))
    
    ggarrange(im, jm, align = "hv", ncol = 2)
  })
  
  Variable3DNpts <- reactive({
    input$Nbpts3DPanel
  })
  
  Variable3DTheta <- reactive({
    input$Theta3DPanel
  })
  
  Variable3DPhi <- reactive({
    input$Phi3DPanel
  })
  
  output$D3Plot <- renderPlot({
    
    nsample <- Variable3DNpts()
    
    Sigma <- matrix(data = c( 1, -1, 0,
                              -1,  2, 0,
                              0,  0, 3), ncol = 3, byrow = T)
    df3d <- as.data.frame(rmvnorm(nsample, mean = rep(0, 3), sigma = Sigma))
    names(df3d) <- paste0("X", 1:3)
    df3d$`Distance from origin` <- sqrt(df3d$X1^2+df3d$X2^2+df3d$X3^2)
    lim <- 5
    scatter3D_fancy(x = df3d$X1, y = df3d$X2, z = df3d$X3,
                    posx = -lim, posy = lim, posz = -lim,
                    colvar = df3d$`Distance from origin`, 
                    theta = Variable3DTheta(), phi = Variable3DPhi(),
                    main = "Distribution gaussienne 3D",  clab = c("Distance from", "origin"),
                    xlim = c(-lim, lim),
                    ylim = c(-lim, lim),
                    zlim = c(-lim, lim))
  })
  
  VariableTCLmean <- reactive({
    input$TCLmeanPanel
  })
  
  VariableTCLsd <- reactive({
    input$TCLsdPanel
  })
  
  VariableTCLNsample <- reactive({
    input$TCLNsample
  })
  
  VariableTCLNhist <- reactive({
    input$TCLNhist
  })
  
  VariableTCLDist <- reactive({
    switch(input$dist,
           norm  = "norm",
#           unif  = "unif",
           lnorm = "lnorm",
           gamma = "gamma",
#           beta  = "beta",
           rnorm)
  })
  
  output$TCL <- renderPlot({
    
    dist <- VariableTCLDist()
    
    nsample <- VariableTCLNsample()
    nhist   <- VariableTCLNhist()
    
    ns <- data.frame(nsample = rep(nsample, nhist))
    
    if (dist == "norm") {
      sigma <- VariableTCLsd()
      mu    <- VariableTCLmean()
      data  <- data.frame(distrib = rnorm(nsample, mean = mu, sd = sigma))
    }
    
    # if (dist == "unif") {
    #   max   <- 3*VariableTCLsd()^2/VariableTCLmean()+VariableTCLmean()
    #   min   <- 2*VariableTCLmean()-max
    #   data  <- data.frame(distrib = runif(nsample, min = min, max = max))
    # }
    
    if (dist == "lnorm") {
      sigma <- sqrt(log((VariableTCLsd()/VariableTCLmean())^2+1))
      mu    <- log(VariableTCLmean()) - VariableTCLsd()^2/2
      data  <- data.frame(distrib = rlnorm(nsample, meanlog = mu, sdlog = sigma))
    }
    
    if (dist == "gamma") {
      shape <- VariableTCLmean()^2/VariableTCLsd()^2
      scale <- VariableTCLsd()^2/VariableTCLmean()
      data  <- data.frame(distrib = rgamma(nsample, shape = shape, scale = scale))
    }
    
    # if (dist == "beta") {
    #   shape1 <- (1-VariableTCLmean())*VariableTCLmean()^2/VariableTCLsd()^2-VariableTCLmean()
    #   shape2 <- shape1*(1-VariableTCLmean())/VariableTCLmean()
    #   data  <- data.frame(distrib = rbeta(nsample, shape1 = shape1, shape2 = shape2))
    # }
    
    df <- data.frame((apply(ns, 
                            FUN = TCL, 
                            MARGIN = 1, 
                            E = VariableTCLmean(), 
                            SD = VariableTCLsd(), 
                            fun = dist) - VariableTCLmean())/(VariableTCLsd()/sqrt(VariableTCLNsample())))
    names(df) <- "Xred"
    
    g <- ggplot(data) +
      geom_histogram(aes(x = distrib, y = ..density..), alpha = 0.2, show.legend = F, bins = 30, center = -1) +
      xlab("Variable aléatoire (un échantillon)") +
      ylab("Densité de probabilité") +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    h <- ggplot(df) +
      geom_histogram(aes(x = Xred, y = ..density..), alpha = 0.2, show.legend = F, bins = 30, center = -1) +
      stat_function(fun = dnorm, color = "blue", size = 1.2) +
      xlab("Variable aléatoire réduite") +
      ylab("Densité de probabilité") +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))
    
    i <- ggplot(df) +
      stat_ecdf(aes(x = Xred), size = 1.2, show.legend = F, bins = 30, center = -1) +
      stat_function(fun = pnorm, color = "blue", size = 1.2) +
      xlab("Variable aléatoire réduite") +
      ylab("Fonction de répartition") +
      theme_bw() +
      theme(axis.text.x  = element_text(size = 16),
            axis.text.y  = element_text(size = 16),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.text  = element_text(size = 16))   
    
    ggarrange(g, h, i, ncol = 3, align = "hv")
    
  })
  
})
