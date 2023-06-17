## ---------------------------
##
## Script Purpose: Figure 3 demonstrating single mediation modelling 
##                 Geom_point plot comparing the estimated indirect effects of the mediators
##                 
##
##                 (1) Assign new column describing which latents are in which group (Heart v Brain)
##                 (2) Rename columns for clarity on plot
##                 (3) Assign point estimate colours for cardiac vs brain latent factors
##                 (4) Ensure point estimates are ordered in terms of effect size &
##                     grouped in terms of whether they are cardiac or brain latents
##                 (5) Set shape of points to indicate significance 
##
##
##

library("tidyverse")
library("ggplot2")
library("paletteer")

sort.by.ies = function(dataframe) {
  ies = dataframe[grepl('ie', dataframe$label),]
  ies.brain = ies[grep('brain', ies$mediator),]
  ies.heart = ies[grep('heart', ies$mediator),]
  sorting = c(ies.brain[order(ies.brain$est.std, decreasing=TRUE),]$mediator, 
              ies.heart[order(ies.heart$est.std, decreasing=TRUE),]$mediator)
  return(sorting)
}

sort.by.brain = function(dataframe) {
  ies = dataframe[grepl('ie2', dataframe$label),]
  ies.brain = ies[grep('brain', ies$mediator),]
  sorting = rev(ies.brain$mediator) #ies.brain[order(ies.brain$mediator, decreasing=TRUE),]$mediator
  return(sorting)
}

direct.indirect.plot = function(plot, sorting, ylims=c(-0.1,0.05)) {
  mycolors <- (values = c( # BRAIN IDP COLOUR ASSIGNMENTS  -----
                           
                           "#023FA5FF", # MO
                           "#455AA7FF", # ICVF
                           "#3C5488B2", # ISOVF
                           "#6673B1FF", # l2
                           "#828BBBFF", # MD
                           "#9BA1C5FF", # OD
                           "#8796C2FF", # l1
                           "#95AFD0FF", # FA
                           "#A7C6DDFF", # L3
                           "#7695f0",   # WMH (note consider revising- too blue?)
                           "#7695f0",   # Volume (note consider revising- too blue?)
                           
                           # CARDIAC MRI IDP COLOUR ASSIGNMENTS  -----
                           
                           "#DED3D5FF", # Global variance
                           "#C8969FFF", # Local uniformity
                           "#BD7B88FF", # Shape
                           "#AF5E70FF", # Size
                           "#A03C56FF", # Local Dimness
                           "#8E063BFF"  # Global intensity
  ))
  # Create column indicating whether heart or brain latent factor -----
  plot  <-
    plot %>% mutate(
      type =
        case_when(
          grepl("heart", mediator) ~ "Heart",
          grepl("brain", mediator) ~ "Brain",
          TRUE ~ "All"
        )
    )
  plot <- plot %>% filter(label == "ie" | label == "de")
  
  # Rename some label column metrics for clarity
  # (e.g. de -> direct effect and ie -> indirect effect) -----
  plot$label <- stringr::str_replace_all(plot$label,
                                         c("de" = "direct effect",
                                           "ie" = "indirect effect"))
  
  
  ### For tidy y-axis, remove underscores on mediator column -----
  plot$mediator <- stringr::str_replace_all(plot$mediator,
                                            c("_" = " ",
                                              "pc" = "PC",
                                              "cc" = "CC"
                                            ))
  sorting = stringr::str_replace_all(sorting,
                                     c("_" = " ",
                                       "pc" = "PC",
                                       "cc" = "CC"
                                     ))
  
  ## new order, group by type of dimensionality reduction method?
  # Manual reorder note that setting x = reorder(mediator,-est.std) works just as well -----
  plot  <- plot  %>%
    mutate(mediator = fct_relevel(mediator, sorting))

  
  #### GEOM POINT PLOT -----
  #### note: if you want the results displayed horizontally, #coord_flip, and include:
  ####        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ####        We have manually assigned an order to these point-estimates, but
  ####        x = reorder(mediator,-est.std) works too
  
  x <- ggplot(
    plot,
    aes(
      x = mediator ,
      y = est.std,
      colour = mediator ,
      group = type,
      shape = significant
    )
  ) +
    
    geom_point(position = position_dodge(width = 0.9),
               size = 2.2,
               stroke = 0.9) +
    
    geom_errorbar(
      aes(ymin = ci.lower,
          ymax = ci.upper),
      position = position_dodge(0.9),
      width = 0.3,
      colour = "darkgrey",
      # colour = "#414141",
      alpha = 0.9,
      size = 0.8
    ) +
    
    geom_hline(yintercept = 0, linetype = "dotted") +
    
    coord_flip() +
    theme_bw() +
    xlab("") +
    ylab("") +
    ylim(ylims[1],ylims[2]) +
    facet_wrap( ~ label)
  
  x <- x +
    theme(legend.position="none",
          axis.title.x = element_text(
            size = 11,
            face = "bold",
            colour = "black"
          ),
          axis.title.y = element_text(
            size = 11,
            face = "bold",
            colour = "black"
          )
    ) +
    scale_shape_manual(values = c(1,
                                  16)) +
    scale_colour_manual(values = mycolors)
  
  return(x)
}

indirect.plot = function(plot, sorting, ylims=c(-0.1,0.05)) {
  mycolors <- (values = c( # BRAIN IDP COLOUR ASSIGNMENTS  -----
                           
                           "#023FA5FF", # MO
                           "#455AA7FF", # ICVF
                           "#3C5488B2", # ISOVF
                           "#6673B1FF", # l2
                           "#828BBBFF", # MD
                           "#9BA1C5FF", # OD
                           "#8796C2FF", # l1
                           "#95AFD0FF", # FA
                           "#A7C6DDFF", # L3
                           "#7695f0",   # Volume (note consider revising- too blue?)
                           
                           # CARDIAC MRI IDP COLOUR ASSIGNMENTS  -----
                           
                           "#DED3D5FF", # Global variance
                           "#C8969FFF", # Local uniformity
                           "#BD7B88FF", # Shape
                           "#AF5E70FF", # Size
                           "#A03C56FF", # Local Dimness
                           "#8E063BFF"  # Global intensity
  ))
  # Create column indicating whether heart or brain latent factor -----
  plot  <-
    plot %>% mutate(
      type =
        case_when(
          grepl("heart", mediator) ~ "Heart",
          grepl("brain", mediator) ~ "Brain",
          TRUE ~ "All"
        )
    )
  plot <- plot %>% filter(grepl('ie',label))
  
  # Rename some label column metrics for clarity
  # (e.g. de -> direct effect and ie -> indirect effect) -----
  plot$label <- rep('indirect effect', length(plot$label)) 
  
  
  ### For tidy y-axis, remove underscores on mediator column -----
  plot$mediator <- stringr::str_replace_all(plot$mediator,
                                            c("_" = " ",
                                              "pc" = "PC",
                                              "cc" = "CC"
                                            ))
  sorting = stringr::str_replace_all(sorting,
                                     c("_" = " ",
                                       "pc" = "PC",
                                       "cc" = "CC"
                                     ))
  
  ## new order, group by type of dimensionality reduction method?
  # Manual reorder note that setting x = reorder(mediator,-est.std) works just as well -----
  plot  <- plot  %>%
    mutate(mediator = fct_relevel(mediator, sorting))
  
  
  #### GEOM POINT PLOT -----
  #### note: if you want the results displayed horizontally, #coord_flip, and include:
  ####        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ####        We have manually assigned an order to these point-estimates, but
  ####        x = reorder(mediator,-est.std) works too
  
  x <- ggplot(
    plot,
    aes(
      x = mediator ,
      y = est.std,
      colour = mediator ,
      group = type,
      shape = significant
    )
  ) +
    
    geom_point(position = position_dodge(width = 0.9),
               size = 2.2,
               stroke = 0.9) +
    
    geom_errorbar(
      aes(ymin = ci.lower,
          ymax = ci.upper),
      position = position_dodge(0.9),
      width = 0.3,
      colour = "darkgrey",
      # colour = "#414141",
      alpha = 0.9,
      size = 0.8
    ) +
    
    geom_hline(yintercept = 0, linetype = "dotted") +
    
    coord_flip() +
    theme_bw() +
    xlab("") +
    ylab("") +
    ylim(ylims[1],ylims[2]) +
    facet_grid( ~ label)
  
  x <- x +
    theme(legend.position="none",
          axis.title.x = element_text(
            size = 11,
            face = "bold",
            colour = "black"
          ),
          axis.title.y = element_text(
            size = 11,
            face = "bold",
            colour = "black"
          ),
          aspect.ratio = 5/4
    ) +
    scale_shape_manual(values = c(1,
                                  16)) +
    scale_colour_manual(values = mycolors)
  
  return(x)
}

sequential.plot = function(plot, sorting, ie.num, ylims=c(-0.1,0.05)) {
  mycolors <- (values = c( # BRAIN IDP COLOUR ASSIGNMENTS  -----
                           
                           "#023FA5FF", # MO
                           "#455AA7FF", # ICVF
                           "#3C5488B2", # ISOVF
                           "#6673B1FF", # l2
                           "#828BBBFF", # MD
                           "#9BA1C5FF", # OD
                           "#8796C2FF", # l1
                           "#95AFD0FF", # FA
                           "#A7C6DDFF", # L3
                           "#7695f0",   # Volume (note consider revising- too blue?)
                           
                           # CARDIAC MRI IDP COLOUR ASSIGNMENTS  -----
                           
                           "#DED3D5FF", # Global variance
                           "#C8969FFF", # Local uniformity
                           "#BD7B88FF", # Shape
                           "#AF5E70FF", # Size
                           "#A03C56FF", # Local Dimness
                           "#8E063BFF"  # Global intensity
  ))
  # Create column indicating whether heart or brain latent factor -----
  plot <- plot %>% filter(grepl(paste('ie',ie.num,sep=''),label))
  
  # Rename some label column metrics for clarity
  # (e.g. de -> direct effect and ie -> indirect effect) -----
  plot$label <- rep(paste('indirect effect ',ie.num,sep=''), length(plot$label)) 
  
  
  ### For tidy y-axis, remove underscores on mediator column -----
  plot$mediator <- stringr::str_replace_all(plot$mediator,
                                            c("_" = " ",
                                              "pc" = "PC",
                                              "cc" = "CC"
                                            ))
  sorting = stringr::str_replace_all(sorting,
                                     c("_" = " ",
                                       "pc" = "PC",
                                       "cc" = "CC"
                                     ))
  
  ## new order, group by type of dimensionality reduction method?
  # Manual reorder note that setting x = reorder(mediator,-est.std) works just as well -----
  plot  <- plot  %>%
    mutate(mediator = fct_relevel(mediator, sorting))
  
  
  #### GEOM POINT PLOT -----
  #### note: if you want the results displayed horizontally, #coord_flip, and include:
  ####        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ####        We have manually assigned an order to these point-estimates, but
  ####        x = reorder(mediator,-est.std) works too
  
  x <- ggplot(
    plot,
    aes(
      x = mediator ,
      y = est.std,
      colour = mediator ,
      shape = significant
    )
  ) +
    
    geom_point(position = position_dodge(width = 0.9),
               size = 2.2,
               stroke = 0.9) +
    
    geom_errorbar(
      aes(ymin = ci.lower,
          ymax = ci.upper),
      position = position_dodge(0.9),
      width = 0.3,
      colour = "darkgrey",
      # colour = "#414141",
      alpha = 0.9,
      size = 0.8
    ) +
    
    geom_hline(yintercept = 0, linetype = "dotted") +
    
    coord_flip() +
    theme_bw() +
    xlab("") +
    ylab("") +
    ylim(ylims[1],ylims[2]) +
    facet_grid( ~ label)
  if(!all(plot$significant)) {
    shape.scale = c(1,16)
  } else {
    shape.scale = c(16,1)
  }
  x <- x +
    theme(legend.position="none",
          axis.title.x = element_text(
            size = 11,
            face = "bold",
            colour = "black"
          ),
          axis.title.y = element_text(
            size = 11,
            face = "bold",
            colour = "black"
          ),
          aspect.ratio = 5/4
    ) +
    scale_shape_manual(values = shape.scale) +
    scale_colour_manual(values = mycolors)
  
  return(x)
}
