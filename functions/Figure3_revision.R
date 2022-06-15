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

## Load packages -----
#install.packages("tidyverse")
#install.packages("ggplot2")
#
library("tidyverse")
library("ggplot2")
library("paletteer")
knitr::opts_knit$set(root.dir = normalizePath(".."))

# Load supplementary table 10 from manuscript -----
plot <- read.csv("tables/final/mediation.latents.single.residualized.csv")
mycolors <- paletteer::paletteer_c("grDevices::Blue-Red",16) 
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

# Create column indicating which cluster -----
plot  <-
  plot %>% mutate(
    cluster_type =
      case_when(
        grepl("latent_pc", mediator) ~ "PCA",
        grepl("latent_cc", mediator) ~ "CCA",
        TRUE ~ "PCA"
      )
  )


# ### For tidy y-axis, remove underscores on mediator column -----
plot$mediator <- stringr::str_replace_all(plot$mediator,
                                          c("_" = " ",
                                            "pc" = "PC",
                                            "cc" = "CC" 
                                            ))

# Plot only direct & indirect effect -----
plot <- plot %>% filter(label == "ie" | label == "de")

# Rename some label column metrics for clarity
# (e.g. de -> direct effect and ie -> indirect effect) -----
plot$label <- stringr::str_replace_all(plot$label,
                                         c("de" = "direct effect",
                                           "ie" = "indirect effect"))


## new order, group by type of dimensionality reduction method?
# Manual reorder note that setting x = reorder(mediator,-est.std) works just as well -----
plot  <- plot  %>%
  mutate(mediator = fct_relevel(mediator, 
                                ## BRAIN LATENTS
                                "brain latent CC1",
                                "brain latent gMD",
                                "brain latent gFA",
                                "brain latent atrophy",
                                "brain latent PC2",
                                "brain latent PC3",
                                "brain latent CC3",
                                "brain latent PC1",
                                "brain latent grey",
                                "brain latent CC2",

                                ## BRAIN LATENTS
                                "heart latent CC1",
                                "heart latent PC1",
                                "heart latent PC3",
                                "heart latent CC3",
                                "heart latent PC2",
                                "heart latent CC2",
                                
                                
  ))


#### GEOM POINT PLOT -----
#### note: if you want the results displayed horizontally, #coord_flip, and include:
####        theme(axis.text.x = element_text(angle = 45, hjust = 1))
####        We have manually assigned an order to these point-estimates, but
####        x = reorder(mediator,-est.std) works too

x <- ggplot(
  plot,
  aes(
    x = mediator,
    y = est.std,
    colour = mediator,
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
  ylab("Indirect effect") +
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

###### 


