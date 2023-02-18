## ---------------------------
##
## Script Purpose: Figure demonstrating Brain IDP and CMR IDP indirect efffect sizes
##                 Boxplot demonstrating how different features  mediate the VRF-CF association
##
##                 (1) Combine both brain IDP SEM analyses and CMR IDP mediation results
##                 (2) Rename columns for clarity on plot
##                 (3) Assign boxplot colours for cardiac vs brain IDPs
##                 (4) Ensure boxplots are ordered in terms of effect size &
##                     grouped in terms of whether they are cardiac or brain IDPs
##
##
##

## Load packages -----
#install.packages("tidyverse")
#install.packages("ggplot2")

library("tidyverse")
library("ggplot2")

# Load supplementary table 11 & 12 from manuscript -----
brain_IDP_plot <- read.csv("tables/final/mediation.individual.single.brain.residualized.csv")
CMR_plot <- read.csv("tables/final/mediation.individual.single.heart.residualized.csv")

# Combine mediation analyses -----
Heart_Brain_mediation_plot <- rbind(CMR_plot,
                                    brain_IDP_plot)

Heart_Brain_mediation_plot <- Heart_Brain_mediation_plot[Heart_Brain_mediation_plot$label %in% c('de','ie'),]

# Rename some mediator column metrics for clarity (e.g. fa -> FA and md -> MD) -----
Heart_Brain_mediation_plot$type <- stringr::str_replace_all(Heart_Brain_mediation_plot$type, 
                                                       c("fa" = "FA", 
                                                         "md" = "MD",
                                                         "icvf" = "ICVF",
                                                         "isovf" = "ISOVF",
                                                         "od" = "OD",
                                                         "mo" = "MO",
                                                         "volume" = "Volume"))

# NOTE that grDevices::Blue-Red colour palette works well for this figure 
# Here I have individually assigned hex colours to demonstrate which colour affects which boxplot
mycolors <- paletteer::paletteer_c("grDevices::Blue-Red",17) 

# mycolors <- (values = c( # BRAIN IDP COLOUR ASSIGNMENTS  -----
#                          
#                         "#023FA5FF", # MO
#                         "#455AA7FF", # ICVF
#                         "#3C5488B2", # ISOVF
#                         "#6673B1FF", # l2
#                         "#828BBBFF", # MD
#                         "#9BA1C5FF", # OD
#                         "#8796C2FF", # l1
#                         "#95AFD0FF", # FA
#                         "#A7C6DDFF", # L3
#                         "#7695f0",   # Volume (note consider revising- too blue?)
#                         
#                         # CARDIAC MRI IDP COLOUR ASSIGNMENTS  -----
#                         
#                         "#DED3D5FF", # Global variance
#                         "#C8969FFF", # Local uniformity
#                         "#BD7B88FF", # Shape
#                         "#AF5E70FF", # Size
#                         "#A03C56FF", # Local Dimness
#                         "#8E063BFF"  # Global intensity
# ))

# Manual reorder note that setting x = reorder(mediator,-est.std) works just as well -----
Heart_Brain_mediation_plot  <- Heart_Brain_mediation_plot  %>%
  mutate(type = fct_relevel(type, 
                                "MO",
                                "ICVF",
                                "ISOVF",
                                "l2",
                                "MD",
                                "OD",
                                "l1",
                                "FA",
                                "l3",
                                "Volume",
                                "Global Variance",
                                "Local Uniformity",
                                "Shape",
                                "Size",
                                "Traditional",
                                "Local Dimness",
                                "Global Intensity"))

# Rename some label column metrics for clarity 
# (e.g. de -> direct effect and ie -> indirect effect) -----
Heart_Brain_mediation_plot$label <- stringr::str_replace_all(Heart_Brain_mediation_plot$label, 
                                                      c("de" = "direct effect", 
                                                        "ie" = "indirect effect"))

Heart_Brain_mediation_plot$mediator <- as.factor(Heart_Brain_mediation_plot$mediator)

#### Note consider revising still:
#### (1) gghighlight L thalamic radiation volume

# Boxplot -----
pd = position_dodge(width = 0.5)

heart_brain_plot_obj = Heart_Brain_mediation_plot %>%
  ggplot(aes(x = est.std, y = type)) +
  stat_boxplot(
    geom = "errorbar",
    position = pd,
    width = 0.3,
   #  colour = "darkgrey",
    alpha = 0.9,
    size = 0.6
  ) +
  geom_boxplot(
    aes(fill = type),
    position = pd,
   #  colour = "darkgrey",
    #  alpha = 0.9,
    size = 0.1,
    outlier.shape = NA
  ) +
  geom_point(aes(colour = type,
                 alpha = 0.6),
             size = 0.9, 
             position = position_jitter()) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors) +
  scale_colour_manual(values = mycolors) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9), 
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11)) +
  # coord_flip ()+
  xlab("") +
  ylab("") +
  facet_wrap(~label)

### END