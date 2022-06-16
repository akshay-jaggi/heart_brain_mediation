## ---------------------------
##
## Script Purpose: Figures demonstrating microstructural indirect efffect sizes
##                 I.e how DTI microstructure appears to mediate the VRF-CF association
##                 (1) Rename variables from supplementary table to abbreviated form
##                 (2) Create a new column that specifies which hemisphere the tract is assigned to
##                 (3) Create a plot of tract associations (FA, MD) for both hemispheres
##
##
##

## Load packages -----
library("tidyverse")
library("ggplot2")

# Load supplementary table 11 from manuscript -----
brain_IDP_plot <- read.csv("tables/final/mediation.individual.single.brain.residualized.csv")

plot <- brain_IDP_plot

#####
# Rename the columns to the metric values they contain (Messy way) -----
# Replace multiple words in the mediator column
plot$mediator <- stringr::str_replace_all(
  plot$mediator,
  c(
    ### BRAIN VOLUMES
    
    "brain_bio.volume_of_accumbens_.left..0.imaging" = "accumbens_left",
    "brain_bio.volume_of_accumbens_.right..0.imaging" = "accumbens_right",
    "brain_bio.volume_of_amygdala_.left..0.imaging" = "amygdala_left",
    "brain_bio.volume_of_amygdala_.right..0.imaging" = "amygdala_right",
    "brain_bio.volume_of_brain_stem_._4th_ventricle.0.imaging" = "brainstem (4th ventricle)",
    "brain_bio.volume_of_brain._grey.white_matter.0.imaging" = "total brain (grey + white matter)",
    "brain_bio.volume_of_caudate_.left..0.imaging" = "caudate_left",
    "brain_bio.volume_of_caudate_.right..0.imaging" = "caudate_right",
    "brain_bio.volume_of_grey_matter.0.imaging" = "grey matter",
    "brain_bio.volume_of_hippocampus_.left..0.imaging" = "hippocampus_left",
    "brain_bio.volume_of_hippocampus_.right..0.imaging" = "hippocampus_right",
    "brain_bio.volume_of_pallidum_.left..0.imaging" = "pallidum_left",
    "brain_bio.volume_of_pallidum_.right..0.imaging" = "pallidum_right",
    "brain_bio.volume_of_peripheral_cortical_grey_matter.0.imaging" = "cortical grey matter",
    "brain_bio.volume_of_putamen_.left..0.imaging" = "putamen_left",
    "brain_bio.volume_of_thalamus_.left..0.imaging" = "thalamus_left",
    "brain_bio.volume_of_thalamus_.right..0.imaging" = "thalamus_right",
    "brain_bio.volume_of_ventricular_cerebrospinal_fluid.0.imaging" = "ventricular VSF",
    "brain_bio.volume_of_white_matter.0.imaging" = "white matter",
    
    ### DTI (FA)
    "brain_bio.weighted.mean_fa_in_tract_acoustic_radiation_.left..0.imaging" = "acoustic radiation_left",
    "brain_bio.weighted.mean_fa_in_tract_acoustic_radiation_.right..0.imaging" = "acoustic radiation_right",
    "brain_bio.weighted.mean_fa_in_tract_anterior_thalamic_radiation_.left..0.imaging" = "anterior thalamic radiation_left",
    "brain_bio.weighted.mean_fa_in_tract_anterior_thalamic_radiation_.right..0.imaging" = "anterior thalamic radiation_right",
    "brain_bio.weighted.mean_fa_in_tract_cingulate_gyrus_part_of_cingulum_.left..0.imaging" = "cingulate gyrus_left",
    "brain_bio.weighted.mean_fa_in_tract_cingulate_gyrus_part_of_cingulum_.right..0.imaging" = "cingulate gyrus_right",
    "brain_bio.weighted.mean_fa_in_tract_corticospinal_tract_.left..0.imaging" = "corticospinal_left",
    "brain_bio.weighted.mean_fa_in_tract_corticospinal_tract_.right..0.imaging" = "corticospinal_right",
    "brain_bio.weighted.mean_fa_in_tract_forceps_major.0.imaging" = "forceps major",
    "brain_bio.weighted.mean_fa_in_tract_forceps_minor.0.imaging" = "forceps minor",
    "brain_bio.weighted.mean_fa_in_tract_inferior_fronto.occipital_fasciculus_.left..0.imaging" = "occipital fasciculus_left",
    "brain_bio.weighted.mean_fa_in_tract_inferior_fronto.occipital_fasciculus_.right..0.imaging" = "occipital fasciculus_right",
    "brain_bio.weighted.mean_fa_in_tract_inferior_longitudinal_fasciculus_.left..0.imaging" = "inferior longitudinal fasciculus_left",
    "brain_bio.weighted.mean_fa_in_tract_inferior_longitudinal_fasciculus_.right..0.imaging" = "inferior longitudinal fasciculus_right",
    "brain_bio.weighted.mean_fa_in_tract_posterior_thalamic_radiation_.left..0.imaging" = "posterior thalamic radiation_left",
    "brain_bio.weighted.mean_fa_in_tract_posterior_thalamic_radiation_.right..0.imaging" = "posterior thalamic radiation_right",
    "brain_bio.weighted.mean_fa_in_tract_superior_longitudinal_fasciculus_.left..0.imaging" = "superior longitudinal fasciculus_left",
    "brain_bio.weighted.mean_fa_in_tract_superior_longitudinal_fasciculus_.right..0.imaging" = "superior longitudinal fasciculus_right",
    "brain_bio.weighted.mean_fa_in_tract_superior_thalamic_radiation_.left..0.imaging" = "superior thalamic radiation_left",
    "brain_bio.weighted.mean_fa_in_tract_superior_thalamic_radiation_.right..0.imaging" = "superior thalamic radiation_right",
    "brain_bio.weighted.mean_fa_in_tract_uncinate_fasciculus_.left..0.imaging" = "uncinate fasciculus_left",
    "brain_bio.weighted.mean_fa_in_tract_uncinate_fasciculus_.right..0.imaging" = "uncinate fasciculus_right",
    
    ### DTI (MD)
    "brain_bio.weighted.mean_md_in_tract_acoustic_radiation_.left..0.imaging" = "acoustic radiation_left",
    "brain_bio.weighted.mean_md_in_tract_acoustic_radiation_.right..0.imaging" = "acoustic radiation_right",
    "brain_bio.weighted.mean_md_in_tract_anterior_thalamic_radiation_.left..0.imaging" = "anterior thalamic radiation_left",
    "brain_bio.weighted.mean_md_in_tract_anterior_thalamic_radiation_.right..0.imaging" = "anterior thalamic radiation_right",
    "brain_bio.weighted.mean_md_in_tract_cingulate_gyrus_part_of_cingulum_.left..0.imaging" = "cingulate gyrus_left",
    "brain_bio.weighted.mean_md_in_tract_cingulate_gyrus_part_of_cingulum_.right..0.imaging" = "cingulate gyrus_right",
    "brain_bio.weighted.mean_md_in_tract_corticospinal_tract_.left..0.imaging" = "corticospinal_left",
    "brain_bio.weighted.mean_md_in_tract_corticospinal_tract_.right..0.imaging" = "corticospinal_right",
    "brain_bio.weighted.mean_md_in_tract_forceps_major.0.imaging" = "forceps major",
    "brain_bio.weighted.mean_md_in_tract_forceps_minor.0.imaging" = "forceps minor",
    "brain_bio.weighted.mean_md_in_tract_inferior_fronto.occipital_fasciculus_.left..0.imaging" = "occipital fasciculus_left",
    "brain_bio.weighted.mean_md_in_tract_inferior_fronto.occipital_fasciculus_.right..0.imaging" = "occipital fasciculus_right",
    "brain_bio.weighted.mean_md_in_tract_inferior_longitudinal_fasciculus_.left..0.imaging" = "inferior longitudinal fasciculus_left",
    "brain_bio.weighted.mean_md_in_tract_inferior_longitudinal_fasciculus_.right..0.imaging" = "inferior longitudinal fasciculus_right",
    "brain_bio.weighted.mean_md_in_tract_posterior_thalamic_radiation_.left..0.imaging" = "posterior thalamic radiation_left",
    "brain_bio.weighted.mean_md_in_tract_posterior_thalamic_radiation_.right..0.imaging" = "posterior thalamic radiation_right",
    "brain_bio.weighted.mean_md_in_tract_superior_longitudinal_fasciculus_.left..0.imaging" = "superior longitudinal fasciculus_left",
    "brain_bio.weighted.mean_md_in_tract_superior_longitudinal_fasciculus_.right..0.imaging" = "superior longitudinal fasciculus_right",
    "brain_bio.weighted.mean_md_in_tract_superior_thalamic_radiation_.left..0.imaging" = "superior thalamic radiation_left",
    "brain_bio.weighted.mean_md_in_tract_superior_thalamic_radiation_.right..0.imaging" = "superior thalamic radiation_right",
    "brain_bio.weighted.mean_md_in_tract_uncinate_fasciculus_.left..0.imaging" = "uncinate fasciculus_left",
    "brain_bio.weighted.mean_md_in_tract_uncinate_fasciculus_.right..0.imaging" = "uncinate fasciculus_right"
    
  )
)


# Plot indirect effect label only -----
plot <- plot %>% filter(label == "ie")
plot <- plot %>% filter(type == "fa" | type == "md")

# reorder by hemisphere -----
plot  <-
  plot %>% mutate(
    hemisphere =
      case_when(
        grepl("_left", mediator) ~ "Left",
        grepl("_right", mediator) ~ "Right",
        grepl("minor", mediator) ~ "Left",
        grepl("major", mediator) ~ "Right",
        TRUE ~ "Both"
      )
  )

###

# Plot of just hemispheres -----
plot <- plot %>% filter(hemisphere == "Left" |
                          hemisphere == "Right")

### For tidy y-axis, remove underscores on mediator column -----
plot$mediator <- stringr::str_replace_all(plot$mediator,
                                 c(
                                   "_left" = "",
                                   "_right" = "",
                                   "minor" = "",
                                   "major" = ""
                                 ))

# rename FA and MD for clarity -----
plot$type <- stringr::str_replace_all(plot$type,
                                 c("fa" = "FA",
                                   "md" = "MD"))
###
plot$mediator <- as.factor(plot$mediator)

# Set Colors
mycolors <- paletteer::paletteer_c("grDevices::Blues",15)[1:11]


#### GEOM POINT PLOT -----

x <- ggplot(
  plot,
  aes(
    x = reorder(mediator,-est.std),
    y = est.std,
    colour = reorder(mediator,-est.std),
    group = hemisphere,
    shape = hemisphere
  )
) +
  
  geom_point(position = position_dodge(width = 0.9),
             size = 2.8,
             stroke = 0.9) +
  
  geom_errorbar(
    aes(ymin = ci.lower,
        ymax = ci.upper),
    position = position_dodge(0.9),
    width = 0.3,
    colour = "darkgrey",
    alpha = 0.9,
    size = 0.8
  ) +
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  coord_flip() +
  theme_bw() +
  xlab("White matter tract (DTI)") +
  ylab("Indirect effect size") +
  facet_wrap( ~ type)

x = x +
  theme(legend.position="none",
    axis.title.x = element_text(
      size = 11,
      face = "bold",
      colour = "black"
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      colour = "black"
    )
  ) +
  scale_shape_manual(values = c(16,
                                1)) +
  scale_fill_manual(values = mycolors) +
  scale_colour_manual(values = mycolors) 

###### 
# File saved as PDF, 8.41 x 6.10 inches
#  pdf(file = "/hemisphere_plot.pdf",   # The directory you want to save the file in
#    width = 8.41, # The width of the plot in inches
#    height = 6.10)
# dev.off()

### END
