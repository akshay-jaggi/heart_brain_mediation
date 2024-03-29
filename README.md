# heart_brain_mediation
 Dimensionality Reduction and Mediation Modeling of Heart/Brain Data in UK Biobank

## notebooks
Contains all the notebooks necessary to run all the code to generate the paper tables and figures.
I plan on converting everything to scripts at the end, so we can more easily just 'hit run' and have everything run consecutively. 
Right now, here's the sequencing of how all notebooks can be run to replicate analyses start to finish. I recommend always clearing your Rmd variable space before running each notebook. Each is designed to run start to finish without needing variables from other notebooks. 
Also, yes, I realize that I've been flip-flopping between the . and _ naming conventions. I'm a python fan at heart, but I've been trying to adapt to R. 

### Parallel Prep Steps
Any one of these notebooks can be run independently to clean and prep the respective dataset  
`/prep/cleaning_covs.Rmd`  
`/prep/cleaning_vrfs.Rmd`  
`/prep/cleaning_cog.Rmd`  
`/prep/cleaning_heart.Rmd`  
`/prep/cleaning_brain.Rmd`   

### Data Merging Bottleneck
Always necessary to run this before moving on to downstream steps  
`/prep/data_merging.Rmd`  

### Parallel Dimensionality Reduction Steps
Any one of these notebooks can be run independently to run reduce the respective dataset  
`/dim_red/reduction_vrfs.Rmd`  
`/dim_red/reduction_cog.Rmd`  
`/dim_red/reduction_heart.Rmd`  
`/dim_red/reduction_brain.Rmd`  
`/dim_red/reduction_joint_cca.ipynb` (a Jupyter notebook because the python implementation of CCA is better imo)  
`/dim_red/reduction_joint.Rmd` (for unifying the data generated by the ipynb with everything else)   

### Latent Merging Bottleneck
Always necessary to run this before moving on to the downstream steps   
`/dim_red/merge_latents.Rmd`  

### Analyses
Each notebook is dedicated to specific figure / section of the paper   
`/analysis/descriptive.Rmd`  
`/analysis/pairwise.linear.Rmd`  
`/analysis/pairwise.linear.Rmd`  
`/analysis/pairwise.linear.Rmd`  
`/analysis/latent.single.mediation.Rmd`  
`/analysis/latent.multiple.mediation.Rmd`  
`/analysis/individual.single.mediation.brain.Rmd`  
`/analysis/individual.single.mediation.heart.Rmd`  

## functions
I generated a handful of custom functions for making heatmaps and calculating explained variance. These functions are very reusable, so I ultimately ended up packaging them into their own functions. 

## figures and tables
All the figures and tables for the paper can be found here. In the final publication version of the repo, I'll probably leave these empty, but for now I'm keeping stuff there. 
