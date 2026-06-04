# Folder Description
This repository contains the full reproducible workflow, code, and supplementary materials for the meta-analysis “AI vs Humans: Does AI Face Harsher Moral Judgements? A Meta-Analysis”. 

All extracted and processed meta-analytic data are located in the `data_clean/` folder.

This pipeline contains computationally heavy steps. Running the full pipeline may take several minutes depending on system resources. The targets workflow should, however, allow you to specify certain steps you want to check the output of, or even re-run. 

The pipeline for this meta-analysis was developed using targets (helps make complex workflows reproducible). 

# Setup
To begin: install the package `renv::`

`install.packages("renv")`
`library(renv)`

Then run the function:

`renv::restore()`

This will ensure that all of your packages are running with the same versions used for this analysis at the time it was initially running (which should prevent any issues arising from pkg versions!)

To get this running, install targets. 

`install.packages("targets")`

`library(targets)`


To run all targets, use the function:

`tar_make()`

If you want to run a specific target (e.g. certain model), run:

`tar_make(name_of_target)`

The name of the target is the first argument in `tar_target`. 

For instance, 

`tar_target(full_df, read_clean_data("data_clean/full_data.csv"))`,

<full_df> is the target name.

Here you would run

`tar_make(full_df)`

This will run all dependencies needed beforehand to get this one (in this case there aren't any).


Alternatively, to view a specific target (e.g. certain model), run:

`tar_load(name_of_target)`


For transparency, the original `_targets.R` file (without excluding raw data, is shared in this project called: og_targets_file_for_transparency)


# Moderator Variable Key
harm -> Moral Domain

in_action -> Decision Type 

intent -> Intent

agent_intel -> Implications (or lack thereof) of the AI Agent's intelligence

aiType_a (or aiTypeA) -> Two-Category Operationalisation of AI: AI System and Robot

aiType_b (or aiTypeB) -> Three-Category Operationalisation of AI: AI System and Mechanical Robot and Humanoid Robot

dv_synonym -> Wording of DV

pma (or PMA) -> Perceptions of Moral Agency

pmc (or PMC) -> Perceptions of Moral Capacity 

responsible -> Responsibility Attributions

rq (or RQ) -> Research Quality Moderator

# Additional Files Key
`supplementary_materials_AIvH.pdf` --> the supplementary materials document. This complements the manuscript and includes additional analyses, tables formatted according to APA. 

`og_targets_file_for_transparency.R` --> the _targets.R file used for the full data extraction and analysis process. It is the version used with the authors' data. It is shared here for transparency as it is not possible to actually share authors' data due to data privacy constraints etc.

`PRISMA_checklist_AIvH.pdf` --> this is the PRISMA checklist for this meta-analysis manuscript. It includes information about where to find the information that supports each item of the PRISMA criteria. 


# Citation 
If you use or adapt this pipeline, please cite the associated preprint:

> Hill-Cousins, O; Sweetman, J; Lowe, C (2026). AI vs Humans: Does AI Face Harsher Moral Judgements? A Meta-Analysis. PsyArXiv. 