The pipeline for this meta-analysis was developed using targets (helps make complex workflows reproducible). 

# Setup
To begin: install the package renv::
install.packages("renv")
library(renv)
Then run the function:
renv::restore()
This will ensure that all of your packages are running with the same versions used for this analysis at the time it was initially running (which should prevent any issues arising from pkg versions!)

To get this running, install targets. 
install.packages("targets")
library(targets)

To run all targets, use the function:
tar_make()
If you want to run a specific target (e.g. certain model), run:
tar_make(name_of_target)
The name of the target is the first argument in tar_target. 
For instance, 
tar_target(full_df, read_clean_data("data_clean/full_data.csv")),
<full_df> is the target name.
Here you would run
tar_make(full_df)
This will run all dependencies needed beforehand to get this one (in this case there aren't any).

Alternatively, to view a specific target (e.g. certain model), run:
tar_load(name_of_target)

For transparency, the original _targets.R file (without excluding raw data, is shared in this project called: og_targets_file_for_transparency)


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

