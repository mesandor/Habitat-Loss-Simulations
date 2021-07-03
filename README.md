# Habitat-Loss-Simulations

# Code and associated data files for the simulations and analyses presented in Sandor et al. (in revision) Extinction of biotic interactions due to habitat loss could accelerate the current biodiversity crisis

# "Scenario 1" is the code used for the area loss simulation for the exponential area loss in an empirical network scenario (scenario 1)
# "Scenario 2" is the code used for the area loss simulation for the exponential area loss in synthetic networks scenario (scenario 2)
# "Scenario 3" is the code used for the area loss simulation for the variable rate of linear area loss in synthetic networks scenario (scenario 3)

# Please note that there are stochastic elements to scenarios 2 and 3 and simply running "Scenario 2" and "Scenario 3" code will not produce identical results to those presented in the paper.

# Data files associated with Scenario 1 code:
# BirdNames.csv
# BirdsGridsJoin.csv.zip: Large file, must be unzipped before using. 
# FullNetworkpresabs.csv: Full bipartite network for frugivorous birds and fruit-producing shrubs and small trees in the Sierra Nevada Mountains, California, USA. A "1" indicates that the given bird species (column) interacts with the given plant species (row). Bird species are listed by their standardized four-letter codes (https://www.birdpop.org/docs/misc/Alpha_codes_eng.pdf). Please species are listed by the first three letters of their genus name combined with the first three letters of their species name (e.g. Amealn is Amelanchier alnifolia). The full list of plant species with their scientific names can be found in Sandor et al. (in revision) Appendix S1, Table S1. The full list of bird species and associated scientific names can be found in Sandor et al. (in revision) Appendix S1, Table S2.
# GridPathForAnalysis.csv
# PlantNames.csv
# PlantsGridsJoin.csv
